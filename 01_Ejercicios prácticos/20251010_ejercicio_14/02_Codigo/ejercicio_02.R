# Librerias ----
library(tidyverse)
library(tidytext)
library(randomForest)
library(tm)
library(caret)
library(e1071)      # Para Naive Bayes y SVM
library(xgboost)    # Para XGBoost
library(nnet)       # Para regresión logística multinomial

?randomForest::randomForest()

# Carga y limpieza inicial de datos ----
comentarios <- readxl::read_xlsx("01_Datos/ofensivo_no_ofensivo.xlsx") %>% 
  rename(Es_Ofensivo = final)

# comentarios %>% 
#   group_by(Es_Ofensivo) %>% 
#   count() %>% 
#   ungroup() %>% 
#   summarise(pp = n/sum(n))

# Palabras vacías en español
stop_words <- tibble(palabras = tm::stopwords("spanish"))

# Preprocesamiento de texto ----
comentarios_limpio <- comentarios %>% 
  mutate(id = 1:nrow(.)) %>% 
  relocate(id, .before = "textOriginal") %>% 
  rename(clasificacion = Es_Ofensivo) %>% 
  mutate(textOriginal = str_to_lower(textOriginal)) %>% 
  mutate(textOriginal = str_replace_all(textOriginal, c(
    "video juegos" = "videojuegos", 
    "vida eterna" = "vida_eterna", 
    "contenido violento" = "contenido_violento", 
    "programas sociales" = "programas_sociales",
    "narco corridos" = "narcocorridos",
    "narco series" = "narcoseries",
    "juegos violentos" = "juegos_violentos",
    "claudia sheinbaum" = "claudia_sheinbaum",
    "gobierno mexicano" = "gobierno",
    "crimen organizado" = "crimen_organizado"
  ))) 

# Tokenización con UNIGRAMAS ----
comentarios_token_uni <- comentarios_limpio %>% 
  unnest_tokens(output = "palabras", input = textOriginal, token = "words") %>% 
  filter(!(palabras %in% c("amp", "br", "href", "https", "www.youtube.com"))) %>%
  filter(!str_detect(palabras, pattern = "\\d")) %>% 
  anti_join(stop_words) %>% 
  group_by(id, clasificacion, palabras) %>% 
  count() %>% 
  ungroup()

# Tokenización con BIGRAMAS ----
comentarios_token_bi <- comentarios_limpio %>% 
  unnest_tokens(output = "palabras", input = textOriginal, token = "ngrams", n = 2) %>% 
  separate(palabras, into = c("p1", "p2"), sep = "\\s", remove = FALSE) %>% 
  filter(!is.na(p1) & !is.na(p2)) %>% 
  filter(!(p1 %in% stop_words$palabras) & !(p2 %in% stop_words$palabras)) %>% 
  filter(!str_detect(p1, pattern = "\\d") & !str_detect(p2, pattern = "\\d")) %>% 
  select(id, clasificacion, palabras) %>% 
  group_by(id, clasificacion, palabras) %>% 
  count() %>% 
  ungroup()

# Combinar unigramas y bigramas ----
comentarios_token <- bind_rows(comentarios_token_uni, comentarios_token_bi) %>% 
  bind_tf_idf(term = palabras, document = id, n = n) %>% 
  arrange(id, -tf_idf)

cat("Total de términos únicos (unigramas + bigramas):", length(unique(comentarios_token$palabras)), "\n\n")

# Creación de la matriz documento-término (DTM) con menos sparsity ----
matriz_dtm <- comentarios_token %>% 
  cast_dtm(document = id, 
           term = palabras, 
           value = n, 
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = 0.95)  # Cambiado de 0.99 a 0.95 para incluir menos términos

cat("Dimensiones de la matriz DTM:", dim(matriz_dtm), "\n\n")

# Preparación de variables para el modelo ----
X <- as_tibble(as.matrix(matriz_dtm))

# Limpieza de datos: reemplazar NA, NaN, Inf por 0
X <- X %>% 
  mutate(across(everything(), ~replace(., is.na(.) | is.nan(.) | is.infinite(.), 0)))

categorias <- comentarios_token %>% 
  dplyr::select(id, clasificacion) %>% 
  distinct() %>% 
  arrange(id)

y <- factor(categorias$clasificacion, levels = unique(categorias$clasificacion))

# División en conjuntos de entrenamiento y validación ----
set.seed(123)
indices_train <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[indices_train, ]
y_train <- y[indices_train]
X_test <- X[-indices_train, ]
y_test <- y[-indices_train]

cat("=== INFORMACIÓN DEL DATASET ===\n")
cat("Tamaño conjunto de entrenamiento:", nrow(X_train), "\n")
cat("Tamaño conjunto de validación:", nrow(X_test), "\n")
cat("Número de features:", ncol(X_train), "\n")
cat("\nDistribución de clases en entrenamiento:\n")
print(table(y_train))
cat("\nDistribución de clases en validación:\n")
print(table(y_test))

# ============================================================================
# MODELO 1: RANDOM FOREST
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                    RANDOM FOREST                           ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

mdl_rf <- randomForest(x = X_train, y = y_train, ntree = 100)

predicciones_rf_train <- predict(mdl_rf, X_train)
predicciones_rf_test <- predict(mdl_rf, X_test)

accuracy_rf_train <- mean(predicciones_rf_train == y_train)
accuracy_rf_test <- mean(predicciones_rf_test == y_test)

cat("Accuracy en entrenamiento:", round(accuracy_rf_train * 100, 2), "%\n")
cat("Accuracy en validación:", round(accuracy_rf_test * 100, 2), "%\n\n")
print(confusionMatrix(predicciones_rf_test, y_test))

# ============================================================================
# MODELO 2: NAIVE BAYES
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                      NAIVE BAYES                           ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

mdl_nb <- naiveBayes(x = X_train, y = y_train)

predicciones_nb_train <- predict(mdl_nb, X_train)
predicciones_nb_test <- predict(mdl_nb, X_test)

accuracy_nb_train <- mean(predicciones_nb_train == y_train)
accuracy_nb_test <- mean(predicciones_nb_test == y_test)

cat("Accuracy en entrenamiento:", round(accuracy_nb_train * 100, 2), "%\n")
cat("Accuracy en validación:", round(accuracy_nb_test * 100, 2), "%\n\n")
print(confusionMatrix(predicciones_nb_test, y_test))

# ============================================================================
# MODELO 3: SVM (Support Vector Machine)
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                          SVM                               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# Convertir a data.frame y escalar datos para SVM
X_train_scaled <- scale(as.data.frame(X_train))
X_test_scaled <- scale(as.data.frame(X_test), 
                       center = attr(X_train_scaled, "scaled:center"),
                       scale = attr(X_train_scaled, "scaled:scale"))

# Reemplazar cualquier NA que pueda surgir del escalado
X_train_scaled[is.na(X_train_scaled)] <- 0
X_test_scaled[is.na(X_test_scaled)] <- 0

mdl_svm <- svm(x = X_train_scaled, y = y_train, kernel = "radial", cost = 1)

predicciones_svm_train <- predict(mdl_svm, X_train_scaled)
predicciones_svm_test <- predict(mdl_svm, X_test_scaled)

accuracy_svm_train <- mean(predicciones_svm_train == y_train)
accuracy_svm_test <- mean(predicciones_svm_test == y_test)

cat("Accuracy en entrenamiento:", round(accuracy_svm_train * 100, 2), "%\n")
cat("Accuracy en validación:", round(accuracy_svm_test * 100, 2), "%\n\n")
print(confusionMatrix(predicciones_svm_test, y_test))

# ============================================================================
# MODELO 4: XGBOOST
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                        XGBOOST                             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = as.numeric(y_test) - 1)

params <- list(
  objective = "multi:softmax",
  num_class = length(levels(y)),
  eta = 0.3,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

mdl_xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0,
  early_stopping_rounds = 10
)

predicciones_xgb_train <- predict(mdl_xgb, as.matrix(X_train))
predicciones_xgb_test <- predict(mdl_xgb, as.matrix(X_test))

predicciones_xgb_train <- factor(levels(y)[predicciones_xgb_train + 1], levels = levels(y))
predicciones_xgb_test <- factor(levels(y)[predicciones_xgb_test + 1], levels = levels(y))

accuracy_xgb_train <- mean(predicciones_xgb_train == y_train)
accuracy_xgb_test <- mean(predicciones_xgb_test == y_test)

cat("Accuracy en entrenamiento:", round(accuracy_xgb_train * 100, 2), "%\n")
cat("Accuracy en validación:", round(accuracy_xgb_test * 100, 2), "%\n\n")
print(confusionMatrix(predicciones_xgb_test, y_test))

# ============================================================================
# MODELO 5: REGRESIÓN LOGÍSTICA MULTINOMIAL
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║               REGRESIÓN LOGÍSTICA MULTINOMIAL              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

mdl_logit <- multinom(y ~ ., data = cbind(X_train, y = y_train), maxit = 200, trace = FALSE)

predicciones_logit_train <- predict(mdl_logit, X_train)
predicciones_logit_test <- predict(mdl_logit, X_test)

accuracy_logit_train <- mean(predicciones_logit_train == y_train)
accuracy_logit_test <- mean(predicciones_logit_test == y_test)

cat("Accuracy en entrenamiento:", round(accuracy_logit_train * 100, 2), "%\n")
cat("Accuracy en validación:", round(accuracy_logit_test * 100, 2), "%\n\n")
print(confusionMatrix(predicciones_logit_test, y_test))

# ============================================================================
# RESUMEN COMPARATIVO
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                  RESUMEN COMPARATIVO                       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

resumen <- tibble(
  Modelo = c("Random Forest", "Naive Bayes", "SVM", "XGBoost", "Reg. Logística"),
  Accuracy_Train = c(accuracy_rf_train, accuracy_nb_train, accuracy_svm_train, 
                     accuracy_xgb_train, accuracy_logit_train),
  Accuracy_Test = c(accuracy_rf_test, accuracy_nb_test, accuracy_svm_test, 
                    accuracy_xgb_test, accuracy_logit_test)
) %>% 
  mutate(
    Accuracy_Train = round(Accuracy_Train * 100, 2),
    Accuracy_Test = round(Accuracy_Test * 100, 2),
    Diferencia = Accuracy_Train - Accuracy_Test
  ) %>% 
  arrange(desc(Accuracy_Test))

print(resumen)

cat("\n🏆 Mejor modelo (según accuracy en validación):", resumen$Modelo[1], "\n")
cat("   Accuracy en validación:", resumen$Accuracy_Test[1], "%\n")

