
# Librerias ----
library(tidyverse)
library(tidytext)
library(randomForest)
library(tm)
library(caret)  # Para crear la partición de datos

# Carga y limpieza inicial de datos ----
# unique(comentarios$Es_Ofensivo)
comentarios <- readxl::read_xlsx("ofensivo_no_ofensivo_3.xlsx") %>% 
  rename(Es_Ofensivo = final)
  # list.files(pattern = ".csv", full.names = T) %>% 
  # lapply(read_csv) %>% 
  # do.call(rbind, .) %>% 
  # filter(Clasificación != "Apoyo o Sarcasmo")

# openxlsx::write.xlsx(comentarios, "comentarios_videojuego.xlsx")

# Palabras vacías en español
stop_words <-  tibble(palabras = tm::stopwords("spanish"))

# Preprocesamiento de texto ----
comentarios_limpio <- comentarios %>% 
  mutate(id = 1:nrow(.)) %>% 
  relocate(id, .before = "textOriginal") %>% 
  rename(clasificacion = Es_Ofensivo) %>% 
  mutate(textOriginal = str_to_lower(textOriginal)) %>% 
  mutate(textOriginal = str_replace_all(textOriginal, c("video juegos" = "videojuegos", 
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

# Análisis exploratorio de bigramas ----
comentarios_limpio %>% 
  unnest_tokens(output = "ngramas", input = textOriginal, token = "ngrams", n = 2) %>% 
  group_by(ngramas) %>% 
  count() %>% 
  arrange(-n) %>% 
  separate(ngramas, into = c("p1", "p2"), sep  = "\\s") %>% 
  anti_join(stop_words, by = c("p1" = "palabras")) %>% 
  anti_join(stop_words, by = c("p2" = "palabras")) %>% 
  filter(!str_detect(p1, pattern = "\\d+")) %>% 
  filter(!str_detect(p2, pattern = "\\d+")) %>% 
  print(n = 20)

# Tokenización y cálculo de TF-IDF ----
comentarios_token <- comentarios_limpio %>% 
  unnest_tokens(output = "palabras", input = textOriginal, token = "words") %>% 
  filter(!(palabras %in% c("amp", "br", "href", "https", "www.youtube.com"))) %>%
  filter(!str_detect(palabras, pattern = "\\d")) %>% 
  anti_join(stop_words) %>% 
  group_by(id, clasificacion, palabras) %>% 
  count() %>% 
  ungroup() %>% 
  bind_tf_idf(term = palabras, document = id, n = n) %>% 
  arrange(id, -tf_idf) 

# Creación de la matriz documento-término (DTM) ----
matriz_dtm <- comentarios_token %>% 
  cast_dtm(document = id, 
           term = palabras, 
           value = n, 
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = 0.99)

# Preparación de variables para el modelo ----
# Convertir la matriz DTM a tibble para usar en el modelo
X <- as_tibble(as.matrix(matriz_dtm))

# Obtener las categorías correspondientes a cada documento
categorias <- comentarios_token %>% 
  dplyr::select(id, clasificacion) %>% 
  distinct() %>% 
  arrange(id)  # Asegurar que estén en el mismo orden que la matriz

y <- factor(categorias$clasificacion, levels = unique(categorias$clasificacion))

# División en conjuntos de entrenamiento y validación ----
# Establecer semilla para reproducibilidad
set.seed(123)

# Crear índices para la partición (70% entrenamiento, 30% validación)
indices_train <- createDataPartition(y, p = 0.7, list = FALSE)

# Crear conjuntos de entrenamiento
X_train <- X[indices_train, ]
y_train <- y[indices_train]

# Crear conjuntos de validación
X_test <- X[-indices_train, ]
y_test <- y[-indices_train]

# Verificar las dimensiones de los conjuntos
cat("Tamaño conjunto de entrenamiento:", nrow(X_train), "\n")
cat("Tamaño conjunto de validación:", nrow(X_test), "\n")
cat("Distribución de clases en entrenamiento:\n")
print(table(y_train))
cat("Distribución de clases en validación:\n")
print(table(y_test))

# Entrenamiento del modelo Random Forest ----
# Entrenar solo con el conjunto de entrenamiento
mdl <- randomForest(x = X_train,
                    y = y_train, 
                    ntree = 50)

mdl
# Evaluación del modelo ----
# Predicciones sobre el conjunto de entrenamiento
predicciones_train <- predict(mdl, X_train)
accuracy_train <- mean(predicciones_train == y_train)
cat("\nAccuracy en entrenamiento:", round(accuracy_train * 100, 2), "%\n")

# Predicciones sobre el conjunto de validación
predicciones_test <- predict(mdl, X_test)
accuracy_test <- mean(predicciones_test == y_test)
cat("Accuracy en validación:", round(accuracy_test * 100, 2), "%\n")

# Matriz de confusión para el conjunto de validación
cat("\nMatriz de confusión (validación):\n")
confusion_matrix <- table(Real = y_test, Predicho = predicciones_test)
print(confusion_matrix)

# Métricas detalladas por clase
cat("\nMétricas por clase:\n")
print(confusionMatrix(predicciones_test, y_test))

# Importancia de variables
cat("\nTop 20 palabras más importantes:\n")
importancia <- importance(mdl)
importancia_df <- data.frame(
  palabra = rownames(importancia),
  importancia = importancia[, 1]
) %>% 
  arrange(desc(importancia)) %>% 
  head(20)
print(importancia_df)


