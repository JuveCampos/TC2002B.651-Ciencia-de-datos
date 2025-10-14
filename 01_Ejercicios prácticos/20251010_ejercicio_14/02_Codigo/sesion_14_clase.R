
# Librerias o Paquetes ----
library(tidyverse)
library(tidytext)
library(randomForest)
library(tm)
library(caret)
library(readxl)

# 1. Cargue los datos del archivo de excel de la carpeta 01_Datos. Estos datos ya vienen etiquetados como “Ofensivo” y “No ofensivo”, dependiendo de cada mensaje. "

"01_Datos/comentarios_video_videojuegos.xlsx"
comentarios <- read_excel("01_Datos/ofensivo_no_ofensivo.xlsx")

comentarios <- comentarios %>% 
  rename(Es_Ofensivo = final) %>% 
  select(-Qwen, -Llama, -igual) %>% 
  slice(-68) # Quitamos el comentario de los emojis

stop_words <- tibble(palabras = tm::stopwords(kind = "spanish")) 

# # n gramas ----
# comentarios %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   unnest_tokens(output = "palabra", input = textOriginal, token = "ngrams", n = 2) %>% 
#   group_by(palabra) %>% 
#   count() %>% 
#   arrange(-n) %>% 
#   separate(palabra, into = c("p1", "p2")) %>% 
#   anti_join(stop_words, by = c("p1" = "palabras")) %>% 
#   anti_join(stop_words, by = c("p2" = "palabras")) %>% 
#   View()

comentarios_limpio <- comentarios %>% 
  mutate(textOriginal = str_to_lower(textOriginal)) %>% 
  mutate(textOriginal = str_replace_all(textOriginal, 
                                        c("programas sociales" = "programas_sociales", 
                                          "video juegos" = "videojuegos", 
                                          "claudia sheinbaum" = "claudia_sheinbaum", 
                                          "crimen organizado" = "crimen_organizado", 
                                          "gobierno mexicano" = "gobierno", 
                                          "redes sociales" = "redes_sociales"
                                          ))) %>% 
  mutate(id = 1:nrow(.)) %>% 
  unnest_tokens(output = "palabra", input = textOriginal, token = "words") %>% 
  anti_join(stop_words, by = c("palabra" = "palabras"))

comentarios_frecuencia <- comentarios_limpio %>% 
  group_by(id, palabra) %>% 
  count()


matriz_dtm <- comentarios_frecuencia %>% 
  cast_dtm(document = id, 
           term = palabra, 
           value = n)

matriz_dtm %>% as.matrix() %>% as_tibble()

comentarios$Es_Ofensivo <- factor(comentarios$Es_Ofensivo, 
                                  levels = c("Ofensivo", "No ofensivo"))


y <- comentarios$Es_Ofensivo
x <- matriz_dtm %>% as.matrix() %>% as_tibble()

indices_train <- createDataPartition(y, p = 0.7, list = FALSE)

# Crear conjuntos de entrenamiento
X_train <- x[indices_train, ]
y_train <- y[indices_train]

# Crear conjuntos de validación
X_test <- x[-indices_train, ]
y_test <- y[-indices_train]

mdl <- randomForest(x = x,
                    y = y,
                    ntree = 50)

mdl

predict(object =X,  )
predicciones_train <- predict(mdl, X_train)
accuracy_train <- mean(predicciones_train == y_train)
print("\nAccuracy en entrenamiento:", round(accuracy_train * 100, 2), "%\n")
table(Real = y_train, predicho = predicciones_train)


# Predicciones sobre el conjunto de validación
predicciones_test <- predict(mdl, X_test)
accuracy_test <- mean(predicciones_test == y_test)
print("Accuracy en validación:", round(accuracy_test * 100, 2), "%\n")

# Matriz de confusión para el conjunto de validación
cat("\nMatriz de confusión (validación):\n")
confusion_matrix <- table(Real = y_test, Predicho = predicciones_test)
print(confusion_matrix)


# 
# 
# stop_words <- tibble(palabras = tm::stopwords(kind = "spanish"))
# replacements <- c("programas sociales" = "programas_sociales", 
#                   "video juegos"       = "videojuegos", 
#                   "claudia sheinbaum"  = "claudia_sheinbaum", 
#                   "crimen organizado"  = "crimen_organizado", 
#                   "gobierno mexicano"  = "gobierno", 
#                   "redes sociales"     = "redes_sociales")
# vocab <- colnames(x) 
# 
# # --- Helper: preprocesa y devuelve tabla de frecuencias palabra-n ---
# preprocess_counts <- function(text, replacements, stop_words) {
#   tibble(text = text) %>% 
#     mutate(text = str_to_lower(text)) %>% 
#     mutate(text = str_replace_all(text, replacements)) %>% 
#     mutate(id = 1L) %>% 
#     unnest_tokens(output = "palabra", input = text, token = "words") %>% 
#     anti_join(stop_words, by = c("palabra" = "palabras")) %>% 
#     count(palabra, name = "n")
# }
# 
# # --- Helper: pasa de conteos a un renglón 1x|vocab| alineado al vocabulario ---
# counts_to_row <- function(counts_tbl, vocab) {
#   if (nrow(counts_tbl) == 0L) {
#     # Sin términos útiles: devuelve todo 0
#     return(as_tibble_row(setNames(rep(0L, length(vocab)), vocab)))
#   }
#   counts_vec <- setNames(counts_tbl$n, counts_tbl$palabra)
#   row_vec <- setNames(rep(0L, length(vocab)), vocab)
#   common_terms <- intersect(names(counts_vec), vocab)
#   row_vec[common_terms] <- unname(counts_vec[common_terms])
#   as_tibble_row(row_vec)
# }
# 
# # --- Función principal: clasifica un comentario usando el modelo entrenado ---
# classify_comment <- function(comment, mdl, vocab, replacements, stop_words, return_prob = TRUE) {
#   counts_tbl <- preprocess_counts(comment, replacements, stop_words)
#   new_row <- counts_to_row(counts_tbl, vocab)
#   
#   # Predicción
#   pred_label <- predict(mdl, new_row)
#   
#   if (!return_prob) return(pred_label)
#   
#   # Probabilidades (o votos normalizados) si el modelo las soporta
#   # En randomForest con clasificación: type = "prob" devuelve prob por clase
#   pred_prob <- tryCatch(
#     predict(mdl, new_row, type = "prob") %>% as_tibble(),
#     error = function(e) NULL
#   )
#   
#   list(
#     label = as.character(pred_label)[1],
#     prob = pred_prob  # puede ser NULL si el modelo no las expone
#   )
# }
# 
# # --- Ejemplo de uso ---
# ejemplo <- "4T es una estafa !! Ya no voten mas por morena !!"
# res <- classify_comment(
#   comment = ejemplo,
#   mdl = mdl,
#   vocab = vocab,
#   replacements = replacements,
#   stop_words = stop_words,
#   return_prob = TRUE
# )
# 
# 
# 
# res
# # $label -> "Ofensivo" / "No ofensivo"
# # $prob  -> tibble con columnas por clase (si disponible)
# 
# # Si solo quieres la etiqueta:
# classify_comment(ejemplo, mdl, vocab, replacements, stop_words, return_prob = FALSE)
