# Código de la sesión 15. 

# Librerias o Paquetes ----
library(tidyverse) # Manejo de datos
library(tidytext) # Manejo de texto, tokenización
library(randomForest) # Modelo de clasificación
library(tm) # Minería de texto
library(caret) # Partición en bases de entrenamiento y validación
library(readxl) # Lectura de exceles

# CARGA DE DATOS ----
# 1. Cargue los datos del archivo de excel de la carpeta 01_Datos. 
# Estos datos ya vienen etiquetados como “Ofensivo” y “No ofensivo”, 
# dependiendo de cada mensaje."
comentarios <- read_excel("01_Datos/comparacion_etiquetas.xlsx") 

# PREPROCESAMIENTO ----
# Hacemos limpieza de los datos
comentarios <- comentarios %>%
  # Nos quedamos con los comentarios donde Ollama y Qwen coincidieron
  # Esta decisión fue a criterio mío. Ustedes quizá quieran revisar manualmente la clasificación. 
  filter(igual) %>% 
  # Generamos una variable con el etiquetado final, con las etiquetas de la columna Ollama
  mutate(final = Ollama) %>% 
  # Filtramos para quedarnos con los textos con más de seis caracteres
  filter(str_length(textOriginal) >= 6) %>% 
  # Removemos los comentarios que están hechos solo de emojis
  filter(!str_detect(textOriginal, pattern = "^[\\p{Emoji_Presentation}\\p{Emoji}\\p{Extended_Pictographic}]+$")) %>% 
  # Generamos una columna de id de comentario, que va de 1 al total de renglones de la tabla 
  # El punto representa a la tabla
  mutate(id = 1:nrow(.)) %>% 
  # De plano, removemos los emojis. 
  mutate(textOriginal = str_remove_all(textOriginal, pattern = "\\p{Emoji_Presentation}|\\p{Emoji}|\\p{Extended_Pictographic}") %>% str_squish()) %>% 
  # Generamos una columna con el número de letras que tiene cada comentario 
  mutate(longitud = str_length(textOriginal))

# Generamos una tabla de stop_words, pero como en este caso hay palabras en inglés y español, 
# la tabla guarda stopwords en dos idiomas. 
stop_words <- tibble(palabras = c(tm::stopwords(kind = "spanish"),
                                  tm::stopwords(kind = "english"))) 

# Acá hice el análisis de bigramas. 
# Como solo lo necesito para comprimir palabras y términos, lo dejo silenciado
# Para ver que es lo que se hizo acá, descilenciarlo. 
# # n gramas
# comentarios %>%
#   mutate(id = 1:nrow(.)) %>%
#   mutate(textOriginal = str_replace_all(textOriginal, c("á" = "a", 
#                                                         "é" = "e", 
#                                                         "í" = "i", 
#                                                         "ó" = "o", 
#                                                         "ú" = "u",
#                                                         "ñ" = "n",
#                                                         "Á" = "A", 
#                                                         "É" = "E", 
#                                                         "Í" = "I", 
#                                                         "Ó" = "O", 
#                                                         "Ú" = "U"))) %>% 
#   mutate(textOriginal = str_remove_all(textOriginal, pattern = "\\d+") %>% str_squish()) %>% 
#   unnest_tokens(output = "palabra", input = textOriginal, token = "ngrams", n = 2) %>%
#   group_by(palabra) %>%
#   count() %>%
#   arrange(-n) %>%
#   separate(palabra, into = c("p1", "p2")) %>%
#   anti_join(stop_words, by = c("p1" = "palabras")) %>%
#   anti_join(stop_words, by = c("p2" = "palabras")) %>%
#   print(n = 20) %>% 
#   View()

# Limpiamos las palabras y tokenizamos: 
comentarios_limpio <- comentarios %>% 
  mutate(textOriginal = str_replace_all(textOriginal, c("á" = "a", 
                                                        "é" = "e", 
                                                        "í" = "i", 
                                                        "ó" = "o", 
                                                        "ú" = "u",
                                                        "ñ" = "n",
                                                        "Á" = "A", 
                                                        "É" = "E", 
                                                        "Í" = "I", 
                                                        "Ó" = "O", 
                                                        "Ú" = "U"))) %>% 
  # Removemos los números
  mutate(textOriginal = str_remove_all(textOriginal, pattern = "\\d+") %>% str_squish()) %>% 
  # Removemos el signo "-"
  mutate(textOriginal = str_remove_all(textOriginal, pattern = "\\-+") %>% str_squish()) %>% 
  # Cambiamos todo a minúsculas
  mutate(textOriginal = str_to_lower(textOriginal)) %>% 
  # Comprimimos los ngramas
  mutate(textOriginal = str_replace_all(textOriginal, 
                                        c("little america" = "little_america", 
                                          "ee uu" = "estados_unidos", 
                                          "estados unidos" = "estados_unidos", 
                                          "green go" = "gringo", 
                                          "grin gos" = "gringo", 
                                          "mercado inmobiliario" = "mercado_inmobiliario"
                                        ))) %>% 
  # Descomponemos en tokens
  unnest_tokens(output = "palabra", input = textOriginal, token = "words") %>% 
  # Quitamos las stop_words
  anti_join(stop_words, by = c("palabra" = "palabras"))

# Revisamos que comentarios se eliminaron en el proceso de tokenización. 
# Esto pudo haber pasado si el comentario tenía puros emojis :3 o si tenía puras stopwords
faltantes <- which(!(1:1019 %in% unique(comentarios_limpio$id)))

# Obtenemos la frecuencia/documento de cada combinación palabra/documento. 
comentarios_frecuencia <- comentarios_limpio %>% 
  group_by(id, palabra) %>% 
  count() %>% 
  arrange(id, -n)

# MATRÍZ DOCUMENTO-TÉRMINO ----
# Obtenemos la matríz documento-término a partir del conteo previo
matriz_dtm <- comentarios_frecuencia %>% 
  cast_dtm(document = id, 
           term = palabra, 
           value = n, weighting = tm::weightTfIdf)
matriz_dtm # La matríz dtm tiene 1018 documentos (comentarios) con un vocabulario de 4,302 palabras

# Así se ven las primeras columnas y renglones de la matríz dtm: 
matriz_dtm %>% as.matrix() %>% as_tibble()

# PREPARACIÓN DE LOS DATOS PARA EL MODELO ----
# Convertimos la variable de etiquetas en una variable factor (categórica) de dos niveles
comentarios$Es_Ofensivo <- factor(comentarios$final, 
                                  levels = c("Ofensivo", "No ofensivo"))

# Como el modelo (todos los modelos) requieren y~X, donde y es la etiqueta resultante y X la matríz de variables dependientes. 
# Definimos estos dos objetos.
y <- comentarios$Es_Ofensivo[-faltantes] # Quitamos los renglones donde la tokenización eliminó los comentarios
x <- matriz_dtm %>% as.matrix() %>% as_tibble() # Convertimos la matríz dtm en una tibble()

# Hacemos la partición en bases de entrenamiento y en bases de validación. 70% se va a entrenamiento y el resto a validación. 
indices_train <- createDataPartition(y, p = 0.7, list = FALSE) # El objeto indices_train lo que hace es darnos los numeros de los comentarios que se van a la base de entrenamiento. 

# Crear conjuntos de entrenamiento
X_train <- x[indices_train, ] # Quedate con los renglones de la matríz dtm que estén en la lista indices_train
y_train <- y[indices_train] # Quedate con los renglones del vector de etiquetas que estén en la lista indices_train

# Crear conjuntos de validación
X_test <- x[-indices_train, ] # Quedate con los renglones de la matríz dtm que NO estén en la lista indices_train
y_test <- y[-indices_train] # Quedate con los renglones del vector de etiquetas que NO estén en la lista indices_train

# ENTRENAMIENTO DEL MODELO ----
# Se escogió randomForest
# Pudo ser cualquier otro modelo (XGBoost, regresión logística, SVM), pero escogimos este.
# ntree = 50 es un primer saque del hiperparámetro número de árboles, donde 50 arboles votan para decir cuales comentarios son ofensivos y cuales no. 
# Si se le suman árboles, mejora la precisión (con rendimientos decrecientes) pero aumenta el tiempo de procesamiento
mdl <- randomForest(x = X_train,y = y_train,ntree = 50) # Esta linea puede tardar un rato. 

# Vemos los resultados del modelo: 
mdl


# PREDICCIONES DEL MODELO ----
# La función predict() nos devuelve las etiquetas que, dado el modelo entrenado, debería tener cada comentario. 
predicciones_train <- predict(mdl, X_train)

# La precisión del modelo es ver a cuantas etiquetas le atinó, del total de etiquetas predichas. 
accuracy_train <- mean(predicciones_train == y_train)
cat("\nAccuracy en entrenamiento:", round(accuracy_train * 100, 2), "%\n")
table(Real = y_train, predicho = predicciones_train)

# Predicciones sobre el conjunto de validación. 
# Acá se prueba la exactitud del modelo con datos que no ha visto. 
# Esto se hace para ver si el modelo nos va a servir para datos que no ha visto antes. 
predicciones_test <- predict(mdl, X_test)
accuracy_test <- mean(predicciones_test == y_test)
cat("Accuracy en validación:", round(accuracy_test * 100, 2), "%\n")

# Matriz de confusión para el conjunto de validación
cat("\nMatriz de confusión (validación):\n")
confusion_matrix <- table(Real = y_test, Predicho = predicciones_test)
print(confusion_matrix)

# ¿Y si le metemos nuestros propios comentarios? ----

# Función principal: clasifica un comentario proporcionado por nosotros usando el modelo ya entrenado: 
classify_comment <- function(comment, mdl, return_prob = TRUE) {
  
  stop_words <- tibble(palabras = tm::stopwords(kind = "spanish"))
  replacements <- c("little america" = "little_america", 
                    "ee uu" = "estados_unidos", 
                    "estados unidos" = "estados_unidos", 
                    "green go" = "gringo", 
                    "grin gos" = "gringo", 
                    "mercado inmobiliario" = "mercado_inmobiliario")
  vocab <- colnames(x)
  
  # Helper: preprocesa y devuelve tabla de frecuencias palabra-n 
  preprocess_counts <- function(text, replacements, stop_words) {
    tibble(text = text) %>%
      mutate(text = str_to_lower(text)) %>%
      mutate(text = str_replace_all(text, replacements)) %>%
      mutate(id = 1L) %>%
      unnest_tokens(output = "palabra", input = text, token = "words") %>%
      anti_join(stop_words, by = c("palabra" = "palabras")) %>%
      count(palabra, name = "n")
  }
  
  # Helper: pasa de conteos a un renglón 1x|vocab| alineado al vocabulario
  counts_to_row <- function(counts_tbl, vocab) {
    if (nrow(counts_tbl) == 0L) {
      # Sin términos útiles: devuelve todo 0
      return(as_tibble_row(setNames(rep(0L, length(vocab)), vocab)))
    }
    counts_vec <- setNames(counts_tbl$n, counts_tbl$palabra)
    row_vec <- setNames(rep(0L, length(vocab)), vocab)
    common_terms <- intersect(names(counts_vec), vocab)
    row_vec[common_terms] <- unname(counts_vec[common_terms])
    as_tibble_row(row_vec)
  }
  
  counts_tbl <- preprocess_counts(comment, replacements, stop_words)
  new_row <- counts_to_row(counts_tbl, vocab)

  # Predicción
  pred_label <- predict(mdl, new_row)

  if (!return_prob) return(pred_label)

  # Probabilidades (o votos normalizados) si el modelo las soporta
  # En randomForest con clasificación: type = "prob" devuelve prob por clase
  pred_prob <- tryCatch(
    predict(mdl, new_row, type = "prob") %>% as_tibble(),
    error = function(e) NULL
  )

  list(
    label = as.character(pred_label)[1],
    prob = pred_prob  # puede ser NULL si el modelo no las expone
  )
}

# --- Ejemplo de uso ---
# Probamos la función escribiendo un comentario: 
ejemplo <- "Los estadounidenses deberían ser más respetuosos"
res <- classify_comment(comment = ejemplo,mdl = mdl,return_prob = TRUE)

res # Acá se ve la probabilidad de que sea de una o de otra categoría. 
# $label -> "Ofensivo" / "No ofensivo"
# $prob  -> tibble con columnas por clase (si disponible)
