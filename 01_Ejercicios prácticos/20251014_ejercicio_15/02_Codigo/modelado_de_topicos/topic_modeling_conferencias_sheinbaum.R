# ============================================================================
# Análisis de Modelado de Tópicos
# Conferencias Estenográficas de la Presidenta Claudia Sheinbaum 2025
# ============================================================================

# Cargar librerías necesarias
library(tidyverse)      # Manipulación de datos
library(tidytext)       # Análisis de texto
library(readxl)         # Leer archivos Excel
library(topicmodels)    # Modelos LDA
library(tm)             # Text mining
library(ggplot2)        # Visualización

# ----------------------------------------------------------------------------
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ----------------------------------------------------------------------------

# Leer el archivo Excel con las conferencias estenográficas
conferencias <- read_excel("01_Datos/estenograficas_sheinbaum.xlsx")

# Explorar la estructura del dataset
str(conferencias)
head(conferencias)

# Verificar dimensiones
dim(conferencias)

# Revisar nombres de columnas
names(conferencias)

# Verificar valores faltantes en columnas clave
sum(is.na(conferencias$titulo))
sum(is.na(conferencias$texto))

# Crear un ID único para cada conferencia si no existe
conferencias <- conferencias %>%
  mutate(doc_id = row_number())

# Revisar longitud de textos
conferencias <- conferencias %>%
  mutate(longitud_texto = nchar(texto))

summary(conferencias$longitud_texto)

# ----------------------------------------------------------------------------
# 2. PREPROCESAMIENTO DE TEXTOS
# ----------------------------------------------------------------------------

# Tokenización: dividir textos en palabras individuales
tokens <- conferencias %>%
  select(doc_id, titulo, texto) %>%
  unnest_tokens(word, texto)

# Cargar stopwords en español
stopwords_es <- stopwords::stopwords("es", source = "stopwords-iso")

# Agregar stopwords personalizadas para contexto de conferencias políticas
# Incluimos palabras comunes en transcripciones estenográficas
stopwords_custom <- c(stopwords_es,
                     "si", "así", "ser", "hacer", "puede", "pueden",
                     "vez", "través", "año", "años", "día", "días",
                     "aquí", "allí", "ahí", "tan", "tal", "también",
                     "bien", "sí", "no", "cómo", "dónde", "cuándo",
                     "presidenta", "presidente", "claudia", "sheinbaum",
                     "buenos", "días", "mañana", "tarde", "gracias",
                     "vamos", "va", "hay", "hoy", "decir", "dijo")

# Limpiar tokens: eliminar stopwords, números y palabras muy cortas
tokens_limpios <- tokens %>%
  # Eliminar números
  filter(!str_detect(word, "^[0-9]+$")) %>%
  # Eliminar palabras muy cortas (menos de 3 caracteres)
  filter(nchar(word) >= 3) %>%
  # Eliminar stopwords
  filter(!word %in% stopwords_custom)

# Contar frecuencias de palabras
frecuencias_palabras <- tokens_limpios %>%
  count(word, sort = TRUE)

# Ver las 25 palabras más frecuentes
head(frecuencias_palabras, 25)

# ----------------------------------------------------------------------------
# 3. CREAR MATRIZ DOCUMENTO-TÉRMINO (DTM)
# ----------------------------------------------------------------------------

# Contar palabras por documento
doc_words <- tokens_limpios %>%
  count(doc_id, word, sort = TRUE)

# Crear DTM usando cast_dtm de tidytext
dtm <- doc_words %>%
  cast_dtm(doc_id, word, n)

# Verificar dimensiones de la matriz
dim(dtm)

# Revisar algunos términos más frecuentes en el corpus
terminos_freq <- colSums(as.matrix(dtm))
terminos_freq_ordenados <- sort(terminos_freq, decreasing = TRUE)
head(terminos_freq_ordenados, 30)

# ----------------------------------------------------------------------------
# 4. MODELADO DE TÓPICOS CON LDA
# ----------------------------------------------------------------------------

# Determinar número de tópicos (k)
# Para 405 conferencias, usaremos k=6 para capturar diversidad temática
k_topics <- 6

# Ajustar modelo LDA
# control: parámetros de estimación
# seed: para reproducibilidad
lda_model <- LDA(dtm,
                 k = k_topics,
                 method = "Gibbs",
                 control = list(seed = 1234,
                               iter = 2000,
                               thin = 100,
                               burnin = 100))

# ----------------------------------------------------------------------------
# 5. EXTRAER Y ANALIZAR TÓPICOS
# ----------------------------------------------------------------------------

# Extraer las palabras más probables por tópico (beta)
topics_beta <- tidy(lda_model, matrix = "beta")

# Top 10 términos por tópico
top_terms <- topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Mostrar resultados
i = 1
for(i in 1:k_topics) {
  cat("\n=== TÓPICO", i, "===\n")
  topic_terms <- top_terms %>%
    filter(topic == i) %>%
    select(term, beta)
  print(topic_terms, n = 10)
}

# Visualización de términos por tópico
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 términos por tópico - Conferencias Presidenta Sheinbaum",
       subtitle = "Análisis de 405 conferencias estenográficas 2025",
       x = "Beta (probabilidad del término en el tópico)",
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11))

# Guardar gráfico
ggsave("03_Graficas/topicos_conferencias_sheinbaum.png",
       width = 14,
       height = 10,
       dpi = 300,
       bg = "white")

# ----------------------------------------------------------------------------
# 6. ANÁLISIS DE DOCUMENTOS POR TÓPICO
# ----------------------------------------------------------------------------

# Extraer distribución de tópicos por documento (gamma)
docs_gamma <- tidy(lda_model, matrix = "gamma")

# Asignar cada documento al tópico dominante
docs_topico_principal <- docs_gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  mutate(document = as.numeric(document))

# Unir con datos originales para ver títulos
conferencias_con_topico <- conferencias %>%
  left_join(docs_topico_principal, by = c("doc_id" = "document"))

# Ver distribución de conferencias por tópico
tabla_distribucion <- conferencias_con_topico %>%
  count(topic, sort = TRUE)

print(tabla_distribucion)

# Ver algunos ejemplos de conferencias por tópico
i = 1
for(i in 1:k_topics) {
  cat("\n=== CONFERENCIAS DEL TÓPICO", i, "===\n")
  ejemplos <- conferencias_con_topico %>%
    filter(topic == i) %>%
    arrange(desc(gamma)) %>%
    head(5) %>%
    select(fecha, gamma)
  print(ejemplos)
}

# ----------------------------------------------------------------------------
# 7. ETIQUETAR TÓPICOS CON OLLAMA
# ----------------------------------------------------------------------------

# Función para etiquetar un tópico usando Ollama
etiquetar_topico <- function(topic_num, top_words, modelo = "llama3.1:latest") {

  # Preparar el prompt
  palabras_str <- paste(top_words, collapse = ", ")

  prompt <- paste0(
    "Analiza las siguientes palabras clave que representan un tópico identificado ",
    "en las conferencias estenográficas de la presidenta de México Claudia Sheinbaum en 2025:\n\n",
    palabras_str, "\n\n",
    "Basándote en estas palabras, proporciona:\n",
    "1. Un título descriptivo corto (máximo 5 palabras) para este tópico\n",
    "2. Una breve descripción de 1-2 oraciones sobre qué trata este tópico en el contexto ",
    "de las políticas y programas del gobierno mexicano\n\n",
    "Responde en español de forma concisa."
  )

  # Crear comando para Ollama
  cmd <- sprintf('ollama run %s "%s"', modelo, gsub('"', '\\"', prompt))

  # Ejecutar comando
  resultado <- system(cmd, intern = TRUE)

  return(paste(resultado, collapse = "\n"))
}

# Extraer top 15 palabras por tópico para etiquetar
topicos_para_etiquetar <- topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  summarise(palabras = list(term))

# Etiquetar cada tópico
etiquetas_topicos <- list()

i = 1
for(i in 1:k_topics) {
  palabras <- topicos_para_etiquetar$palabras[[i]]

  cat("\n--- Etiquetando Tópico", i, "---\n")

  etiqueta <- etiquetar_topico(i, palabras)
  etiquetas_topicos[[i]] <- etiqueta

  cat("Palabras clave:", paste(palabras[1:10], collapse = ", "), "\n")
  cat("Etiqueta generada:\n")
  cat(etiqueta, "\n")
}

# ----------------------------------------------------------------------------
# 8. RESUMEN FINAL
# ----------------------------------------------------------------------------

cat("\n\n============================================================================\n")
cat("RESUMEN DEL ANÁLISIS DE MODELADO DE TÓPICOS\n")
cat("Conferencias Estenográficas - Presidenta Claudia Sheinbaum 2025\n")
cat("============================================================================\n\n")

cat("Total de conferencias analizadas:", nrow(conferencias), "\n")
cat("Número de tópicos identificados:", k_topics, "\n")
cat("Total de términos únicos:", ncol(dtm), "\n\n")

cat("Distribución de conferencias por tópico:\n")
print(tabla_distribucion)

cat("\n\nTópicos identificados con sus términos principales:\n\n")
i = 1
for(i in 1:k_topics) {
  cat("--- TÓPICO", i, "---\n")

  # Términos principales
  terminos <- top_terms %>%
    filter(topic == i) %>%
    head(10) %>%
    pull(term)
  cat("Términos:", paste(terminos, collapse = ", "), "\n")

  # Número de conferencias
  n_docs <- sum(conferencias_con_topico$topic == i, na.rm = TRUE)
  cat("Conferencias:", n_docs, "\n")

  # Etiqueta generada
  if(length(etiquetas_topicos) >= i) {
    cat("\nEtiqueta:\n", etiquetas_topicos[[i]], "\n")
  }

  cat("\n")
}

cat("\nAnálisis completado exitosamente.\n")
cat("Gráfico guardado en: 03_Graficas/topicos_conferencias_sheinbaum.png\n")
