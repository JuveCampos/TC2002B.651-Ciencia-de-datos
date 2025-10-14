# ============================================================================
# Análisis de Modelado de Tópicos para Artículos de Juvenal Campos
# ATiempo.Tv
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

# Leer el archivo Excel con los artículos
articulos <- read_excel("01_Datos/tabla_archivos_juve.xlsx")

# Explorar la estructura del dataset
str(articulos)
head(articulos)

# Verificar dimensiones
dim(articulos)

# Revisar nombres de columnas
names(articulos)

# Verificar valores faltantes en columnas clave
sum(is.na(articulos$titulos))
sum(is.na(articulos$textos))

# Crear un ID único para cada artículo si no existe
articulos <- articulos %>%
  mutate(doc_id = row_number())

# Revisar longitud de textos
articulos <- articulos %>%
  mutate(longitud_texto = nchar(textos)) %>% 
  arrange(longitud_texto)

summary(articulos$longitud_texto)

# ----------------------------------------------------------------------------
# 2. PREPROCESAMIENTO DE TEXTOS
# ----------------------------------------------------------------------------

# Tokenización: dividir textos en palabras individuales
tokens <- articulos %>%
  select(doc_id, titulos, textos) %>%
  unnest_tokens(word, textos)

# Cargar stopwords en español
stopwords_es <- stopwords::stopwords("es", source = "stopwords-iso")

# Agregar stopwords personalizadas comunes en artículos periodísticos
stopwords_custom <- c(stopwords_es,
                     "si", "así", "ser", "hacer", "puede", "pueden",
                     "vez", "través", "año", "años", "día", "días",
                     "aquí", "allí", "ahí", "tan", "tal", "también",
                     "bien", "sí", "no", "cómo", "dónde", "cuándo")

# Limpiar tokens: eliminar stopwords, números y palabras muy cortas
tokens_limpios <- tokens %>%
  # Convertir a minúsculas (ya hecho por unnest_tokens)
  # Eliminar números
  filter(!str_detect(word, "^[0-9]+$")) %>%
  # Eliminar palabras muy cortas (menos de 3 caracteres)
  filter(nchar(word) >= 3) %>%
  # Eliminar stopwords
  filter(!word %in% stopwords_custom)

# Contar frecuencias de palabras
frecuencias_palabras <- tokens_limpios %>%
  count(word, sort = TRUE)

# Ver las 20 palabras más frecuentes
head(frecuencias_palabras, 20)

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
# Probaremos con k=5 inicialmente (ajustable según resultados)
k_topics <- 5

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
topics_beta <- tidy(lda_model, matrix = "beta") %>% 
  arrange(topic, -beta)

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
  labs(title = "Top 10 términos por tópico",
       x = "Beta (probabilidad del término en el tópico)",
       y = NULL) +
  theme_minimal()

# Guardar gráfico
ggsave("03_Graficas/topicos_terminos.png", width = 12, height = 8, dpi = 300)

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
articulos_con_topico <- articulos %>%
  left_join(docs_topico_principal, by = c("doc_id" = "document"))

# Ver distribución de artículos por tópico
tabla_distribucion <- articulos_con_topico %>%
  count(topic, sort = TRUE)

print(tabla_distribucion)

# Ver algunos ejemplos de artículos por tópico
for(i in 1:k_topics) {
  cat("\n=== ARTÍCULOS DEL TÓPICO", i, "===\n")
  ejemplos <- articulos_con_topico %>%
    filter(topic == i) %>%
    arrange(desc(gamma)) %>%
    head(5) %>%
    select(titulos, gamma)
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
    "en artículos periodísticos escritos por Juvenal Campos para ATiempo.Tv:\n\n",
    palabras_str, "\n\n",
    "Basándote en estas palabras, proporciona:\n",
    "1. Un título descriptivo corto (máximo 5 palabras) para este tópico\n",
    "2. Una breve descripción de 1-2 oraciones sobre qué trata este tópico\n\n",
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
cat("============================================================================\n\n")

cat("Total de artículos analizados:", nrow(articulos), "\n")
cat("Número de tópicos identificados:", k_topics, "\n")
cat("Total de términos únicos:", ncol(dtm), "\n\n")

cat("Distribución de artículos por tópico:\n")
print(tabla_distribucion)

cat("\n\nTópicos identificados con sus términos principales:\n\n")
for(i in 1:k_topics) {
  cat("--- TÓPICO", i, "---\n")

  # Términos principales
  terminos <- top_terms %>%
    filter(topic == i) %>%
    head(10) %>%
    pull(term)
  cat("Términos:", paste(terminos, collapse = ", "), "\n")

  # Número de artículos
  n_docs <- sum(articulos_con_topico$topic == i, na.rm = TRUE)
  cat("Artículos:", n_docs, "\n")

  # Etiqueta generada
  if(length(etiquetas_topicos) >= i) {
    cat("\nEtiqueta:\n", etiquetas_topicos[[i]], "\n")
  }

  cat("\n")
}

cat("\nAnálisis completado exitosamente.\n")
cat("Gráfico guardado en: 03_Graficas/topicos_terminos.png\n")
