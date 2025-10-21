
# Cargar librerías necesarias
library(tm) # stopwords, funciones de minería de texto
library(topicmodels) # Aplicación de LDA
library(tidytext) # Manejo de texto, tokenización
library(tidyverse) # Manejo de datos

# Preparar los documentos
docs <- c(
  "El equipo ganó el partido y el entrenador estaba contento tras la victoria.",
  "El jugador anotó un gol en el último minuto del partido de fútbol.",
  "El presidente anunció nuevas medidas de política económica en su discurso de la noche.",
  "El ministro presentó un plan económico y político para el desarrollo del país.",
  "La empresa tecnológica lanzó un nuevo smartphone con cámara de alta resolución.",
  "Millones de personas compraron el smartphone inteligente este año."
)

# 1. Preprocesamiento: crear corpus y matriz documento-término
# Crear un corpus
corpus <- Corpus(VectorSource(docs))

# Preprocesamiento del texto
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Definir stopwords en español (similar a las del ejemplo Python)
stopwords_es <- c("el", "la", "de", "y", "en", "un", "una", "para", 
                  "del", "con", "por", "al", "los", "las")
corpus <- tm_map(corpus, removeWords, stopwords_es)

# Crear matriz documento-término
dtm <- DocumentTermMatrix(corpus)

# 2. Entrenar el modelo LDA con 3 tópicos
set.seed(0)  # Para reproducibilidad
lda_model <- LDA(dtm, k = 3, control = list(seed = 0))

# 3. Mostrar las palabras más probables de cada tópico
# Obtener los términos por tópico
terms_per_topic <- terms(lda_model, 5)  # Top 5 palabras por tópico

# Mostrar resultados
for(i in 1:3) {
  print(str_c("Tópico ", i, ": ", paste(terms_per_topic[,i], collapse = ", ")))
}

# Alternativa: ver la matriz beta (probabilidades palabra-tópico) de forma más detallada
library(tidytext)
topics_tidy <- tidy(lda_model, matrix = "beta")

# Ver las 5 palabras más probables por tópico
top_terms <- topics_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  arrange(topic, -beta)

print(top_terms)

# Opcional: Ver la distribución de tópicos por documento
doc_topics <- posterior(lda_model)$topics
print("Distribución de tópicos por documento:")
print(round(doc_topics, 3))
