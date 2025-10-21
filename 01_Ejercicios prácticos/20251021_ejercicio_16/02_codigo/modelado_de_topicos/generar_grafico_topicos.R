# Script para generar el gráfico de tópicos


library(tidyverse)
library(tidytext)
library(readxl)
library(topicmodels)
library(tm)

# Leer datos
articulos <- read_excel("01_Datos/tabla_archivos_juve.xlsx")
articulos <- articulos %>% mutate(doc_id = row_number())

# Tokenización y limpieza
tokens <- articulos %>%
  select(doc_id, titulos, textos) %>%
  unnest_tokens(word, textos)

stopwords_es <- stopwords::stopwords("es", source = "stopwords-iso")
stopwords_custom <- c(stopwords_es,
                     "si", "así", "ser", "hacer", "puede", "pueden",
                     "vez", "través", "año", "años", "día", "días",
                     "aquí", "allí", "ahí", "tan", "tal", "también",
                     "bien", "sí", "no", "cómo", "dónde", "cuándo")

tokens_limpios <- tokens %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) >= 3) %>%
  filter(!word %in% stopwords_custom)

# Crear DTM
doc_words <- tokens_limpios %>%
  count(doc_id, word, sort = TRUE)

dtm <- doc_words %>%
  cast_dtm(doc_id, word, n)

# Modelo LDA
set.seed(1234)
lda_model <- LDA(dtm, k = 5, method = "Gibbs",
                 control = list(seed = 1234, iter = 2000, thin = 100, burnin = 100))

# Extraer términos
topics_beta <- tidy(lda_model, matrix = "beta")

top_terms <- topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Crear y guardar gráfico
grafico <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 términos por tópico",
       subtitle = "Análisis de modelado de tópicos - Artículos de Juvenal Campos",
       x = "Beta (probabilidad del término en el tópico)",
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"))

# Guardar gráfico
ggsave("03_Graficas/topicos_terminos.png",
       plot = grafico,
       width = 14,
       height = 10,
       dpi = 300,
       bg = "white")

print("Gráfico guardado exitosamente en: 03_Graficas/topicos_terminos.png")
