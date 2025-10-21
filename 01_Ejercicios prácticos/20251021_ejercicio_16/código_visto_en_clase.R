
# libreria ---
library(tm)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(readxl)
library(rebus)
library(udpipe)

# Cargamos los datos ----
articulos_juve <- read_excel("01_datos/tabla_archivos_juve.xlsx") %>% 
  select(titulos, textos) %>% 
  mutate(id = 1:nrow(.)) %>% 
  relocate(id, .before = titulos)

# Preprocesamiento ----
stop_words <- stopwords(kind = "spanish")

stopwords_2 <- str_c("\\b", stop_words, "\\b")

articulos_juve2 <- articulos_juve %>% 
  mutate(textos = str_to_lower(textos)) %>% 
  mutate(textos = str_remove_all(textos, pattern = "[:punct:]")) %>% 
  mutate(textos = str_remove_all(textos, c(or1(stopwords_2)))) %>% 
  mutate(textos = str_replace_all(textos, c("redes sociales" = "redes_sociales", 
                                            "medio comunicación" = "medio_comunicacion", 
                                            "nativo digital" = "nativo_digital", 
                                            "inteligencia artificial" = "inteligencia_artificial", 
                                            "\\bia\\b" = "inteligencia_artificial", 
                                            "nuevo león" = "nuevo_león", 
                                            "ciudad méxico" = "ciudad_de_méxico", 
                                            "jornada laboral" = "jornada_laboral", 
                                            "manolo jiménez" = "manolo_jiménez", 
                                            "población económicamente activa" = "población_económicamente_activa"
                                            ))) %>% 
  mutate(textos = str_squish(textos)) 
  
# ngramas <- articulos_juve2 %>% 
#   unnest_tokens(output = "palabra", 
#                 input = textos, 
#                 token = "ngrams", n = 3) %>% 
#   group_by(palabra) %>% 
#   count() %>% 
#   arrange(-n)

modelo <- udpipe_download_model(language = "spanish")
udmodel <- udpipe_load_model(modelo$file_model)

lemas_juve <- udpipe_annotate(udmodel, x = articulos_juve2$textos) %>% 
  as_tibble() %>% 
  mutate(id = str_extract(doc_id, pattern = "\\d+")) %>% 
  select(id, token, lemma, upos) %>% 
  filter(upos != "PUNCT") %>% 
  mutate(is.number = str_detect(lemma, pattern = "\\d")) %>% 
  filter(!is.number) %>% 
  filter(!(lemma %in% c("él", "hacer", "Coahuila", 
                        "México","méxico","poder", "primero", "mayor", "año", 
                        "bien", "cada", "solo")))


# Matriz documento término 
dtm_juve <- lemas_juve %>% 
  group_by(id, lemma) %>% 
  count() %>% 
  cast_dtm(document = id, term = lemma, value = n)

dtm_juve %>% as.matrix() %>% as_tibble()

# Modelado de tópicos
lda_juve <- LDA(x = dtm_juve, k = 5, method = "Gibbs", control = list(seed = 1111))

# Betas: 
betas_juve <- tidy(lda_juve, matrix = "beta")

# Gamas: 
gammas_juve <- tidy(lda_juve, matrix = "gamma")


# Extraemos la matríz beta: 
# La matríz Beta es la matríz que nos dá la probabilidad de ocurrencia de w dado Z.
# P(w|z)
betas_juve <- tidy(lda_juve, matrix = "beta")
betas_juve %>%
  filter(topic == 2) %>%
  arrange(-beta) %>% 
  print(n = 50)

# 10 principales palabras por tema: 
bd_plot <- betas_juve %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) 

bd_plot %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Extraemos la matríz gamma: 
# La matríz gamma nos dice la probabilidad de distribución de cada tema, z para cada documento, d
gammas_juve <- tidy(lda_juve, matrix = "gamma") %>% 
  mutate(document = as.numeric(document)) %>% 
  arrange(document, -gamma) %>% 
  left_join(catalogo_articulos)

# Etiquetemos temas.
betas_juve
catalogo_temas <- tibble(topic = 1:6,
                         tema = c("Estadísticas", 
                                  "Agua",
                                  "IA",
                                  "Economía",
                                  "Laboral",
                                  "Elecciones"))

# Clasifiquemos artículos:
gammas_juve %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  left_join(catalogo_temas) %>%
  View()

source("02_codigo/etiquetado_automatizado.R")
etiquetas <- label_topics_with_ollama(lda_fit = lda_juve, 
                                      docs_df = articulos_juve %>% rename(doc_id = id, 
                                                                          text = textos, 
                                                                          title = titulos))











# ====== DETERMINACIÓN DEL NÚMERO ÓPTIMO DE TÓPICOS ======

# Definir rango de tópicos a evaluar
k_values <- seq(2, 25, by = 1)  # Evaluar de 2 a 15 tópicos

# Calcular perplejidad para cada valor de k
perplexity_values <- sapply(k_values, function(k) {
  cat("Calculando modelo con k =", k, "\n")
  
  # Ajustar modelo LDA
  lda_model <- LDA(dtm_juve, 
                   k = k, 
                   method = "Gibbs", 
                   control = list(seed = 1111, 
                                  iter = 2000,  # Número de iteraciones
                                  thin = 100,   # Thinning
                                  burnin = 1000)) # Burn-in
  
  # Calcular perplejidad - CORRECCIÓN: especificar newdata
  perplexity(lda_model, newdata = dtm_juve)
})

# Crear data frame con resultados
perplexity_results <- tibble(
  k = k_values,
  perplexity = perplexity_values
)

# Visualizar resultados
library(ggplot2)

perplexity_plot <- ggplot(perplexity_results, aes(x = k, y = perplexity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(title = "Perplejidad vs Número de Tópicos",
       subtitle = "Valores más bajos indican mejor ajuste",
       x = "Número de Tópicos (k)",
       y = "Perplejidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(perplexity_plot)

# Mostrar tabla de resultados
print(perplexity_results)

# Identificar el k con menor perplejidad
optimal_k <- perplexity_results %>%
  filter(perplexity == min(perplexity)) %>%
  pull(k)

cat("\n================================================\n")
cat("Número óptimo de tópicos basado en perplejidad:", optimal_k, "\n")
cat("Perplejidad mínima:", min(perplexity_values), "\n")
cat("================================================\n")

