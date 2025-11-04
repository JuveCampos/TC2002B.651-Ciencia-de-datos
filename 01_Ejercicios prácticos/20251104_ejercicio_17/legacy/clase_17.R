
# Librerías/Paquetes
library(tidyverse)
library(rvest)
library(tidytext)
library(syuzhet)   # léxico NRC con opción para español
library(httr2)     # Consulta de APIs
library(jsonlite)  # Consulta de Jsons

# Extraemos la versión estenográfica de la mañanera. 
url <- "https://www.gob.mx/presidencia/es/articulos/version-estenografica-conferencia-de-prensa-de-la-presidenta-claudia-sheinbaum-pardo-del-03-de-noviembre-de-2025?idiom=es"
  # readxl::read_xlsx("01_Datos/estenograficas_sheinbaum.xlsx") %>% 
  # head(1)
  
html <- read_html(url)

p <- html %>% 
  html_nodes("p") %>% 
  html_text() 

steno <- tibble(texto = p) %>% 
  mutate(quien_dijo = str_extract(texto,
                                  "[A-ZÁÉÍÓÚÜÑ]{2,}(?:\\s+[A-ZÁÉÍÓÚÜÑ]{2,})+|PREGUNTA")) %>%
  fill(quien_dijo,.direction = "down") %>% 
  filter(!is.na(quien_dijo))
  


# Exploramos el texto: 
steno$texto

# Limpiamos los tokens
frases <- steno %>% 
  unnest_tokens(output = "tokens", 
                input = texto,
                token = "sentences", 
                to_lower = FALSE) %>% 
  mutate(tokens = str_remove_all(tokens, pattern = "[A-ZÁÉÍÓÚÜÑ]{2,}(?:\\s+[A-ZÁÉÍÓÚÜÑ]{2,})+"
)) %>% 
  mutate(tokens = str_remove_all(tokens, "\\s*,\\s*:\\s*"))

# Ahora hacemos el análisis de sentimientos.

# ============================================================
# 1. Análisis de sentimientos con NRC en español (syuzhet)
# ============================================================

# 1. Calculamos los sentimientos por frase usando NRC (lang = "spanish")

sentimientos_syuzhet <- syuzhet::get_nrc_sentiment(frases$tokens, lang = "spanish") %>% 
  as_tibble() 
# %>% 
#   rename(enojo = anger, 
#          anticipacion = anticipation, 
#          disgusto = disgust, 
#          miedo = fear, 
#          alegría = joy, 
#          tristeza = sadness, 
#          sorpresa = surprise, 
#          confianza = trust, 
#          negativo = negative, 
#          positivo = positive)

sentimientos_frases <- frases %>% 
  # añadimos un id de frase para referencia posterior
  mutate(id_frase = row_number()) %>% 
  # unimos, columna a columna, la matriz de sentimientos devuelta por syuzhet
  bind_cols(
    sentimientos_syuzhet
  ) %>% 
  # 2. Creamos un score simple de polaridad por frase
  mutate(
    # columnas que devuelve NRC: anger, anticipation, disgust, fear,
    # joy, sadness, surprise, trust, negative, positive
    score_polaridad = positive - negative
  )

sentimientos_frases %>% 
  select(id_frase, tokens, positive, negative) %>% 
  View()

# 3. Resumen global de la mañanera (sumando todas las frases)
resumen_sentimiento <- sentimientos_frases %>% 
  summarise(
    total_positive = sum(positive, na.rm = TRUE),
    total_negative = sum(negative, na.rm = TRUE),
    score_global   = total_positive - total_negative
  )

# 4. Pasamos a formato "largo" para analizar y graficar emociones
sentimientos_largos <- sentimientos_frases %>% 
  select(
    id_frase,
    anger:positive   # todas las columnas de emociones + positivo/negativo
  ) %>% 
  pivot_longer(
    cols      = anger:positive,
    names_to  = "emocion",
    values_to = "valor"
  ) %>% 
  group_by(emocion) %>% 
  summarise(
    valor_total = sum(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(valor_total))

# 5. Ejemplo rápido de gráfica de emociones
sentimientos_largos %>% 
  ggplot(aes(x = reorder(emocion, valor_total), y = valor_total)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Emoción (NRC)",
    y = "Recuento (suma en todas las frases)",
    title = "Distribución de emociones en la mañanera",
    subtitle = "Léxico NRC en español vía syuzhet"
  )

# Ahora con Embeddings. 
OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL_LOCAL <- "mxbai-embed-large"
LLM_MODELS <- c("llama3.1:latest", "qwen2.5:latest")
OPENAI_API_KEY <- "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"


# Consulta al modelo de embeddings de Ollama

model = EMBED_MODEL_LOCAL
texto = "Hola"

req <- request(paste0(OLLAMA_HOST, "/api/embeddings")) %>% 
  req_body_json(list(model = model, prompt = texto)) %>% 
  req_perform()

resp <- resp_body_json(req)
as.numeric(unlist(resp$embedding))



