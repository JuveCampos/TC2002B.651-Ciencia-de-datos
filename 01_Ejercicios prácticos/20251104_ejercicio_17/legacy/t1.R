

# ============================================================
# Análisis de sentimiento con embeddings (Ollama + mxbai-embed-large)
# Método: zero-shot por similitud con prototipos (positivo/negativo/neutro)
# Requisitos previos (una sola vez):
#   - Ollama instalado y corriendo: `ollama serve`
#   - Modelo: `ollama pull mxbai-embed-large`
#   - Paquetes R: install.packages(c("tidyverse","httr2","purrr","ggplot2"))
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(purrr)
  library(ggplot2)
})

# ---------------------------
# Parámetros
# ---------------------------
OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL <- "mxbai-embed-large"

# ---------------------------
# Utilidades
# ---------------------------
cosine_sim <- function(a, b) {
  # a y b: vectores numéricos del mismo largo
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

get_embedding_ollama <- function(texto,
                                 model = EMBED_MODEL,
                                 host = OLLAMA_HOST) {
  # Llama al endpoint local de Ollama para embeddings
  req <- request(paste0(host, "/api/embeddings")) |>
    req_body_json(list(
      model = model,
      prompt = texto
    )) |>
    req_perform()
  
  resp <- resp_body_json(req)
  emb <- as.numeric(unlist(resp$embedding))  # <- garantiza vector numérico
  emb
}

# (Opcional) Comprobación rápida de que Ollama responde
# Si falla, asegúrate de ejecutar `ollama serve` en una terminal
invisible(try({
  request(paste0(OLLAMA_HOST, "/api/tags")) |> req_perform()
}, silent = TRUE))

# ---------------------------
# Textos de ejemplo a clasificar (edítalos libremente)
# ---------------------------
textos <- tibble::tibble(
  id = 1:8,
  texto = c(
    "El servicio fue excelente, súper recomendado.",
    "Me atendieron tarde y además fueron groseros.",
    "No estuvo mal, pero esperaba más.",
    "Buenísimo... si te gusta esperar una hora.",
    "Está bien, nada especial.",
    "La comida llegó fría, pésimo.",
    "Pensé que iba a ser peor, al final estuvo decente.",
    "La plataforma es lenta pero el soporte es muy amable."
  )
)

# ---------------------------
# Prototipos (ajústalos a tu dominio si quieres)
# ---------------------------
prototipos <- tibble::tibble(
  etiqueta = c("positivo", "negativo", "neutro"),
  texto = c(
    "Esta es una opinión positiva, la persona está satisfecha y recomienda.",
    "Esta es una opinión negativa, la persona se queja o está inconforme.",
    "Esta es una opinión neutral, la persona describe sin emoción clara."
  )
)

# ---------------------------
# Embeddings de prototipos y de textos
# ---------------------------
prototipos_emb <- prototipos %>%
  mutate(embedding = map(texto, ~ get_embedding_ollama(.x)))

textos_emb <- textos %>%
  mutate(embedding = map(texto, ~ get_embedding_ollama(.x)))

# Verificaciones mínimas (dimensiones y tipo)
stopifnot(is.numeric(prototipos_emb$embedding[[1]]))
stopifnot(is.numeric(textos_emb$embedding[[1]]))
stopifnot(length(unique(map_int(prototipos_emb$embedding, length))) == 1)
stopifnot(length(unique(map_int(textos_emb$embedding,     length))) == 1)

# ---------------------------
# Clasificación por similitud (versión robusta sin rowwise)
# ---------------------------
proto_embs   <- prototipos_emb$embedding          # lista de vectores
proto_labels <- prototipos_emb$etiqueta           # etiquetas

# Para cada texto, calculamos similitud con cada prototipo
sim_list <- map(textos_emb$embedding, function(emb_texto) {
  sims <- map_dbl(proto_embs, ~ cosine_sim(.x, emb_texto))
  tibble(etiqueta = proto_labels, sim = sims)
})

# Elegimos el mejor prototipo por texto
clasificados <- tibble(
  id          = textos_emb$id,
  texto       = textos_emb$texto,
  similitudes = sim_list
) %>%
  mutate(
    idx_max       = map_int(similitudes, ~ which.max(.x$sim)),
    sentimiento   = map2_chr(similitudes, idx_max, ~ .x$etiqueta[.y]),
    similitud_max = map2_dbl(similitudes, idx_max, ~ .x$sim[.y])
  ) %>%
  select(id, texto, sentimiento, similitud_max)

# ---------------------------
# Resultados
# ---------------------------
cat("\n=== RESULTADOS ===\n")
print(clasificados)

cat("\n=== DETALLES (todas las similitudes por texto) ===\n")
detalles <- tibble(
  id = textos_emb$id,
  texto = textos_emb$texto,
  similitudes = sim_list
) %>%
  unnest(similitudes) %>%
  arrange(id, desc(sim))
print(detalles)

# ---------------------------
# Visualización rápida (opcional)
# ---------------------------
ggplot(clasificados,
       aes(x = reorder(substr(texto, 1, 60), similitud_max),
           y = similitud_max,
           fill = sentimiento)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentimiento por embeddings (Ollama + mxbai-embed-large)",
    x = "Texto (truncado)",
    y = "Similitud con el prototipo elegido",
    fill = "Sentimiento"
  ) +
  theme_minimal(base_size = 12)
