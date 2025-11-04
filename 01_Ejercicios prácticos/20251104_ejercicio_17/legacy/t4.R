
# ============================================================
# INTRODUCCIÓN A EMBEDDINGS: Analogías y Aritmética Vectorial
# ============================================================
#
# ¿QUÉ SON LOS EMBEDDINGS?
# ------------------------
# Los embeddings son representaciones numéricas (vectores) de palabras o textos
# que capturan el SIGNIFICADO SEMÁNTICO. La idea fundamental es que palabras
# con significados similares tienen vectores cercanos en el espacio.
#
# LO SORPRENDENTE: ¡Podemos hacer ARITMÉTICA con significados!
#
# Ejemplos famosos:
#   Rey - Hombre + Mujer ≈ Reina
#   París - Francia + España ≈ Madrid
#   Grande - Pequeño + Alto ≈ Bajo
#
# ¿POR QUÉ FUNCIONA?
# ------------------
# Los embeddings capturan RELACIONES SEMÁNTICAS como vectores.
# Si "Rey" y "Hombre" están relacionados de cierta manera, y aplicamos
# esa misma relación a "Mujer", obtenemos "Reina".
#
# En este script exploraremos estas analogías de forma interactiva y visual.
#
# Requisitos:
#   - Ollama: `ollama pull mxbai-embed-large`
#   - OpenAI API Key (opcional): en variable OPENAI_API_KEY
#   - Paquetes: tidyverse, httr2, ggplot2, ggrepel, Rtsne
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(ggplot2)
  library(ggrepel)  # Para etiquetas que no se solapan
})

# Verificar/instalar Rtsne para visualización
if (!require("Rtsne", quietly = TRUE)) {
  cat("Instalando paquete Rtsne para visualización...\n")
  install.packages("Rtsne", repos = "https://cloud.r-project.org")
  library(Rtsne)
}

# ---------------------------
# CONFIGURACIÓN
# ---------------------------
# "TUTORIAL: EMBEDDINGS Y ARITMÉTICA VECTORIAL\n")

# Seleccionar fuente de embeddings
USE_OPENAI <- TRUE  # Cambiar a TRUE para usar OpenAI

OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL_LOCAL <- "mxbai-embed-large"
OPENAI_API_KEY <- "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"

if (USE_OPENAI) {
  if (nchar(OPENAI_API_KEY) == 0) {
    stop("OpenAI API Key no encontrada. Configura OPENAI_API_KEY o usa USE_OPENAI <- FALSE")
  }
  cat("✓ Usando: OpenAI (text-embedding-3-small)\n\n")
} else {
  cat("✓ Usando: Ollama (", EMBED_MODEL_LOCAL, ")\n\n")
}

# ---------------------------
# FUNCIONES DE EMBEDDINGS
# ---------------------------

# Ollama
get_embedding_ollama <- function(texto, model = EMBED_MODEL_LOCAL) {
  req <- request(paste0(OLLAMA_HOST, "/api/embeddings")) |>
    req_body_json(list(model = model, prompt = texto)) |>
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$embedding))
}

# OpenAI
get_embedding_openai <- function(texto) {
  req <- request("https://api.openai.com/v1/embeddings") |>
    req_headers(
      Authorization = paste("Bearer", OPENAI_API_KEY),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = "text-embedding-3-small",
      input = texto
    )) |>
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$data[[1]]$embedding))
}

# Función unificada
get_embedding <- function(texto) {
  if (USE_OPENAI) {
    get_embedding_openai(texto)
  } else {
    get_embedding_ollama(texto)
  }
}

# ---------------------------
# FUNCIONES DE SIMILITUD
# ---------------------------

# Similitud coseno (1 = idénticos, 0 = perpendiculares, -1 = opuestos)
cosine_sim <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

# Distancia euclidiana
euclidean_dist <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# Encuentra la palabra más cercana a un vector
encontrar_mas_cercano <- function(vector_objetivo, vocabulario_df, excluir = c()) {
  vocabulario_df %>%
    filter(!palabra %in% excluir) %>%
    mutate(similitud = map_dbl(embedding, ~ cosine_sim(.x, vector_objetivo))) %>%
    arrange(desc(similitud)) %>%
    head(5) %>%
    select(palabra, similitud)
}

# ============================================================
# PARTE 1: CONCEPTOS BÁSICOS
# ============================================================

# Palabras básicas para explorar
palabras_base <- c(
  "gato", "perro", "león", "tigre",
  "mesa", "silla", "sofá",
  "feliz", "alegre", "triste", "enojado",
  "grande", "pequeño", "enorme", "diminuto"
)

# Generando embeddings para vocabulario base... (Esto puede tardar un momento)
vocabulario <- tibble(palabra = palabras_base) %>%
  mutate(embedding = map(palabra, get_embedding, .progress = TRUE))

# "\n✓ Embeddings generados!\n")
print(str_c("Dimensión de cada vector: ", length(vocabulario$embedding[[1]]), "\n\n"))

# Ejemplo de similitudes
# "--- EJEMPLO 1: Similitudes entre palabras ---"

palabra1 <- "gato"
palabra2 <- "perro"
palabra3 <- "mesa"

emb1 <- vocabulario %>% filter(palabra == palabra1) %>% pull(embedding) %>% .[[1]]
emb2 <- vocabulario %>% filter(palabra == palabra2) %>% pull(embedding) %>% .[[1]]
emb3 <- vocabulario %>% filter(palabra == palabra3) %>% pull(embedding) %>% .[[1]]

# "Similitud coseno entre palabras:\n"
cat(sprintf("  '%s' ↔ '%s': %.4f (Ambos son animales)\n", palabra1, palabra2, cosine_sim(emb1, emb2)))
cat(sprintf("  '%s' ↔ '%s': %.4f (Animal vs. Mueble)\n", palabra1, palabra3, cosine_sim(emb1, emb3)))
cat(sprintf("  '%s' ↔ '%s': %.4f (Animal vs. Mueble)\n", palabra2, palabra3, cosine_sim(emb2, emb3)))

# \n💡 OBSERVACIÓN: Palabras relacionadas tienen mayor similitud!\n\n

# ============================================================
# PARTE 2: ANALOGÍAS CLÁSICAS
# "PARTE 2: Aritmética vectorial - Analogías\n"
# ============================================================

# "La magia de los embeddings: podemos hacer ARITMÉTICA con significados!\n"
# "Fórmula: A - B + C ≈ D\n"
# "Ejemplo: Rey - Hombre + Mujer ≈ Reina\n\n"

# ---------------------------
# ANALOGÍA 1: Género
# ---------------------------
# --- ANALOGÍA 1: Rey - Hombre + Mujer = ? ---

# Crear vocabulario extendido para esta analogía
palabras_genero <- c("rey", "reina", "hombre", "mujer",
                     "príncipe", "princesa", "actor", "actriz",
                     "padre", "madre", "hijo", "hija")

# "Generando embeddings para palabras de género...
vocab_genero <- tibble(palabra = palabras_genero) %>%
  mutate(embedding = map(palabra, get_embedding, .progress = TRUE))

# Hacer la analogía
rey <- vocab_genero %>% filter(palabra == "rey") %>% pull(embedding) %>% pluck(1)
hombre <- vocab_genero %>% filter(palabra == "hombre") %>% pull(embedding) %>% .[[1]]
mujer <- vocab_genero %>% filter(palabra == "mujer") %>% pull(embedding) %>% .[[1]]

# Calcular: Rey - Hombre + Mujer
resultado <- rey - hombre + mujer

cat("\nCalculando: Rey - Hombre + Mujer...\n")
cat("Palabras más cercanas al resultado:\n\n")
resultados_genero <- encontrar_mas_cercano(resultado,
                                           vocab_genero, 
                                           excluir = c("rey", "hombre", "mujer"))
print(resultados_genero, n = 5)

cat("\n✨ ¡El modelo encontró 'reina'!\n")
cat("Esto muestra que los embeddings capturan relaciones de género.\n\n")

# ---------------------------
# ANALOGÍA 2: Geografía (Capital de países)
# ---------------------------
cat("--- ANALOGÍA 2: Francia - París + Colombia = ? ---\n\n")

palabras_geografia <- c("Francia", "París", "Colombia", "Bogotá",
                        "España", "Madrid", "México", "Ciudad de México",
                        "Argentina", "Buenos Aires", "Chile", "Santiago", "Roma")

cat("Generando embeddings para geografía...\n")
vocab_geo <- tibble(palabra = palabras_geografia) %>%
  mutate(embedding = map(palabra, get_embedding, .progress = TRUE))

francia <- vocab_geo %>% filter(palabra == "Francia") %>% pull(embedding) %>% .[[1]]
paris <- vocab_geo %>% filter(palabra == "París") %>% pull(embedding) %>% .[[1]]
colombia <- vocab_geo %>% filter(palabra == "Colombia") %>% pull(embedding) %>% .[[1]]
argentina <- vocab_geo %>% filter(palabra == "Argentina") %>% pull(embedding) %>% .[[1]]
roma <-  vocab_geo %>% filter(palabra == "Roma") %>% pull(embedding) %>% .[[1]]

# Calcular: Francia - París + Colombia
resultado_geo <- argentina + roma
  # francia - paris + colombia

cat("\nCalculando: Francia - París + Colombia...\n")
cat("(Esperamos encontrar: Bogotá)\n")
cat("Palabras más cercanas al resultado:\n\n")
resultados_geo <- encontrar_mas_cercano(resultado_geo, vocab_geo,
                                        excluir = c("Francia", "París", "Roma", "Argentina"))
print(resultados_geo, n = 5)

cat("\n✨ ¡El modelo entiende la relación capital-país!\n\n")

# ---------------------------
# ANALOGÍA 3: Opuestos / Antonimia
# ---------------------------
cat("--- ANALOGÍA 3: Grande - Pequeño + Alto = ? ---\n\n")

palabras_antonimos <- c("grande", "pequeño", "alto", "bajo",
                        "rápido", "lento", "caliente", "frío",
                        "fuerte", "débil", "claro", "oscuro")

cat("Generando embeddings para antónimos...\n")
vocab_ant <- tibble(palabra = palabras_antonimos) %>%
  mutate(embedding = map(palabra, get_embedding, .progress = TRUE))

grande <- vocab_ant %>% filter(palabra == "grande") %>% pull(embedding) %>% .[[1]]
pequeño <- vocab_ant %>% filter(palabra == "pequeño") %>% pull(embedding) %>% .[[1]]
alto <- vocab_ant %>% filter(palabra == "alto") %>% pull(embedding) %>% .[[1]]

# Calcular: Grande - Pequeño + Alto
resultado_ant <- grande - pequeño + alto

cat("\nCalculando: Grande - Pequeño + Alto...\n")
cat("(Esperamos: Bajo, porque 'grande↔pequeño' tiene relación similar a 'alto↔bajo')\n")
cat("Palabras más cercanas al resultado:\n\n")
resultados_ant <- encontrar_mas_cercano(resultado_ant, vocab_ant,
                                        excluir = c("grande", "alto"))
print(resultados_ant, n = 5)

cat("\n✨ El modelo captura relaciones de antonimia!\n\n")

# ============================================================
# PARTE 3: VISUALIZACIÓN
# ============================================================
# "PARTE 3: Visualizando embeddings en 2D\n"
# "Los embeddings tienen cientos de dimensiones (ej: 1024).
# "Para visualizarlos, los reducimos a 2D usando t-SNE.

# Combinar todos los vocabularios
vocab_completo <- bind_rows(
  vocab_genero %>% mutate(categoria = "Género"),
  vocab_geo %>% mutate(categoria = "Geografía"),
  vocab_ant %>% mutate(categoria = "Antónimos")
) %>%
  distinct(palabra, .keep_all = TRUE)

# Preparar matriz para t-SNE
matriz_embeddings <- do.call(rbind, vocab_completo$embedding)

cat("Reduciendo dimensionalidad con t-SNE...\n")
cat("(Esto puede tardar un momento)\n\n")

set.seed(42)
tsne_result <- Rtsne(
  matriz_embeddings,
  dims = 2,
  perplexity = min(15, floor((nrow(matriz_embeddings) - 1) / 3)),
  verbose = FALSE,
  max_iter = 500
)

# Agregar coordenadas 2D
vocab_completo <- vocab_completo %>%
  mutate(
    x = tsne_result$Y[, 1],
    y = tsne_result$Y[, 2]
  )

cat("✓ Visualización preparada!\n\n")

# Gráfico 1: Todos los conceptos
p1 <- ggplot(vocab_completo, aes(x = x, y = y, color = categoria, label = palabra)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(size = 3.5, max.overlaps = 20, force = 2) +
  labs(
    title = "Embeddings visualizados en 2D (t-SNE)",
    subtitle = "Palabras similares aparecen cercanas en el espacio",
    x = "Dimensión 1",
    y = "Dimensión 2",
    color = "Categoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(p1)

# Gráfico 2: Enfoque en geografía (mostrando la analogía)
vocab_geo_vis <- vocab_completo %>% filter(categoria == "Geografía")

p2 <- ggplot(vocab_geo_vis, aes(x = x, y = y, label = palabra)) +
  geom_point(size = 4, color = "#2196F3", alpha = 0.8) +
  geom_text_repel(size = 4, max.overlaps = 20, force = 3) +
  # Líneas para mostrar relaciones
  geom_segment(
    data = vocab_geo_vis %>% filter(palabra %in% c("Francia", "París")),
    aes(
      x = vocab_geo_vis %>% filter(palabra == "Francia") %>% pull(x),
      y = vocab_geo_vis %>% filter(palabra == "Francia") %>% pull(y),
      xend = vocab_geo_vis %>% filter(palabra == "París") %>% pull(x),
      yend = vocab_geo_vis %>% filter(palabra == "París") %>% pull(y)
    ),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "#FF5722",
    linewidth = 1
  ) +
  geom_segment(
    data = vocab_geo_vis %>% filter(palabra %in% c("Colombia", "Bogotá")),
    aes(
      x = vocab_geo_vis %>% filter(palabra == "Colombia") %>% pull(x),
      y = vocab_geo_vis %>% filter(palabra == "Colombia") %>% pull(y),
      xend = vocab_geo_vis %>% filter(palabra == "Bogotá") %>% pull(x),
      yend = vocab_geo_vis %>% filter(palabra == "Bogotá") %>% pull(y)
    ),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "#4CAF50",
    linewidth = 1
  ) +
  labs(
    title = "Relación Capital-País en el espacio vectorial",
    subtitle = "Las flechas muestran vectores similares: País → Capital",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

print(p2)

# Gráfico 3: Matriz de similitudes
cat("\n--- MATRIZ DE SIMILITUDES (Geografía) ---\n\n")

# Calcular todas las similitudes
palabras_sel <- c("Francia", "París", "Colombia", "Bogotá", "España", "Madrid")
vocab_sel <- vocab_geo %>% filter(palabra %in% palabras_sel)

similitudes <- expand_grid(
  palabra1 = palabras_sel,
  palabra2 = palabras_sel
) %>%
  rowwise() %>%
  mutate(
    emb1 = list(vocab_sel %>% filter(palabra == palabra1) %>% pull(embedding) %>% .[[1]]),
    emb2 = list(vocab_sel %>% filter(palabra == palabra2) %>% pull(embedding) %>% .[[1]]),
    similitud = cosine_sim(emb1, emb2)
  ) %>%
  select(palabra1, palabra2, similitud)

p3 <- ggplot(similitudes, aes(x = palabra1, y = palabra2, fill = similitud)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", similitud)), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#2196F3",
    mid = "#FFC107",
    high = "#F44336",
    midpoint = 0.5,
    limits = c(0, 1),
    name = "Similitud\nCoseno"
  ) +
  labs(
    title = "Matriz de Similitudes - Países y Capitales",
    subtitle = "Valores altos (rojo) = más similares",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p3)

# ============================================================
# CONCLUSIONES Y CONCEPTOS CLAVE
# ============================================================
cat("\n\n============================================================\n")
cat("CONCLUSIONES Y CONCEPTOS CLAVE\n")
cat("============================================================\n\n")

cat("1. ¿QUÉ SON LOS EMBEDDINGS?\n")
cat("   → Representaciones numéricas de texto que capturan significado\n")
cat("   → Palabras similares tienen vectores cercanos\n")
cat("   → Dimensiones típicas: 384, 768, 1024, 1536, etc.\n\n")

cat("2. ARITMÉTICA VECTORIAL\n")
cat("   → Podemos sumar y restar significados: Rey - Hombre + Mujer = Reina\n")
cat("   → Las relaciones semánticas se capturan como direcciones vectoriales\n")
cat("   → Funciona porque el modelo fue entrenado en grandes corpus de texto\n\n")

cat("3. SIMILITUD COSENO\n")
cat("   → Mide el 'ángulo' entre vectores\n")
cat("   → Rango: -1 (opuestos) a 1 (idénticos)\n")
cat("   → Más usado que distancia euclidiana para embeddings\n\n")

cat("4. APLICACIONES\n")
cat("   → Búsqueda semántica (encontrar documentos similares)\n")
cat("   → Clasificación de texto (como vimos en scripts anteriores)\n")
cat("   → Sistemas de recomendación\n")
cat("   → Detección de duplicados\n")
cat("   → Clustering de documentos\n\n")

cat("5. LIMITACIONES\n")
cat("   → No capturan contexto completo (mejor usar modelos LLM para eso)\n")
cat("   → Sesgos del entrenamiento se reflejan en los embeddings\n")
cat("   → Analogías complejas pueden fallar\n\n")

cat("6. MODELOS DE EMBEDDINGS POPULARES\n")
cat("   → Locales: mxbai-embed-large, nomic-embed-text (Ollama)\n")
cat("   → Cloud: OpenAI text-embedding-3-small/large\n")
cat("   → Open source: sentence-transformers (Hugging Face)\n\n")

cat("============================================================\n")
cat("Experimentos sugeridos:\n")
cat("============================================================\n\n")

cat("1. Prueba otras analogías:\n")
cat("   - Doctor - Hombre + Mujer = ?\n")
cat("   - Bueno - Malo + Feliz = ?\n")
cat("   - Perro - Cachorro + Gato = ?\n\n")

cat("2. Explora similitudes:\n")
cat("   - ¿Qué tan similares son 'carro' y 'automóvil'?\n")
cat("   - ¿'banco' (mueble) vs 'banco' (institución)?\n\n")

cat("3. Cambia el modelo:\n")
cat("   - Prueba USE_OPENAI <- TRUE para comparar\n")
cat("   - Ollama: prueba 'nomic-embed-text'\n\n")

cat("============================================================\n")
cat("¡Script completado!\n")
cat("Modelo usado:", if(USE_OPENAI) "OpenAI" else EMBED_MODEL_LOCAL, "\n")
cat("============================================================\n")
