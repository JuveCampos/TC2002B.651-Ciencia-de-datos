
# ============================================================
# ANÁLISIS DE SENTIMIENTO CON EMBEDDINGS - Tutorial Didáctico
# ============================================================
#
# ¿QUÉ SON LOS EMBEDDINGS?
# ------------------------
# Los embeddings son representaciones numéricas (vectores) de texto que capturan
# el SIGNIFICADO SEMÁNTICO de las palabras o frases. A diferencia de técnicas
# simples como contar palabras, los embeddings colocan textos similares en
# posiciones cercanas en un espacio vectorial de alta dimensión (ej: 1024 dims).
#
# Ejemplo conceptual:
#   "Me encanta" y "Me gusta mucho" → vectores muy cercanos
#   "Me encanta" y "Es horrible" → vectores muy lejanos
#
# VENTAJAS:
#   ✓ Capturan contexto y matices del lenguaje
#   ✓ Funcionan bien con textos cortos o largos
#   ✓ No requieren entrenamiento (modelos pre-entrenados)
#   ✓ Multilingües en muchos casos
#
# MÉTODO EN ESTE SCRIPT:
#   Usamos MÚLTIPLES PROTOTIPOS por categoría (en vez de uno solo) para
#   representar mejor la diversidad dentro de cada sentimiento.
#   Luego calculamos la similitud promedio con cada categoría.
#
# Requisitos:
#   - Ollama corriendo: `ollama serve`
#   - Modelo: `ollama pull mxbai-embed-large`
#   - Paquetes: tidyverse, httr2, purrr, ggplot2
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(purrr)
  library(ggplot2)
})

# ---------------------------
# CONFIGURACIÓN
# ---------------------------
OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL <- "mxbai-embed-large"

# ---------------------------
# FUNCIONES AUXILIARES
# ---------------------------

# Calcula similitud coseno entre dos vectores
# (Mide el "ángulo" entre vectores: 1 = idénticos, 0 = perpendiculares, -1 = opuestos)
cosine_sim <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

# Obtiene embedding de Ollama para un texto
get_embedding <- function(texto, model = EMBED_MODEL, host = OLLAMA_HOST) {
  req <- request(paste0(host, "/api/embeddings")) |>
    req_body_json(list(model = model, prompt = texto)) |>
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$embedding))
}

# Verificar que Ollama está corriendo
cat("\n[1/6] Verificando conexión con Ollama...\n")
tryCatch({
  request(paste0(OLLAMA_HOST, "/api/tags")) |> req_perform()
  cat("✓ Ollama está corriendo correctamente\n")
}, error = function(e) {
  stop("✗ Error: No se puede conectar a Ollama. ¿Ejecutaste 'ollama serve'?")
})

# ---------------------------
# TEXTOS DE PRUEBA (casos variados)
# ---------------------------
cat("\n[2/6] Preparando textos de prueba...\n")

textos <- tibble(
  id = 1:12,
  texto = c(
    # POSITIVOS claros
    "¡Excelente servicio! Superó mis expectativas completamente.",
    "Me encanta este producto, lo recomiendo 100%.",
    "Maravillosa experiencia, volveré sin duda.",

    # NEGATIVOS claros
    "Pésimo servicio, nunca vuelvo. Una pérdida de tiempo y dinero.",
    "Horrible experiencia, me trataron muy mal.",
    "No funciona, es una estafa total.",

    # NEUTROS
    "Es un producto normal, nada del otro mundo.",
    "Cumple con lo básico, ni bueno ni malo.",
    "Funciona como esperaba, sin sorpresas.",

    # CASOS DIFÍCILES (sarcasmo, mixtos)
    "Genial... tres horas esperando para nada.",  # Sarcasmo negativo
    "No está mal, pero el precio es muy alto.",   # Mixto tirando a negativo
    "Rápido y económico, aunque la calidad es regular."  # Mixto tirando a positivo
  )
)

# ---------------------------
# PROTOTIPOS MEJORADOS (MÚLTIPLES POR CATEGORÍA)
# ---------------------------
# MEJORA CLAVE: Usamos varios ejemplos por categoría para capturar
# mejor la diversidad de expresiones dentro de cada sentimiento

cat("\n[3/6] Definiendo prototipos mejorados...\n")

prototipos <- tibble(
  categoria = c(
    # POSITIVOS (5 variantes)
    "positivo", "positivo", "positivo", "positivo", "positivo",
    # NEGATIVOS (5 variantes)
    "negativo", "negativo", "negativo", "negativo", "negativo",
    # NEUTROS (5 variantes)
    "neutro", "neutro", "neutro", "neutro", "neutro"
  ),
  ejemplo = c(
    # Ejemplos POSITIVOS variados
    "Estoy muy feliz con el resultado, superó mis expectativas.",
    "Excelente producto, lo recomiendo totalmente.",
    "Me encantó la experiencia, todo perfecto.",
    "Muy satisfecho con la compra, gran calidad.",
    "Increíble servicio, la mejor decisión que tomé.",

    # Ejemplos NEGATIVOS variados
    "Muy decepcionado, no cumplió mis expectativas.",
    "Pésima calidad, no lo recomiendo para nada.",
    "Horrible experiencia, perdí mi tiempo y dinero.",
    "Me arrepiento de la compra, muy mal servicio.",
    "Terrible producto, no funciona como prometen.",

    # Ejemplos NEUTROS variados
    "El producto es normal, cumple lo básico.",
    "Ni bueno ni malo, es promedio.",
    "Funciona correctamente, sin grandes sorpresas.",
    "Es estándar, similar a otros del mercado.",
    "Adecuado para uso básico, nada especial."
  )
)

cat("✓ Prototipos definidos:",
    sum(prototipos$categoria == "positivo"), "positivos,",
    sum(prototipos$categoria == "negativo"), "negativos,",
    sum(prototipos$categoria == "neutro"), "neutros\n")

# ---------------------------
# GENERAR EMBEDDINGS
# ---------------------------
cat("\n[4/6] Generando embeddings (esto puede tardar un momento)...\n")

# Embeddings de prototipos
cat("  → Procesando", nrow(prototipos), "prototipos...")
prototipos <- prototipos |>
  mutate(embedding = map(ejemplo, get_embedding, .progress = TRUE))

# Embeddings de textos a clasificar
cat("\n  → Procesando", nrow(textos), "textos de prueba...")
textos <- textos |>
  mutate(embedding = map(texto, get_embedding, .progress = TRUE))

cat("\n✓ Embeddings generados (dimensión:", length(prototipos$embedding[[1]]), ")\n")

# ---------------------------
# CLASIFICACIÓN MEJORADA (Promedio de similitudes por categoría)
# ---------------------------
cat("\n[5/6] Clasificando textos...\n")

# Para cada texto, calculamos similitud con TODOS los prototipos
# y luego promediamos por categoría
clasificar_texto <- function(emb_texto, proto_df) {
  # Calcular similitud con cada prototipo
  sims <- map_dbl(proto_df$embedding, ~ cosine_sim(.x, emb_texto))

  # Agregar a la tabla de prototipos
  proto_df |>
    mutate(similitud = sims) |>
    group_by(categoria) |>
    # CLAVE: Promediamos las similitudes dentro de cada categoría
    summarise(
      sim_promedio = mean(similitud),
      sim_max = max(similitud),
      .groups = "drop"
    ) |>
    arrange(desc(sim_promedio))
}

# Aplicar clasificación a todos los textos
resultados <- textos |>
  mutate(
    clasificacion = map(embedding, ~ clasificar_texto(.x, prototipos))
  ) |>
  mutate(
    sentimiento = map_chr(clasificacion, ~ .x$categoria[1]),
    confianza = map_dbl(clasificacion, ~ .x$sim_promedio[1]),
    segunda_opcion = map_chr(clasificacion, ~ .x$categoria[2]),
    diferencia = map_dbl(clasificacion, ~ .x$sim_promedio[1] - .x$sim_promedio[2])
  )

usethis::edit_r_environ()

# ---------------------------
# RESULTADOS Y ANÁLISIS
# ---------------------------
cat("\n[6/6] ========== RESULTADOS ==========\n\n")

# Tabla resumen
resultado_limpio <- resultados |>
  select(id, texto, sentimiento, confianza, segunda_opcion, diferencia)

print(resultado_limpio, n = Inf)

# Estadísticas
cat("\n--- ESTADÍSTICAS ---\n")
cat("Distribución de sentimientos:\n")
print(table(resultados$sentimiento))

cat("\nConfianza promedio por sentimiento:\n")
resultados |>
  group_by(sentimiento) |>
  summarise(confianza_promedio = mean(confianza)) |>
  print()

# Casos ambiguos (diferencia pequeña entre primera y segunda opción)
cat("\n--- CASOS AMBIGUOS (diferencia < 0.05) ---\n")
ambiguos <- resultados |>
  filter(diferencia < 0.05) |>
  select(id, texto, sentimiento, segunda_opcion, diferencia)

if (nrow(ambiguos) > 0) {
  print(ambiguos, n = Inf)
} else {
  cat("No hay casos ambiguos.\n")
}

# ---------------------------
# VISUALIZACIONES
# ---------------------------

# Gráfico 1: Confianza por texto
p1 <- ggplot(resultados,
             aes(x = reorder(id, confianza),
                 y = confianza,
                 fill = sentimiento)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Confianza en la clasificación por texto",
    subtitle = "Método: Promedio de similitud con múltiples prototipos",
    x = "ID del texto",
    y = "Confianza (similitud promedio)",
    fill = "Sentimiento"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  ))

print(p1)

# Gráfico 2: Diferencia entre primera y segunda opción (certeza)
p2 <- ggplot(resultados,
             aes(x = reorder(id, diferencia),
                 y = diferencia,
                 fill = sentimiento)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 1, y = 0.055, label = "Umbral ambigüedad",
           hjust = 0, size = 3, color = "red") +
  labs(
    title = "Certeza de la clasificación",
    subtitle = "Diferencia entre la primera y segunda opción más probable",
    x = "ID del texto",
    y = "Diferencia de similitud",
    fill = "Sentimiento"
  ) +
  theme_minimal(base_size = 11) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  ))

print(p2)

# ---------------------------
# RECOMENDACIONES Y CONCLUSIONES
# ---------------------------
cat("\n=== ANÁLISIS Y RECOMENDACIONES ===\n\n")

cat("MEJORAS IMPLEMENTADAS respecto al script original:\n")
cat("1. ✓ Múltiples prototipos por categoría (5 en vez de 1)\n")
cat("2. ✓ Promedio de similitudes para más robustez\n")
cat("3. ✓ Métricas de confianza y detección de casos ambiguos\n")
cat("4. ✓ Prototipos con ejemplos reales (no descripciones genéricas)\n\n")

cat("¿ES ESTE EL MEJOR ENFOQUE?\n")
cat("PROS:\n")
cat("  + Zero-shot: no requiere datos etiquetados\n")
cat("  + Interpretable: puedes ver por qué clasifica así\n")
cat("  + Flexible: fácil agregar nuevas categorías\n")
cat("  + Multilingüe: funciona en varios idiomas\n\n")

cat("CONTRAS:\n")
cat("  - Precisión limitada vs. modelos supervisados\n")
cat("  - Problemas con sarcasmo e ironía\n")
cat("  - Depende de la calidad de los prototipos\n\n")

cat("ALTERNATIVAS PARA MEJORAR AÚN MÁS:\n")
cat("1. Few-shot learning: Usar ejemplos etiquetados + k-NN\n")
cat("2. Modelos LLM: Llamar a un modelo de chat (llama3, mistral)\n")
cat("3. Fine-tuning: Entrenar un clasificador sobre los embeddings\n")
cat("4. Ensemble: Combinar embeddings con reglas o léxicos\n\n")

cat("CUÁNDO USAR EMBEDDINGS:\n")
cat("→ Cuando no tienes datos etiquetados\n")
cat("→ Cuando necesitas clasificación rápida y simple\n")
cat("→ Como baseline para comparar con otros métodos\n")
cat("→ Para clustering o búsqueda semántica\n\n")

cat("========================================\n")
cat("Script completado exitosamente.\n")
cat("Modelo usado:", EMBED_MODEL, "\n")
cat("Dimensión de embeddings:", length(textos$embedding[[1]]), "\n")
cat("========================================\n")
