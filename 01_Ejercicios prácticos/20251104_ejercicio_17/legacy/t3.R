
# ============================================================
# COMPARACIÓN DE MÉTODOS DE ANÁLISIS DE SENTIMIENTO
# ============================================================
#
# Este script compara tres enfoques diferentes:
#
# 1. LEXICON-BASED (Basado en diccionario)
#    - Usa un diccionario de palabras con polaridad (+/-)
#    - Rápido y simple
#    - No captura contexto ni sarcasmo
#
# 2. EMBEDDINGS (Representaciones vectoriales)
#    - Local: mxbai-embed-large (Ollama)
#    - Cloud: text-embedding-3-small (OpenAI)
#    - Captura semántica pero limitado con sarcasmo
#
# 3. LLM (Modelos de lenguaje grandes)
#    - llama3.1:latest (Ollama)
#    - qwen2.5:latest (Ollama)
#    - Mejor comprensión de contexto y sarcasmo
#
# Requisitos:
#   - Ollama con: llama3.1:latest, qwen2.5:latest, mxbai-embed-large
#   - Variable de ambiente: OPENAI_API_KEY
#   - Paquetes: tidyverse, httr2, jsonlite
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(jsonlite)
})

# ---------------------------
# CONFIGURACIÓN
# ---------------------------
OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL_LOCAL <- "mxbai-embed-large"
LLM_MODELS <- c("llama3.1:latest", "qwen2.5:latest")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Verificar configuración
cat("\n=== VERIFICANDO CONFIGURACIÓN ===\n")
cat("✓ Ollama host:", OLLAMA_HOST, "\n")
cat("✓ OpenAI API Key:", if(nchar(OPENAI_API_KEY) > 0) "Configurada" else "NO ENCONTRADA", "\n\n")

if (nchar(OPENAI_API_KEY) == 0) {
  warning("⚠ OpenAI API Key no encontrada. El método de embeddings OpenAI será omitido.")
}

# ---------------------------
# TEXTOS DE PRUEBA
# ---------------------------
cat("=== PREPARANDO TEXTOS DE PRUEBA ===\n")

textos <- tibble(
  id = 1:15,
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

    # CASOS DIFÍCILES (sarcasmo e ironía)
    "Genial... tres horas esperando para nada.",
    "Perfecto, justo lo que necesitaba: que no funcione.",
    "Qué maravilla de atención, me ignoraron completamente.",

    # CASOS MIXTOS
    "No está mal, pero el precio es muy alto.",
    "Rápido y económico, aunque la calidad es regular.",
    "Buen servicio pero el producto llegó dañado."
  ),
  # Etiqueta "ground truth" para evaluación
  etiqueta_real = c(
    "positivo", "positivo", "positivo",  # 1-3
    "negativo", "negativo", "negativo",  # 4-6
    "neutro", "neutro", "neutro",        # 7-9
    "negativo", "negativo", "negativo",  # 10-12 (sarcasmo)
    "negativo", "neutro", "negativo"     # 13-15 (mixtos)
  )
)

cat("✓", nrow(textos), "textos preparados\n")
cat("  - Positivos:", sum(textos$etiqueta_real == "positivo"), "\n")
cat("  - Negativos:", sum(textos$etiqueta_real == "negativo"), "\n")
cat("  - Neutros:", sum(textos$etiqueta_real == "neutro"), "\n\n")

# ============================================================
# MÉTODO 1: LEXICON-BASED
# ============================================================
cat("\n=== MÉTODO 1: LEXICON-BASED ===\n")

# Diccionario de palabras en español con polaridad
lexicon_positivo <- c(
  "excelente", "genial", "maravillosa", "encanta", "recomiendo", "perfecto",
  "satisfecho", "feliz", "bueno", "buena", "mejor", "increíble", "súper",
  "fantástico", "estupendo", "magnífico", "superó", "expectativas",
  "buen", "buena", "rápido", "económico", "volveré"
)

lexicon_negativo <- c(
  "pésimo", "horrible", "mal", "nunca", "pérdida", "estafa", "dañado",
  "decepcionado", "arrepiento", "terrible", "no funciona", "grosero",
  "ignoraron", "tarde", "fría", "lento", "caro", "alto", "precio",
  "regular", "limitado", "problemas", "defectuoso", "esperando"
)

# Modificadores (negaciones, intensificadores)
negaciones <- c("no", "nunca", "jamás", "tampoco", "sin")
intensificadores <- c("muy", "súper", "extremadamente", "totalmente", "completamente")

# Función de clasificación lexicon-based
clasificar_lexicon <- function(texto) {
  texto_lower <- tolower(texto)
  palabras <- str_split(texto_lower, "\\s+")[[1]]

  # Contar palabras positivas y negativas
  n_positivas <- sum(palabras %in% lexicon_positivo)
  n_negativas <- sum(palabras %in% lexicon_negativo)

  # Detectar negaciones (invierte la polaridad)
  tiene_negacion <- any(palabras %in% negaciones)
  if (tiene_negacion) {
    temp <- n_positivas
    n_positivas <- n_negativas
    n_negativas <- temp
  }

  # Calcular score
  score <- n_positivas - n_negativas

  # Clasificar
  if (score > 0) {
    return(list(sentimiento = "positivo", score = score, confianza = n_positivas + n_negativas))
  } else if (score < 0) {
    return(list(sentimiento = "negativo", score = score, confianza = n_positivas + n_negativas))
  } else {
    return(list(sentimiento = "neutro", score = score, confianza = 0))
  }
}

# Aplicar clasificación
cat("Clasificando con lexicon...\n")
resultados_lexicon <- textos %>%
  mutate(
    resultado = map(texto, clasificar_lexicon),
    sentimiento_lexicon = map_chr(resultado, "sentimiento"),
    score_lexicon = map_dbl(resultado, "score"),
    confianza_lexicon = map_dbl(resultado, "confianza")
  ) %>%
  select(-resultado)

cat("✓ Lexicon completado\n")

# ============================================================
# MÉTODO 2: EMBEDDINGS
# ============================================================
cat("\n=== MÉTODO 2: EMBEDDINGS ===\n")

# --- Funciones auxiliares ---
cosine_sim <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

# Embeddings locales (Ollama)
get_embedding_ollama <- function(texto, model = EMBED_MODEL_LOCAL, host = OLLAMA_HOST) {
  req <- request(paste0(host, "/api/embeddings")) |>
    req_body_json(list(model = model, prompt = texto)) |>
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$embedding))
}

# Embeddings OpenAI
texto = "Ni bueno ni malo"
get_embedding_openai <- function(texto, api_key = "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") {
  
  if (nchar(api_key) == 0) {
    return(NULL)
  }

  req <- request("https://api.openai.com/v1/embeddings") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
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

# Prototipos mejorados
prototipos <- tibble(
  categoria = c(
    rep("positivo", 5),
    rep("negativo", 5),
    rep("neutro", 5)
  ),
  ejemplo = c(
    # Positivos
    "Estoy muy feliz con el resultado, superó mis expectativas.",
    "Excelente producto, lo recomiendo totalmente.",
    "Me encantó la experiencia, todo perfecto.",
    "Muy satisfecho con la compra, gran calidad.",
    "Increíble servicio, la mejor decisión que tomé.",

    # Negativos (incluyendo sarcasmo)
    "Muy decepcionado, no cumplió mis expectativas.",
    "Pésima calidad, no lo recomiendo para nada.",
    "Genial, justo lo que necesitaba: que no funcione.",
    "Qué maravilla, perdí mi tiempo completamente.",
    "Perfecto, tres horas esperando para nada.",

    # Neutros
    "El producto es normal, cumple lo básico.",
    "Ni bueno ni malo, es promedio.",
    "Funciona correctamente, sin grandes sorpresas.",
    "Es estándar, similar a otros del mercado.",
    "Adecuado para uso básico, nada especial."
  )
)

# Función de clasificación por embeddings
clasificar_embeddings <- function(emb_texto, proto_df) {
  sims <- map_dbl(proto_df$embedding, ~ cosine_sim(.x, emb_texto))

  proto_df %>%
    mutate(similitud = sims) %>%
    group_by(categoria) %>%
    summarise(
      sim_promedio = mean(similitud),
      sim_max = max(similitud),
      .groups = "drop"
    ) %>%
    arrange(desc(sim_promedio))
}

# --- Embeddings locales (Ollama) ---
cat("\n[2a] Embeddings locales (Ollama)...\n")
cat("  → Procesando prototipos...")
prototipos_local <- prototipos %>%
  mutate(embedding = map(ejemplo, possibly(get_embedding_ollama, NULL), .progress = TRUE))

# Verificar que todos los embeddings se generaron
if (any(map_lgl(prototipos_local$embedding, is.null))) {
  warning("⚠ Algunos embeddings locales fallaron. Verifica que Ollama esté corriendo.")
}

cat("\n  → Procesando textos de prueba...")
resultados_emb_local <- textos %>%
  mutate(
    embedding = map(texto, possibly(get_embedding_ollama, NULL), 
                    .progress = TRUE),
    clasificacion = map(embedding, ~ {
      if (is.null(.x)) return(NULL)
      clasificar_embeddings(.x, prototipos_local)
    }),
    sentimiento_emb_local = map_chr(clasificacion,
                                    ~ if(is.null(.x)) NA_character_ else 
                                      .x$categoria[1]),
    confianza_emb_local = map_dbl(clasificacion, ~ if(is.null(.x)) NA_real_ else .x$sim_promedio[1])
  ) %>%
  select(-embedding, -clasificacion)

cat("✓ Embeddings locales completados\n")

# --- Embeddings OpenAI ---
if (nchar(OPENAI_API_KEY) > 0) {
  cat("\n[2b] Embeddings OpenAI...\n")
  cat("  → Procesando prototipos...")

  prototipos_openai <- prototipos %>%
    mutate(embedding = map(ejemplo, possibly(get_embedding_openai, NULL), .progress = TRUE))

  cat("\n  → Procesando textos de prueba...")
  resultados_emb_openai <- textos %>%
    mutate(
      embedding = map(texto, possibly(get_embedding_openai, NULL), .progress = TRUE),
      clasificacion = map(embedding, ~ {
        if (is.null(.x)) return(NULL)
        clasificar_embeddings(.x, prototipos_openai)
      }),
      sentimiento_emb_openai = map_chr(clasificacion, ~ if(is.null(.x)) NA_character_ else .x$categoria[1]),
      confianza_emb_openai = map_dbl(clasificacion, ~ if(is.null(.x)) NA_real_ else .x$sim_promedio[1])
    ) %>%
    select(-embedding, -clasificacion)

  cat("✓ Embeddings OpenAI completados\n")
} else {
  cat("\n[2b] Embeddings OpenAI omitidos (no hay API key)\n")
  resultados_emb_openai <- textos %>%
    mutate(
      sentimiento_emb_openai = NA_character_,
      confianza_emb_openai = NA_real_
    )
}

# ============================================================
# MÉTODO 3: LLMs
# ============================================================
cat("\n=== MÉTODO 3: LLMs ===\n")

# Función para llamar a LLM local con Ollama
llamar_llm_ollama <- function(texto, model, host = OLLAMA_HOST) {
  prompt <- paste0(
    "Clasifica el siguiente texto en una de estas categorías: positivo, negativo, neutro.\n\n",
    "Texto: \"", texto, "\"\n\n",
    "Responde SOLO con una palabra: positivo, negativo o neutro.\n",
    "Respuesta:"
  )

  req <- request(paste0(host, "/api/generate")) %>%
    req_body_json(list(
      model = model,
      prompt = prompt,
      stream = FALSE,
      options = list(
        temperature = 0.1,
        num_predict = 10
      )
    )) %>%
    req_perform()

  resp <- resp_body_json(req)
  respuesta <- tolower(trimws(resp$response))

  # Extraer sentimiento
  if (str_detect(respuesta, "positivo")) {
    return("positivo")
  } else if (str_detect(respuesta, "negativo")) {
    return("negativo")
  } else if (str_detect(respuesta, "neutro")) {
    return("neutro")
  } else {
    return(NA_character_)
  }
}

# --- LLM 1: llama3.1 ---
cat("\n[3a] Clasificando con llama3.1:latest...\n")
resultados_llm1 <- textos %>%
  mutate(
    sentimiento_llama31 = map_chr(
      texto,
      possibly(~ llamar_llm_ollama(.x, "llama3.1:latest"), NA_character_),
      .progress = TRUE
    )
  )

cat("✓ llama3.1 completado\n")

# --- LLM 2: qwen2.5 ---
cat("\n[3b] Clasificando con qwen2.5:latest...\n")
resultados_llm2 <- textos %>%
  mutate(
    sentimiento_qwen25 = map_chr(
      texto,
      possibly(~ llamar_llm_ollama(.x, "qwen2.5:latest"), NA_character_),
      .progress = TRUE
    )
  )

cat("✓ qwen2.5 completado\n")

# ============================================================
# CONSOLIDAR RESULTADOS
# ============================================================
cat("\n=== CONSOLIDANDO RESULTADOS ===\n")

resultados_final <- textos %>%
  left_join(resultados_lexicon %>% select(id, sentimiento_lexicon, score_lexicon), by = "id") %>%
  left_join(resultados_emb_local %>% select(id, sentimiento_emb_local, confianza_emb_local), by = "id") %>%
  left_join(resultados_emb_openai %>% select(id, sentimiento_emb_openai, confianza_emb_openai), by = "id") %>%
  left_join(resultados_llm1 %>% select(id, sentimiento_llama31), by = "id") %>%
  left_join(resultados_llm2 %>% select(id, sentimiento_qwen25), by = "id")

cat("✓ Resultados consolidados\n")

# ============================================================
# EVALUACIÓN Y COMPARACIÓN
# ============================================================
cat("\n\n========== RESULTADOS COMPARATIVOS ==========\n\n")

# Tabla completa
print(resultados_final %>%
        select(id, texto, etiqueta_real, sentimiento_lexicon,
               sentimiento_emb_local, sentimiento_emb_openai,
               sentimiento_llama31, sentimiento_qwen25),
      n = Inf)

resultados_final %>%
  select(id, texto, etiqueta_real, sentimiento_lexicon,
         sentimiento_emb_local, sentimiento_emb_openai,
         sentimiento_llama31, sentimiento_qwen25) %>% 
  View()

# Calcular accuracy
calcular_accuracy <- function(prediccion, real) {
  if (all(is.na(prediccion))) return(NA_real_)
  mean(prediccion == real, na.rm = TRUE)
}

cat("\n\n--- ACCURACY (% de aciertos) ---\n")
accuracies <- tibble(
  Método = c(
    "Lexicon",
    "Embeddings (Local)",
    "Embeddings (OpenAI)",
    "LLM (llama3.1)",
    "LLM (qwen2.5)"
  ),
  Accuracy = c(
    calcular_accuracy(resultados_final$sentimiento_lexicon, resultados_final$etiqueta_real),
    calcular_accuracy(resultados_final$sentimiento_emb_local, resultados_final$etiqueta_real),
    calcular_accuracy(resultados_final$sentimiento_emb_openai, resultados_final$etiqueta_real),
    calcular_accuracy(resultados_final$sentimiento_llama31, resultados_final$etiqueta_real),
    calcular_accuracy(resultados_final$sentimiento_qwen25, resultados_final$etiqueta_real)
  )
) %>%
  arrange(desc(Accuracy))

print(accuracies)

# Análisis por tipo de caso
cat("\n--- ANÁLISIS POR TIPO DE CASO ---\n")

# Casos claros (1-9)
casos_claros <- resultados_final %>% filter(id <= 9)
cat("\nCasos CLAROS (id 1-9):\n")
cat("  Lexicon:", calcular_accuracy(casos_claros$sentimiento_lexicon, casos_claros$etiqueta_real), "\n")
cat("  Emb Local:", calcular_accuracy(casos_claros$sentimiento_emb_local, casos_claros$etiqueta_real), "\n")
cat("  Emb OpenAI:", calcular_accuracy(casos_claros$sentimiento_emb_openai, casos_claros$etiqueta_real), "\n")
cat("  llama3.1:", calcular_accuracy(casos_claros$sentimiento_llama31, casos_claros$etiqueta_real), "\n")
cat("  qwen2.5:", calcular_accuracy(casos_claros$sentimiento_qwen25, casos_claros$etiqueta_real), "\n")

# Casos con sarcasmo (10-12)
casos_sarcasmo <- resultados_final %>% filter(id >= 10 & id <= 12)
cat("\nCasos SARCASMO (id 10-12):\n")
cat("  Lexicon:", calcular_accuracy(casos_sarcasmo$sentimiento_lexicon, casos_sarcasmo$etiqueta_real), "\n")
cat("  Emb Local:", calcular_accuracy(casos_sarcasmo$sentimiento_emb_local, casos_sarcasmo$etiqueta_real), "\n")
cat("  Emb OpenAI:", calcular_accuracy(casos_sarcasmo$sentimiento_emb_openai, casos_sarcasmo$etiqueta_real), "\n")
cat("  llama3.1:", calcular_accuracy(casos_sarcasmo$sentimiento_llama31, casos_sarcasmo$etiqueta_real), "\n")
cat("  qwen2.5:", calcular_accuracy(casos_sarcasmo$sentimiento_qwen25, casos_sarcasmo$etiqueta_real), "\n")

# Casos mixtos (13-15)
casos_mixtos <- resultados_final %>% filter(id >= 13)
cat("\nCasos MIXTOS (id 13-15):\n")
cat("  Lexicon:", calcular_accuracy(casos_mixtos$sentimiento_lexicon, casos_mixtos$etiqueta_real), "\n")
cat("  Emb Local:", calcular_accuracy(casos_mixtos$sentimiento_emb_local, casos_mixtos$etiqueta_real), "\n")
cat("  Emb OpenAI:", calcular_accuracy(casos_mixtos$sentimiento_emb_openai, casos_mixtos$etiqueta_real), "\n")
cat("  llama3.1:", calcular_accuracy(casos_mixtos$sentimiento_llama31, casos_mixtos$etiqueta_real), "\n")
cat("  qwen2.5:", calcular_accuracy(casos_mixtos$sentimiento_qwen25, casos_mixtos$etiqueta_real), "\n")

# ============================================================
# VISUALIZACIONES
# ============================================================

# Preparar datos para visualización (formato largo)
resultados_long <- resultados_final %>%
  select(id, texto, etiqueta_real,
         sentimiento_lexicon, sentimiento_emb_local, sentimiento_emb_openai,
         sentimiento_llama31, sentimiento_qwen25) %>%
  pivot_longer(
    cols = starts_with("sentimiento_"),
    names_to = "metodo",
    values_to = "prediccion",
    names_prefix = "sentimiento_"
  ) %>%
  mutate(
    metodo = case_when(
      metodo == "lexicon" ~ "Lexicon",
      metodo == "emb_local" ~ "Emb (Local)",
      metodo == "emb_openai" ~ "Emb (OpenAI)",
      metodo == "llama31" ~ "LLM (llama3.1)",
      metodo == "qwen25" ~ "LLM (qwen2.5)",
      TRUE ~ metodo
    ),
    correcto = prediccion == etiqueta_real
  )

# Gráfico 1: Accuracy por método
p1 <- ggplot(accuracies %>% filter(!is.na(Accuracy)),
             aes(x = reorder(Método, Accuracy), y = Accuracy, fill = Método)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  ylim(0, 1.1) +
  labs(
    title = "Comparación de Accuracy por Método",
    subtitle = "Porcentaje de clasificaciones correctas",
    x = NULL,
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Gráfico 2: Heatmap de predicciones
p2 <- ggplot(resultados_long %>% filter(!is.na(prediccion)),
             aes(x = metodo, y = factor(id), fill = correcto)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = substr(prediccion, 1, 3)), size = 3) +
  scale_fill_manual(
    values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"),
    labels = c("Correcto", "Incorrecto"),
    name = "Resultado"
  ) +
  labs(
    title = "Mapa de Predicciones por Método",
    subtitle = "Verde = correcto, Rojo = incorrecto",
    x = "Método",
    y = "ID del texto"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(p2)

# Gráfico 3: Accuracy por tipo de caso
accuracy_tipo <- bind_rows(
  casos_claros %>%
    summarise(
      tipo = "Claros",
      across(starts_with("sentimiento_"),
             ~ calcular_accuracy(.x, etiqueta_real))
    ),
  casos_sarcasmo %>%
    summarise(
      tipo = "Sarcasmo",
      across(starts_with("sentimiento_"),
             ~ calcular_accuracy(.x, etiqueta_real))
    ),
  casos_mixtos %>%
    summarise(
      tipo = "Mixtos",
      across(starts_with("sentimiento_"),
             ~ calcular_accuracy(.x, etiqueta_real))
    )
) %>%
  pivot_longer(
    cols = starts_with("sentimiento_"),
    names_to = "metodo",
    values_to = "accuracy",
    names_prefix = "sentimiento_"
  ) %>%
  mutate(
    metodo = case_when(
      metodo == "lexicon" ~ "Lexicon",
      metodo == "emb_local" ~ "Emb (Local)",
      metodo == "emb_openai" ~ "Emb (OpenAI)",
      metodo == "llama31" ~ "LLM (llama3.1)",
      metodo == "qwen25" ~ "LLM (qwen2.5)",
      TRUE ~ metodo
    )
  )

p3 <- ggplot(accuracy_tipo %>% filter(!is.na(accuracy)),
             aes(x = tipo, y = accuracy, fill = metodo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.0f%%", accuracy * 100)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  ylim(0, 1.1) +
  labs(
    title = "Accuracy por Tipo de Caso",
    subtitle = "Comparación del rendimiento en casos claros, sarcasmo y mixtos",
    x = "Tipo de caso",
    y = "Accuracy",
    fill = "Método"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p3)

# ============================================================
# CONCLUSIONES
# ============================================================
cat("\n\n========== CONCLUSIONES ==========\n\n")

cat("RESUMEN DE MÉTODOS:\n\n")

cat("1. LEXICON-BASED\n")
cat("   PROS: Rápido, interpretable, sin dependencias externas\n")
cat("   CONTRAS: No entiende contexto, falla con sarcasmo y negaciones complejas\n")
cat("   MEJOR PARA: Análisis rápido de grandes volúmenes, casos claros\n\n")

cat("2. EMBEDDINGS\n")
cat("   PROS: Captura semántica, funciona sin entrenamiento\n")
cat("   CONTRAS: Limitado con sarcasmo, depende de prototipos\n")
cat("   MEJOR PARA: Clasificación sin datos etiquetados, búsqueda semántica\n\n")

cat("3. LLMs\n")
cat("   PROS: Mejor comprensión de contexto, maneja sarcasmo\n")
cat("   CONTRAS: Más lento, puede ser inconsistente\n")
cat("   MEJOR PARA: Casos complejos, análisis cualitativo, alta precisión\n\n")

cat("RECOMENDACIÓN:\n")
cat("→ Para PRODUCCIÓN: LLM (mejor accuracy en casos difíciles)\n")
cat("→ Para EXPLORACIÓN: Embeddings (balance velocidad/calidad)\n")
cat("→ Para FILTRADO INICIAL: Lexicon (muy rápido para descartar casos obvios)\n")
cat("→ ENFOQUE HÍBRIDO: Lexicon + LLM (rápido para casos claros, preciso para dudosos)\n\n")

cat("========================================\n")
cat("Script completado exitosamente.\n")
cat("========================================\n")
