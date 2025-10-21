# ============================================================================
# Optimización del Número de Tópicos - Artículos Juvenal Campos
# Métodos: Perplejidad, Coherencia y Log-Likelihood
# ============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(tidytext)
library(readxl)
library(topicmodels)
library(tm)
library(ggplot2)

# ----------------------------------------------------------------------------
# 1. CARGAR Y PREPROCESAR DATOS
# ----------------------------------------------------------------------------

cat("=== PREPARANDO DATOS ===\n\n")

# Leer el archivo Excel con los artículos
articulos <- read_excel("01_Datos/tabla_archivos_juve.xlsx")

# Crear un ID único para cada artículo
articulos <- articulos %>%
  mutate(doc_id = row_number())

# Tokenización
tokens <- articulos %>%
  select(doc_id, titulos, textos) %>%
  unnest_tokens(word, textos)

# Cargar stopwords en español
stopwords_es <- stopwords::stopwords("es", source = "stopwords-iso")

# Agregar stopwords personalizadas
stopwords_custom <- c(stopwords_es,
                     "si", "así", "ser", "hacer", "puede", "pueden",
                     "vez", "través", "año", "años", "día", "días",
                     "aquí", "allí", "ahí", "tan", "tal", "también",
                     "bien", "sí", "no", "cómo", "dónde", "cuándo")

# Limpiar tokens
tokens_limpios <- tokens %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) >= 3) %>%
  filter(!word %in% stopwords_custom)

# Contar palabras por documento
doc_words <- tokens_limpios %>%
  count(doc_id, word, sort = TRUE)

# Crear DTM
dtm <- doc_words %>%
  cast_dtm(doc_id, word, n)

cat("Matriz DTM creada:", nrow(dtm), "documentos x", ncol(dtm), "términos\n\n")

# ----------------------------------------------------------------------------
# 2. MÉTODO 1: PERPLEJIDAD (PERPLEXITY)
# ----------------------------------------------------------------------------

cat("=== CALCULANDO PERPLEJIDAD PARA DIFERENTES VALORES DE K ===\n\n")

# Definir rango de k a probar
k_values <- seq(2, 12, by = 1)

# Calcular perplejidad para cada k
# Usaremos validación cruzada hold-out (80% entrenamiento, 20% prueba)
set.seed(1234)

# Dividir datos en entrenamiento y prueba
n_docs <- nrow(dtm)
train_idx <- sample(1:n_docs, size = floor(0.8 * n_docs))
test_idx <- setdiff(1:n_docs, train_idx)

dtm_train <- dtm[train_idx, ]
dtm_test <- dtm[test_idx, ]

# Calcular perplejidad para cada k
perplexity_results <- data.frame(k = integer(), perplexity = numeric())

i = 1
for(i in 1:length(k_values)) {
  k <- k_values[i]

  cat("Calculando perplejidad para k =", k, "...\n")

  # Ajustar modelo en datos de entrenamiento
  lda_temp <- LDA(dtm_train,
                  k = k,
                  method = "Gibbs",
                  control = list(seed = 1234,
                                iter = 1000,
                                thin = 50,
                                burnin = 50))

  # Calcular perplejidad en datos de prueba
  perp <- perplexity(lda_temp, newdata = dtm_test)

  perplexity_results <- rbind(perplexity_results,
                              data.frame(k = k, perplexity = perp))

  cat("  Perplejidad:", round(perp, 2), "\n")
}

cat("\n=== RESULTADOS DE PERPLEJIDAD ===\n")
print(perplexity_results)

# Encontrar k óptimo (menor perplejidad)
k_optimo_perp <- perplexity_results$k[which.min(perplexity_results$perplexity)]
cat("\nK óptimo según perplejidad:", k_optimo_perp, "\n")
cat("Perplejidad mínima:", round(min(perplexity_results$perplexity), 2), "\n\n")

# ----------------------------------------------------------------------------
# 3. MÉTODO 2: LOG-LIKELIHOOD
# ----------------------------------------------------------------------------

cat("=== CALCULANDO LOG-LIKELIHOOD PARA DIFERENTES VALORES DE K ===\n\n")

# Calcular log-likelihood para cada k
loglik_results <- data.frame(k = integer(), loglikelihood = numeric())

i = 1
for(i in 1:length(k_values)) {
  k <- k_values[i]

  cat("Calculando log-likelihood para k =", k, "...\n")

  # Ajustar modelo
  lda_temp <- LDA(dtm,
                  k = k,
                  method = "Gibbs",
                  control = list(seed = 1234,
                                iter = 1000,
                                thin = 50,
                                burnin = 50))

  # Calcular log-likelihood
  ll <- logLik(lda_temp)

  loglik_results <- rbind(loglik_results,
                         data.frame(k = k, loglikelihood = as.numeric(ll)))

  cat("  Log-likelihood:", round(as.numeric(ll), 2), "\n")
}

cat("\n=== RESULTADOS DE LOG-LIKELIHOOD ===\n")
print(loglik_results)

# Encontrar k óptimo (mayor log-likelihood)
k_optimo_ll <- loglik_results$k[which.max(loglik_results$loglikelihood)]
cat("\nK óptimo según log-likelihood:", k_optimo_ll, "\n")
cat("Log-likelihood máximo:", round(max(loglik_results$loglikelihood), 2), "\n\n")

# ----------------------------------------------------------------------------
# 4. MÉTODO 3: COHERENCIA SEMÁNTICA (PMI)
# ----------------------------------------------------------------------------

cat("=== CALCULANDO COHERENCIA SEMÁNTICA PARA DIFERENTES K ===\n\n")

# Función para calcular coherencia semántica simple
# Basada en co-ocurrencia de términos top en documentos (PMI)
calcular_coherencia <- function(lda_model, dtm, n_terms = 10) {

  # Extraer términos top por tópico
  topics_beta <- tidy(lda_model, matrix = "beta")

  # Convertir DTM a matriz binaria (presencia/ausencia)
  dtm_matrix <- as.matrix(dtm)
  dtm_binary <- (dtm_matrix > 0) * 1

  # Calcular co-ocurrencias entre términos top
  coherence_scores <- numeric()

  topics <- unique(topics_beta$topic)

  i = 1
  for(i in 1:length(topics)) {
    topic <- topics[i]

    # Obtener términos del tópico actual
    terms <- topics_beta %>%
      filter(topic == !!topic) %>%
      slice_max(beta, n = n_terms) %>%
      pull(term)

    # Filtrar términos que existen en DTM
    terms <- terms[terms %in% colnames(dtm_matrix)]

    if(length(terms) < 2) {
      coherence_scores <- c(coherence_scores, 0)
      next
    }

    # Calcular PMI (Pointwise Mutual Information) promedio
    pmi_sum <- 0
    count <- 0

    j = 1
    for(j in 1:(length(terms)-1)) {
      k = j + 1
      for(k in (j+1):length(terms)) {
        term1 <- terms[j]
        term2 <- terms[k]

        # Documentos que contienen cada término
        docs_term1 <- sum(dtm_binary[, term1])
        docs_term2 <- sum(dtm_binary[, term2])
        docs_both <- sum(dtm_binary[, term1] & dtm_binary[, term2])

        # Evitar log(0)
        if(docs_both > 0 && docs_term1 > 0 && docs_term2 > 0) {
          pmi <- log((docs_both * nrow(dtm_binary)) / (docs_term1 * docs_term2))
          pmi_sum <- pmi_sum + pmi
          count <- count + 1
        }
      }
    }

    coherence_scores <- c(coherence_scores, pmi_sum / max(count, 1))
  }

  # Retornar coherencia promedio
  return(mean(coherence_scores))
}

# Calcular coherencia para diferentes k
coherence_results <- data.frame(k = integer(), coherence = numeric())

i = 1
for(i in 1:length(k_values)) {
  k <- k_values[i]

  cat("Calculando coherencia para k =", k, "...\n")

  # Ajustar modelo
  lda_temp <- LDA(dtm,
                  k = k,
                  method = "Gibbs",
                  control = list(seed = 1234,
                                iter = 1000,
                                thin = 50,
                                burnin = 50))

  # Calcular coherencia
  coh <- calcular_coherencia(lda_temp, dtm, n_terms = 10)

  coherence_results <- rbind(coherence_results,
                            data.frame(k = k, coherence = coh))

  cat("  Coherencia:", round(coh, 4), "\n")
}

cat("\n=== RESULTADOS DE COHERENCIA SEMÁNTICA ===\n")
print(coherence_results)

# Encontrar k óptimo (mayor coherencia)
k_optimo_coh <- coherence_results$k[which.max(coherence_results$coherence)]
cat("\nK óptimo según coherencia:", k_optimo_coh, "\n")
cat("Coherencia máxima:", round(max(coherence_results$coherence), 4), "\n\n")

# ----------------------------------------------------------------------------
# 5. VISUALIZACIONES
# ----------------------------------------------------------------------------

cat("=== GENERANDO VISUALIZACIONES ===\n\n")

# Gráfico 1: Perplejidad
plot_perplexity <- ggplot(perplexity_results, aes(x = k, y = perplexity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3, color = "blue") +
  geom_vline(xintercept = k_optimo_perp, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = k_optimo_perp + 0.5, y = max(perplexity_results$perplexity) * 0.95,
           label = paste("Óptimo: k =", k_optimo_perp),
           color = "red", hjust = 0, size = 4) +
  labs(title = "Perplejidad vs Número de Tópicos",
       subtitle = "Menor perplejidad indica mejor capacidad predictiva del modelo",
       x = "Número de tópicos (k)",
       y = "Perplejidad") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

ggsave("03_Graficas/optimizacion_perplexidad.png",
       plot = plot_perplexity,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")

# Gráfico 2: Log-Likelihood
plot_loglik <- ggplot(loglik_results, aes(x = k, y = loglikelihood)) +
  geom_line(color = "purple", size = 1) +
  geom_point(size = 3, color = "purple") +
  geom_vline(xintercept = k_optimo_ll, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = k_optimo_ll + 0.5, y = min(loglik_results$loglikelihood) * 1.001,
           label = paste("Óptimo: k =", k_optimo_ll),
           color = "red", hjust = 0, size = 4) +
  labs(title = "Log-Likelihood vs Número de Tópicos",
       subtitle = "Mayor log-likelihood indica mejor ajuste del modelo a los datos",
       x = "Número de tópicos (k)",
       y = "Log-Likelihood") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

ggsave("03_Graficas/optimizacion_loglikelihood.png",
       plot = plot_loglik,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")

# Gráfico 3: Coherencia
plot_coherence <- ggplot(coherence_results, aes(x = k, y = coherence)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(size = 3, color = "darkgreen") +
  geom_vline(xintercept = k_optimo_coh, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = k_optimo_coh + 0.5, y = max(coherence_results$coherence) * 0.95,
           label = paste("Óptimo: k =", k_optimo_coh),
           color = "red", hjust = 0, size = 4) +
  labs(title = "Coherencia Semántica vs Número de Tópicos",
       subtitle = "Mayor coherencia indica tópicos más interpretables semánticamente (PMI)",
       x = "Número de tópicos (k)",
       y = "Coherencia (PMI promedio)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

ggsave("03_Graficas/optimizacion_coherencia.png",
       plot = plot_coherence,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")

# Gráfico 4: Comparación combinada (escalas normalizadas)
# Normalizar todas las métricas a rango [0, 1]
all_metrics <- perplexity_results %>%
  mutate(perplexity_norm = 1 - (perplexity - min(perplexity)) / (max(perplexity) - min(perplexity))) %>%
  left_join(loglik_results, by = "k") %>%
  mutate(loglik_norm = (loglikelihood - min(loglikelihood)) / (max(loglikelihood) - min(loglikelihood))) %>%
  left_join(coherence_results, by = "k") %>%
  mutate(coherence_norm = (coherence - min(coherence)) / (max(coherence) - min(coherence))) %>%
  select(k, perplexity_norm, loglik_norm, coherence_norm) %>%
  pivot_longer(cols = -k, names_to = "metrica", values_to = "valor_normalizado")

plot_combined <- ggplot(all_metrics, aes(x = k, y = valor_normalizado, color = metrica)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("perplexity_norm" = "blue",
                                "loglik_norm" = "purple",
                                "coherence_norm" = "darkgreen"),
                    labels = c("Perplejidad (invertida)",
                              "Log-Likelihood",
                              "Coherencia")) +
  labs(title = "Comparación de Métricas de Optimización",
       subtitle = "Valores normalizados a [0,1] - Mayor es mejor para todas",
       x = "Número de tópicos (k)",
       y = "Valor normalizado",
       color = "Métrica") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom")

ggsave("03_Graficas/optimizacion_comparacion.png",
       plot = plot_combined,
       width = 12,
       height = 7,
       dpi = 300,
       bg = "white")

cat("Gráficos guardados exitosamente.\n\n")

# ----------------------------------------------------------------------------
# 6. RESUMEN Y RECOMENDACIONES
# ----------------------------------------------------------------------------

cat("\n============================================================================\n")
cat("RESUMEN DE OPTIMIZACIÓN DEL NÚMERO DE TÓPICOS\n")
cat("============================================================================\n\n")

cat("Rango evaluado: k =", min(k_values), "a", max(k_values), "\n\n")

cat("--- RECOMENDACIONES POR MÉTODO ---\n\n")

cat("1. PERPLEJIDAD - Capacidad Predictiva (menor es mejor)\n")
cat("   K óptimo:", k_optimo_perp, "\n")
cat("   Perplejidad:", round(min(perplexity_results$perplexity), 2), "\n")
cat("   Interpretación: Qué tan bien el modelo predice datos no vistos\n\n")

cat("2. LOG-LIKELIHOOD - Ajuste del Modelo (mayor es mejor)\n")
cat("   K óptimo:", k_optimo_ll, "\n")
cat("   Log-Likelihood:", round(max(loglik_results$loglikelihood), 2), "\n")
cat("   Interpretación: Qué tan bien el modelo explica los datos observados\n\n")

cat("3. COHERENCIA SEMÁNTICA - Interpretabilidad (mayor es mejor)\n")
cat("   K óptimo:", k_optimo_coh, "\n")
cat("   Coherencia (PMI):", round(max(coherence_results$coherence), 4), "\n")
cat("   Interpretación: Qué tan relacionados están los términos dentro de cada tópico\n\n")

# Tabla resumen
cat("--- TABLA COMPARATIVA ---\n\n")
resumen <- data.frame(
  k = k_values,
  Perplejidad = perplexity_results$perplexity,
  LogLikelihood = loglik_results$loglikelihood,
  Coherencia = coherence_results$coherence
)
print(resumen)

# Calcular k promedio sugerido
k_sugeridos <- c(k_optimo_perp, k_optimo_ll, k_optimo_coh)
k_promedio <- round(median(k_sugeridos))

cat("\n--- RECOMENDACIÓN FINAL ---\n\n")
cat("Basado en los tres métodos evaluados:\n\n")
cat("  K sugerido (mediana):", k_promedio, "\n")
cat("  Rango recomendado:", min(k_sugeridos), "-", max(k_sugeridos), "\n\n")

cat("Valores específicos por método:\n")
cat("  - Perplejidad:", k_optimo_perp, "\n")
cat("  - Log-Likelihood:", k_optimo_ll, "\n")
cat("  - Coherencia:", k_optimo_coh, "\n\n")

cat("NOTA IMPORTANTE:\n")
cat("La elección final debe considerar:\n")
cat("1. Balance entre las tres métricas\n")
cat("2. Interpretabilidad de los tópicos resultantes\n")
cat("3. Objetivos del análisis (¿se busca granularidad o generalización?)\n")
cat("4. Tamaño del corpus (110 documentos)\n\n")

cat("RECOMENDACIÓN PRÁCTICA:\n")
cat("Para este corpus de", nrow(articulos), "artículos, se sugiere:\n")
cat("  - Probar k =", k_promedio, "como valor principal\n")
cat("  - Considerar también k =", k_optimo_coh, "(mejor coherencia)\n")
cat("  - Revisar manualmente los tópicos resultantes\n\n")

cat("Gráficos generados:\n")
cat("  - 03_Graficas/optimizacion_perplexidad.png\n")
cat("  - 03_Graficas/optimizacion_loglikelihood.png\n")
cat("  - 03_Graficas/optimizacion_coherencia.png\n")
cat("  - 03_Graficas/optimizacion_comparacion.png\n\n")

cat("Análisis de optimización completado exitosamente.\n")
