
library(tidytext)
library(topicmodels)
library(dplyr)
library(tibble)
library(stringr)

# Preparar los documentos
docs <- c(
  "El equipo ganó el partido y el entrenador estaba contento tras la victoria.",
  "El jugador anotó un gol en el último minuto del partido de fútbol.",
  "El presidente anunció nuevas medidas de política económica en su discurso de la noche.",
  "El ministro presentó un plan económico y político para el desarrollo del país.",
  "La empresa tecnológica lanzó un nuevo smartphone con cámara de alta resolución.",
  "Millones de personas compraron el smartphone inteligente este año."
)

# Crear matriz documento-término
docs_df <- tibble(doc_id = 1:length(docs), text = docs)
stopwords_es <- tibble(word = c("el", "la", "de", "y", "en", "un", "una", "para", 
                                "del", "con", "por", "al", "los", "las"))

dtm <- docs_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_es, by = "word") %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

# ============================================
# FORMA CORRECTA DE ESPECIFICAR HIPERPARÁMETROS
# ============================================

print("=== EXPERIMENTO CON DIFERENTES VALORES DE ALFA ===")

# OPCIÓN 1: Usar método VEM (por defecto)
# En VEM, solo puedes controlar alfa directamente
# El control de beta es limitado

# Alfa bajo: documentos especializados
set.seed(123)
lda_alpha_bajo <- LDA(dtm, 
                      k = 3, 
                      control = list(
                        alpha = 0.1,  # ALFA BAJO
                        seed = 123
                      ))

# Alfa alto: documentos con mezcla uniforme
lda_alpha_alto <- LDA(dtm, 
                      k = 3, 
                      control = list(
                        alpha = 5,    # ALFA ALTO
                        seed = 123
                      ))

# Ver distribuciones
gamma_bajo <- tidy(lda_alpha_bajo, matrix = "gamma")
gamma_alto <- tidy(lda_alpha_alto, matrix = "gamma")

print("Con ALFA BAJO (0.1) - Documentos tienden a especializarse:")
distribucion_bajo <- gamma_bajo %>%
  group_by(document) %>%
  summarise(
    topico_dominante = which.max(gamma),
    prob_maxima = max(gamma),
    num_topicos_sobre_0.2 = sum(gamma > 0.2)
  ) %>%
  mutate(
    descripcion = str_c("Doc ", document, 
                        ": Tópico principal = ", topico_dominante,
                        " (", round(prob_maxima, 2), ")",
                        " - Tópicos activos: ", num_topicos_sobre_0.2)
  )
print(as.data.frame(select(distribucion_bajo, descripcion)))

print("\nCon ALFA ALTO (5) - Documentos con mezcla más uniforme:")
distribucion_alto <- gamma_alto %>%
  group_by(document) %>%
  summarise(
    topico_dominante = which.max(gamma),
    prob_maxima = max(gamma),
    num_topicos_sobre_0.2 = sum(gamma > 0.2)
  ) %>%
  mutate(
    descripcion = str_c("Doc ", document, 
                        ": Tópico principal = ", topico_dominante,
                        " (", round(prob_maxima, 2), ")",
                        " - Tópicos activos: ", num_topicos_sobre_0.2)
  )
print(as.data.frame(select(distribucion_alto, descripcion)))

# ============================================
# OPCIÓN 2: Usar método Gibbs sampling
# Con Gibbs SÍ puedes controlar tanto alfa como beta (aquí se llama delta)
# ============================================

print("\n=== USANDO GIBBS SAMPLING PARA CONTROLAR ALFA Y BETA ===")

# Con Gibbs sampling - Beta bajo (vocabulario concentrado)
lda_gibbs_beta_bajo <- LDA(dtm, 
                           k = 3,
                           method = "Gibbs",
                           control = list(
                             alpha = 1,
                             delta = 0.001,  # BETA BAJO en Gibbs
                             seed = 123,
                             iter = 2000,
                             burnin = 1000
                           ))

# Con Gibbs sampling - Beta alto (vocabulario disperso)
lda_gibbs_beta_alto <- LDA(dtm, 
                           k = 3,
                           method = "Gibbs",
                           control = list(
                             alpha = 1,
                             delta = 1,      # BETA ALTO en Gibbs
                             seed = 123,
                             iter = 2000,
                             burnin = 1000
                           ))

# Comparar concentración del vocabulario
print("\nCon BETA BAJO (0.001) - Vocabulario muy concentrado:")
terms(lda_gibbs_beta_bajo, 5)

print("\nCon BETA ALTO (1) - Vocabulario más disperso:")
terms(lda_gibbs_beta_alto, 5)

# Analizar la distribución de probabilidades de palabras
beta_matrix_bajo <- exp(lda_gibbs_beta_bajo@beta)
beta_matrix_alto <- exp(lda_gibbs_beta_alto@beta)

# Calcular entropía para cada tópico (menor entropía = más concentrado)
entropia_topicos_bajo <- apply(beta_matrix_bajo, 1, function(x) {
  p <- x / sum(x)
  -sum(p * log(p + 1e-10))
})

entropia_topicos_alto <- apply(beta_matrix_alto, 1, function(x) {
  p <- x / sum(x)
  -sum(p * log(p + 1e-10))
})

print("\n=== ENTROPÍA DEL VOCABULARIO POR TÓPICO ===")
print("(Menor entropía = vocabulario más concentrado)")
comparacion_entropia <- tibble(
  Topico = 1:3,
  `Entropía Beta Bajo` = round(entropia_topicos_bajo, 3),
  `Entropía Beta Alto` = round(entropia_topicos_alto, 3),
  Diferencia = round(entropia_topicos_alto - entropia_topicos_bajo, 3)
)
print(as.data.frame(comparacion_entropia))

# ============================================
# VALORES RECOMENDADOS
# ============================================
print("\n=== CONFIGURACIÓN RECOMENDADA ===")

# Heurística común: alfa = 50/k
lda_recomendado <- LDA(dtm, 
                       k = 3,
                       method = "Gibbs",
                       control = list(
                         alpha = 50/3,     # Heurística: 50/k
                         delta = 0.1,      # Beta = 0.1 es común
                         seed = 123,
                         iter = 2000,
                         burnin = 1000,
                         thin = 100
                       ))

print(str_c("Hiperparámetros recomendados: alfa = ", round(50/3, 2), ", beta = 0.1"))
print("\nTópicos resultantes:")
terms(lda_recomendado, 5)

# ============================================
# RESUMEN DE MÉTODOS Y PARÁMETROS
# ============================================
print("\n=== RESUMEN: MÉTODOS Y PARÁMETROS EN R ===")

metodos_params <- tibble(
  Método = c("VEM (defecto)", "VEM (defecto)", 
             "Gibbs", "Gibbs"),
  Parámetro = c("alpha", "beta/delta", 
                "alpha", "delta"),
  Disponible = c("SÍ", "NO (usa valor fijo)",
                 "SÍ", "SÍ"),
  Cómo_especificar = c(
    "control = list(alpha = valor)",
    "No se puede cambiar directamente",
    "control = list(alpha = valor)",
    "control = list(delta = valor)"
  )
)

print(as.data.frame(metodos_params))

# Efecto de los hiperparámetros
print("\n=== EFECTOS DE LOS HIPERPARÁMETROS ===")
efectos <- tibble(
  Parámetro = c("Alfa (α)", "Alfa (α)", "Beta (β)", "Beta (β)"),
  Valor = c("Bajo (ej: 0.1)", "Alto (ej: 5)", 
            "Bajo (ej: 0.001)", "Alto (ej: 1)"),
  Efecto = c(
    "Docs con pocos tópicos dominantes",
    "Docs con mezcla uniforme de tópicos",
    "Tópicos con pocas palabras clave",
    "Tópicos con vocabulario amplio"
  )
)
print(as.data.frame(efectos))
