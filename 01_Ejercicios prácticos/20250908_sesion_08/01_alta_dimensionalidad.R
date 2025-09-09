# Ilustración de Alta Dimensionalidad en Datos de Texto
# ========================================================

# Cargar librerías necesarias
library(tm)        # Para procesamiento de texto
library(Matrix)    # Para matrices dispersas
library(ggplot2)   # Para visualizaciones
library(tidyr)     # Para manipulación de datos

# Configurar semilla para reproducibilidad
set.seed(123)
# 1. CREAR UN CORPUS DE EJEMPLO
# ------------------------------
documentos <- c(
  "El análisis de texto es fascinante y complejo",
  "Los datos de texto tienen alta dimensionalidad",
  "Machine learning procesa grandes volúmenes de texto",
  "El procesamiento del lenguaje natural es importante",
  "Los algoritmos analizan patrones en los datos",
  "La dimensionalidad aumenta con el vocabulario",
  "Cada palabra única añade una nueva dimensión",
  "Los vectores de texto son típicamente dispersos"
)

corpus <- Corpus(VectorSource(documentos))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

# Crear matriz documento-término
dtm <- DocumentTermMatrix(corpus)
matriz_dtm <- as.matrix(dtm)

cat("=== DIMENSIONALIDAD DE LA REPRESENTACIÓN ===\n")
cat("Dimensiones de la matriz:", nrow(matriz_dtm), "documentos x", 
    ncol(matriz_dtm), "palabras únicas\n")
cat("Total de elementos en la matriz:", nrow(matriz_dtm) * ncol(matriz_dtm), "\n")
cat("Cada documento es un vector de", ncol(matriz_dtm), "dimensiones\n\n")

# 3. ANALIZAR DISPERSIDAD (SPARSITY)
# -----------------------------------
total_elementos <- prod(dim(matriz_dtm))
elementos_cero <- sum(matriz_dtm == 0)
elementos_no_cero <- sum(matriz_dtm != 0)
sparsity <- elementos_cero / total_elementos * 100

cat("=== ANÁLISIS DE DISPERSIDAD ===\n")
cat("Elementos totales:", total_elementos, "\n")
cat("Elementos con valor 0:", elementos_cero, "\n")
cat("Elementos con valor > 0:", elementos_no_cero, "\n")
cat("Dispersidad (% de ceros):", round(sparsity, 2), "%\n\n")

# 4. VISUALIZACIÓN DE LA MATRIZ DISPERSA
# ---------------------------------------
# Convertir matriz a formato largo para visualización
matriz_larga <- as.data.frame(matriz_dtm)
matriz_larga$documento <- paste("Doc", 1:nrow(matriz_dtm))
matriz_larga <- pivot_longer(matriz_larga, 
                             cols = -documento,
                             names_to = "palabra",
                             values_to = "frecuencia")

# Crear heatmap
p1 <- ggplot(matriz_larga, aes(x = palabra, y = documento, fill = frecuencia)) +
  geom_tile() +
  geom_text(aes(label = frecuencia), 
            color = "white") + 
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Matriz Documento-Término: Visualización de Alta Dimensionalidad",
       subtitle = paste("Cada fila es un documento, cada columna una palabra única (",
                        ncol(matriz_dtm), "dimensiones)"),
       x = "Palabras (Dimensiones)", 
       y = "Documentos",
       fill = "Frecuencia")

print(p1)

# 5. EJEMPLO CON VOCABULARIO MÁS GRANDE
# --------------------------------------
cat("=== ESCALAMIENTO DE DIMENSIONALIDAD ===\n")

# Simular diferentes tamaños de corpus
tamaños_corpus <- c(10, 50, 100, 500, 1000)
vocabularios_estimados <- c()

for (n_docs in tamaños_corpus) {
  # Estimar vocabulario único (aproximación)
  vocab_size <- round(n_docs * 15 * 0.6)  # Asumiendo ~15 palabras/doc, 60% únicas
  vocabularios_estimados <- c(vocabularios_estimados, vocab_size)
}

df_escalamiento <- data.frame(
  documentos = tamaños_corpus,
  dimensiones = vocabularios_estimados,
  elementos_totales = tamaños_corpus * vocabularios_estimados
)

print(df_escalamiento)

# Visualizar crecimiento de dimensionalidad
p2 <- ggplot(df_escalamiento, aes(x = documentos, y = dimensiones)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(size = 3, color = "darkblue") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Crecimiento de la Dimensionalidad con el Tamaño del Corpus",
       x = "Número de Documentos (escala log)",
       y = "Número de Dimensiones (escala log)",
       subtitle = "La dimensionalidad crece rápidamente con más documentos")

print(p2)

# 6. COMPARACIÓN: VECTOR DE UN DOCUMENTO ESPECÍFICO
# --------------------------------------------------
cat("\n=== EJEMPLO: VECTOR DEL DOCUMENTO 1 ===\n")
vector_doc1 <- matriz_dtm[1, ]
cat("Dimensiones del vector:", length(vector_doc1), "\n")
cat("Valores no cero:", sum(vector_doc1 > 0), "\n")
cat("Valores cero:", sum(vector_doc1 == 0), "\n")
cat("\nPalabras presentes en el documento 1:\n")
palabras_presentes <- names(vector_doc1[vector_doc1 > 0])
for (palabra in palabras_presentes) {
  cat("  -", palabra, ": frecuencia =", vector_doc1[palabra], "\n")
}

# 7. PROBLEMA DE LA MALDICIÓN DE LA DIMENSIONALIDAD
# -------------------------------------------------
cat("\n=== IMPLICACIONES DE LA ALTA DIMENSIONALIDAD ===\n")
cat("1. Memoria requerida: Una matriz de 10,000 docs x 50,000 palabras\n")
cat("   requeriría", format(10000 * 50000 * 8 / 1024^2, digits = 2), 
    "MB en formato denso\n")
cat("2. La mayoría de algoritmos de ML sufren con alta dimensionalidad\n")
cat("3. La distancia entre puntos se vuelve menos significativa\n")
cat("4. Se requieren técnicas de reducción dimensional (PCA, LSA, embeddings)\n")

# 8. VISUALIZACIÓN DE DISPERSIDAD POR DOCUMENTO
# ---------------------------------------------
dispersidad_por_doc <- apply(matriz_dtm, 1, function(x) sum(x == 0) / length(x) * 100)
df_dispersidad <- data.frame(
  documento = paste("Doc", 1:length(dispersidad_por_doc)),
  dispersidad = dispersidad_por_doc
)

p3 <- ggplot(df_dispersidad, aes(x = documento, y = dispersidad)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  labs(title = "Dispersidad por Documento",
       subtitle = "Porcentaje de dimensiones con valor cero",
       x = "Documento",
       y = "% de Ceros") +
  ylim(0, 100)

print(p3)

cat("\n=== RESUMEN ===\n")
cat("Los datos de texto son de alta dimensionalidad porque:\n")
cat("- Cada palabra única del vocabulario representa una dimensión\n")
cat("- Los vocabularios pueden tener miles o millones de palabras\n")
cat("- Las matrices resultantes son muy dispersas (muchos ceros)\n")
cat("- Esto crea desafíos computacionales y estadísticos significativos\n")
