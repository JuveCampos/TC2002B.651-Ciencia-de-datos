# Clasificación de Sentimientos con Random Forest ####

library(tidyverse)
library(readxl)
library(writexl)
library(tidytext)
library(randomForest)
library(tm)
library(plotly)

# Cargar datos de entrenamiento ####

datos_entrenamiento <- read_excel("comentarios_de_entrenamiento.xlsx")

# Cargar datos a clasificar ####

datos_clasificar <- read_excel("legacy/dataset_sentimientos.xlsx") %>%
  select(-pred_lexicon, -pred_emb_local, -pred_emb_openai, -pred_llama31, -pred_qwen25)

# Stopwords en español ####

# Crear lista de stopwords en español
# tm tiene stopwords en español, o podemos usar una lista personalizada
stopwords_es <- tibble(word = tm::stopwords("spanish"))

# Preprocesamiento de texto ####

# Función para limpiar y tokenizar texto
limpiar_texto <- function(texto) {
  texto %>%
    str_to_lower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]") %>%
    str_squish()
}

# Aplicar limpieza a datos de entrenamiento
datos_entrenamiento_limpio <- datos_entrenamiento %>%
  mutate(
    texto_limpio = limpiar_texto(comentario),
    sentimiento = as.factor(sentimiento)
  )

# Aplicar limpieza a datos a clasificar
datos_clasificar_limpio <- datos_clasificar %>%
  mutate(texto_limpio = limpiar_texto(texto))

# Crear matriz TF-IDF para entrenamiento ####

# Tokenizar textos de entrenamiento
tokens_entrenamiento <- datos_entrenamiento_limpio %>%
  mutate(id_temp = row_number()) %>%
  select(id_temp, texto_limpio, sentimiento) %>%
  unnest_tokens(word, texto_limpio) %>%
  # Eliminar stopwords en español
  anti_join(stopwords_es, by = "word")

# Calcular TF-IDF
tfidf_entrenamiento <- tokens_entrenamiento %>%
  count(id_temp, word) %>%
  bind_tf_idf(word, id_temp, n) %>%
  group_by(id_temp) %>%
  slice_max(tf_idf, n = 50) %>%
  ungroup()

# Crear matriz documento-término para entrenamiento
matriz_entrenamiento <- tfidf_entrenamiento %>%
  cast_dtm(id_temp, word, tf_idf) %>%
  as.matrix()

# Convertir a dataframe y agregar etiquetas
df_entrenamiento <- as.data.frame(matriz_entrenamiento)
df_entrenamiento$sentimiento <- datos_entrenamiento_limpio$sentimiento

# Crear matriz TF-IDF para datos a clasificar ####

# Tokenizar textos a clasificar
tokens_clasificar <- datos_clasificar_limpio %>%
  select(id, texto_limpio) %>%
  unnest_tokens(word, texto_limpio) %>%
  # Eliminar stopwords en español
  anti_join(stopwords_es, by = "word")

# Calcular TF-IDF manteniendo el mismo vocabulario del entrenamiento
vocabulario_entrenamiento <- tfidf_entrenamiento %>%
  distinct(word) %>%
  pull(word)

tfidf_clasificar <- tokens_clasificar %>%
  filter(word %in% vocabulario_entrenamiento) %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  group_by(id) %>%
  slice_max(tf_idf, n = 50) %>%
  ungroup()

# Crear matriz documento-término para clasificar
# Asegurar que tenga las mismas columnas que el entrenamiento
todas_palabras <- colnames(matriz_entrenamiento)

# Crear dataframe base con todos los IDs para mantener todas las filas
base_ids <- datos_clasificar %>% select(id)

# Crear matriz TF-IDF y combinar con todos los IDs
matriz_clasificar_temp <- tfidf_clasificar %>%
  pivot_wider(id_cols = id, names_from = word, values_from = tf_idf, values_fill = 0)

# Hacer left_join para mantener todos los IDs, incluso los que no tienen palabras del vocabulario
matriz_clasificar <- base_ids %>%
  left_join(matriz_clasificar_temp, by = "id")

# Reemplazar NA con 0 en todas las columnas de palabras
matriz_clasificar <- matriz_clasificar %>%
  mutate(across(-id, ~ replace_na(.x, 0)))

# Seleccionar solo las columnas del vocabulario de entrenamiento que existen
matriz_clasificar <- matriz_clasificar %>%
  select(id, any_of(todas_palabras))

# Agregar columnas faltantes con ceros
columnas_faltantes <- setdiff(todas_palabras, colnames(matriz_clasificar))

for (col in columnas_faltantes) {
  matriz_clasificar[[col]] <- 0
}

# Reordenar columnas para que coincidan con el entrenamiento
matriz_clasificar <- matriz_clasificar %>%
  select(id, all_of(todas_palabras))

# Convertir a dataframe manteniendo el orden original de los IDs
df_clasificar <- matriz_clasificar %>%
  select(-id) %>%
  as.data.frame()

# Entrenar modelo Random Forest ####

set.seed(42)

modelo_rf <- randomForest(
  sentimiento ~ .,
  data = df_entrenamiento,
  ntree = 500,
  mtry = sqrt(ncol(df_entrenamiento) - 1),
  importance = TRUE
)

# Realizar predicciones ####

predicciones <- predict(modelo_rf, df_clasificar, type = "class")
probabilidades <- predict(modelo_rf, df_clasificar, type = "prob")

# Combinar resultados ####

resultados <- datos_clasificar %>%
  mutate(
    pred_random_forest = as.character(predicciones),
    prob_positivo = probabilidades[, "positivo"],
    prob_negativo = probabilidades[, "negativo"],
    prob_neutro = probabilidades[, "neutro"],
    confianza = pmax(prob_positivo, prob_negativo, prob_neutro)
  )

# Calcular accuracy si existe etiqueta real ####

if ("etiqueta_real" %in% colnames(resultados)) {
  accuracy <- resultados %>%
    filter(!is.na(etiqueta_real)) %>%
    summarise(
      accuracy = mean(pred_random_forest == etiqueta_real, na.rm = TRUE),
      total = n(),
      correctos = sum(pred_random_forest == etiqueta_real, na.rm = TRUE)
    )

  # Matriz de confusión
  matriz_confusion <- resultados %>%
    filter(!is.na(etiqueta_real)) %>%
    count(etiqueta_real, pred_random_forest) %>%
    pivot_wider(names_from = pred_random_forest, values_from = n, values_fill = 0)

  # Métricas por clase
  metricas_clase <- resultados %>%
    filter(!is.na(etiqueta_real)) %>%
    group_by(etiqueta_real) %>%
    summarise(
      total = n(),
      correctos = sum(pred_random_forest == etiqueta_real),
      precision = correctos / total
    )

  # Guardar resultados con métricas
  write_xlsx(
    list(
      resultados = resultados,
      accuracy = accuracy,
      matriz_confusion = matriz_confusion,
      metricas_clase = metricas_clase
    ),
    "2_random_forest/2_random_forest_resultados.xlsx"
  )
} else {
  # Guardar solo resultados
  write_xlsx(resultados, "2_random_forest/2_random_forest_resultados.xlsx")
}

# Importancia de variables ####

importancia <- importance(modelo_rf) %>%
  as.data.frame() %>%
  rownames_to_column("palabra") %>%
  as_tibble() %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(20)

# Visualización de distribución de predicciones ####

resumen_pred <- resultados %>%
  count(pred_random_forest) %>%
  mutate(porcentaje = n / sum(n) * 100)

ggplot(resumen_pred, aes(x = pred_random_forest, y = n, fill = pred_random_forest)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
            vjust = -0.5) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = "Distribución de Sentimientos - Random Forest",
    x = "Sentimiento",
    y = "Cantidad de comentarios"
  ) +
  theme_minimal()

ggsave("2_random_forest/2_distribucion_predicciones.png", width = 10, height = 6, dpi = 300)

# Visualización de confianza de predicciones estática ####

ggplot(resultados, aes(x = id, y = confianza, color = pred_random_forest)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = "Confianza de Predicciones por Comentario",
    x = "ID del comentario",
    y = "Confianza de la predicción",
    color = "Sentimiento"
  ) +
  theme_minimal()

ggsave("2_random_forest/2_confianza_predicciones.png", width = 12, height = 6, dpi = 300)

# Visualización interactiva de confianza con plotly ####

grafica_confianza_interactiva <- plot_ly(
  data = resultados,
  x = ~id,
  y = ~confianza,
  color = ~pred_random_forest,
  colors = c("positivo" = "#4CAF50", "negativo" = "#F44336", "neutro" = "#9E9E9E"),
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  # El texto que aparece en el tooltip al pasar el mouse
  text = ~paste0(
    "ID: ", id, "<br>",
    "Confianza: ", round(confianza, 3), "<br>",
    "Predicción: ", pred_random_forest, "<br>",
    "Prob Positivo: ", round(prob_positivo, 3), "<br>",
    "Prob Negativo: ", round(prob_negativo, 3), "<br>",
    "Prob Neutro: ", round(prob_neutro, 3), "<br>",
    "Comentario: ", texto
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Confianza de Predicciones por Comentario (Interactivo)",
    xaxis = list(title = "ID del comentario"),
    yaxis = list(title = "Confianza de la predicción"),
    hovermode = "closest"
  )

htmlwidgets::saveWidget(grafica_confianza_interactiva, "2_random_forest/2_confianza_interactivo.html", selfcontained = TRUE)

# Visualización de importancia de palabras ####

ggplot(importancia, aes(x = reorder(palabra, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#2196F3") +
  coord_flip() +
  labs(
    title = "Palabras Más Importantes para la Clasificación",
    subtitle = "Top 20 según Mean Decrease Gini",
    x = "Palabra",
    y = "Importancia"
  ) +
  theme_minimal()

ggsave("2_random_forest/2_importancia_palabras.png", width = 10, height = 8, dpi = 300)

# Visualización de matriz de confusión ####

if ("etiqueta_real" %in% colnames(resultados)) {
  matriz_conf_long <- resultados %>%
    filter(!is.na(etiqueta_real)) %>%
    count(etiqueta_real, pred_random_forest) %>%
    group_by(etiqueta_real) %>%
    mutate(porcentaje = n / sum(n) * 100)

  ggplot(matriz_conf_long, aes(x = etiqueta_real, y = pred_random_forest, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
              color = "white", size = 4) +
    scale_fill_gradient(low = "#E3F2FD", high = "#1976D2") +
    labs(
      title = "Matriz de Confusión - Random Forest",
      x = "Etiqueta Real",
      y = "Predicción",
      fill = "Cantidad"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  ggsave("2_random_forest/2_matriz_confusion.png", width = 8, height = 6, dpi = 300)
}
