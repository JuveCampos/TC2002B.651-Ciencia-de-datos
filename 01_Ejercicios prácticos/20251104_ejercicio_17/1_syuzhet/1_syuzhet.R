# Análisis de Sentimientos con Syuzhet ####

library(tidyverse)
library(readxl)
library(writexl)
library(syuzhet)
library(tidytext)
library(plotly)

# Cargar datos ####

datos <- read_excel("dataset_sentimientos.xlsx")

# Análisis con syuzhet ####

# El paquete syuzhet ofrece varios métodos de análisis de sentimiento
# Para textos en español, usaremos el método "syuzhet" que funciona mejor con múltiples idiomas
# También podríamos usar "afinn", "bing" o "nrc", pero "syuzhet" es más robusto para español

datos_con_sentimiento <- datos %>%
  mutate(
    # Obtener score de sentimiento para cada texto usando el método syuzhet
    # Este método es mejor para textos en español que los léxicos ingleses
    syuzhet_score = get_sentiment(texto, method = "syuzhet", language = "spanish"),

    # Clasificar en base al score
    # Score > 0: positivo, Score < 0: negativo, Score = 0: neutro
    pred_syuzhet = case_when(
      syuzhet_score > 0 ~ "positivo",
      syuzhet_score < 0 ~ "negativo",
      TRUE ~ "neutro"
    )
  ) %>% 
  select(-pred_lexicon, -pred_emb_local, -pred_emb_openai, -pred_llama31, -pred_qwen25)

# Análisis detallado por emociones ####

# Syuzhet también puede detectar emociones específicas usando el léxico NRC
# NRC tiene soporte para español
emociones_nrc <- datos %>%
  select(id, texto) %>%
  mutate(
    # map(): aplica la función get_nrc_sentiment() a cada elemento del vector texto
    # ~ indica que estamos creando una función anónima
    # .x representa cada elemento individual del vector
    # Resultado: para cada texto, obtenemos un dataframe con scores de emociones
    emociones = map(texto, ~ get_nrc_sentiment(.x, language = "spanish"))
  ) %>%
  unnest(emociones) %>%
  select(id, texto, everything())

# Combinar resultados de sentimiento y emociones
resultados_completos <- datos_con_sentimiento %>%
  left_join(
    emociones_nrc %>% select(id, anger:positive),
    by = "id"
  )

# Calcular accuracy si existe etiqueta real ####

if ("etiqueta_real" %in% colnames(resultados_completos)) {
  accuracy <- resultados_completos %>%
    filter(!is.na(etiqueta_real)) %>%
    summarise(
      accuracy = mean(pred_syuzhet == etiqueta_real, na.rm = TRUE),
      total = n(),
      correctos = sum(pred_syuzhet == etiqueta_real, na.rm = TRUE)
    )

  # Matriz de confusión
  matriz_confusion <- resultados_completos %>%
    filter(!is.na(etiqueta_real)) %>%
    count(etiqueta_real, pred_syuzhet) %>%
    pivot_wider(names_from = pred_syuzhet, values_from = n, values_fill = 0)

  # Guardar métricas
  write_xlsx(
    list(
      resultados = resultados_completos,
      accuracy = accuracy,
      matriz_confusion = matriz_confusion
    ),
    "1_syuzhet/1_syuzhet_resultados.xlsx"
  )
} else {
  # Guardar solo resultados
  write_xlsx(resultados_completos, "1_syuzhet/1_syuzhet_resultados.xlsx")
}

# Resumen de resultados ####

resumen <- resultados_completos %>%
  count(pred_syuzhet) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Visualización de distribución de sentimientos ####

ggplot(resumen, aes(x = pred_syuzhet, y = n, fill = pred_syuzhet)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
            vjust = -0.5) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = "Distribución de Sentimientos - Método Syuzhet",
    x = "Sentimiento",
    y = "Cantidad de comentarios"
  ) +
  theme_minimal()

ggsave("1_syuzhet/1_distribucion_sentimientos.png", width = 10, height = 6, dpi = 300)

# Visualización de scores de sentimiento estática ####

ggplot(resultados_completos, aes(x = id, y = syuzhet_score, color = pred_syuzhet)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = "Score de Sentimiento por Comentario",
    x = "ID del comentario",
    y = "Score de sentimiento",
    color = "Clasificación"
  ) +
  theme_minimal()

ggsave("1_syuzhet/1_scores_sentimientos.png", width = 12, height = 6, dpi = 300)

# Visualización interactiva de scores con plotly ####

# Crear gráfica interactiva donde el hover muestra el comentario completo
grafica_interactiva <- plot_ly(
  data = resultados_completos,
  x = ~id,
  y = ~syuzhet_score,
  color = ~pred_syuzhet,
  colors = c("positivo" = "#4CAF50", "negativo" = "#F44336", "neutro" = "#9E9E9E"),
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  # El texto que aparece en el tooltip al pasar el mouse
  text = ~paste0(
    "ID: ", id, "<br>",
    "Score: ", round(syuzhet_score, 2), "<br>",
    "Clasificación: ", pred_syuzhet, "<br>",
    "Comentario: ", texto
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Score de Sentimiento por Comentario (Interactivo)",
    xaxis = list(title = "ID del comentario"),
    yaxis = list(title = "Score de sentimiento"),
    hovermode = "closest",
    # Agregar línea horizontal en y=0
    shapes = list(
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = 0,
        y1 = 0,
        line = list(color = "gray", dash = "dash")
      )
    )
  )

grafica_interactiva
# Guardar gráfica interactiva como HTML
htmlwidgets::saveWidget(grafica_interactiva, "1_syuzhet/1_scores_interactivo.html", selfcontained = TRUE)

# Análisis de emociones dominantes ####

emociones_promedio <- resultados_completos %>%
  summarise(across(c(anger, anticipation, disgust, fear, joy, sadness,
                     surprise, trust, negative, positive), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "emocion", values_to = "promedio")

ggplot(emociones_promedio, aes(x = reorder(emocion, promedio), y = promedio, fill = emocion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Emociones Promedio Detectadas en los Comentarios",
    subtitle = "Basado en el léxico NRC",
    x = "Emoción",
    y = "Score promedio"
  ) +
  theme_minimal()

ggsave("1_syuzhet/1_emociones_promedio.png", width = 10, height = 6, dpi = 300)
