# Clasificación de Sentimientos con Embeddings ####

library(tidyverse)
library(readxl)
library(writexl)
library(httr2)
library(jsonlite)
library(plotly)

# Configuración ####

# Seleccionar método: "ollama" o "openai"
METODO <- "ollama"

# Configuración de Ollama
OLLAMA_HOST <- "http://localhost:11434"
EMBED_MODEL_OLLAMA <- "mxbai-embed-large"

# Configuración de OpenAI
# Si usas OpenAI, configura tu API key aquí o en variable de entorno
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Cargar datos ####

datos <- read_excel("legacy/dataset_sentimientos.xlsx") %>%
  select(-pred_lexicon, -pred_emb_local, -pred_emb_openai, -pred_llama31, -pred_qwen25)

# Función para obtener embeddings de Ollama ####

get_embedding_ollama <- function(texto, model = EMBED_MODEL_OLLAMA, host = OLLAMA_HOST) {
  req <- request(paste0(host, "/api/embeddings")) %>%
    req_body_json(list(model = model, prompt = texto)) %>%
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$embedding))
}

# Función para obtener embeddings de OpenAI ####

get_embedding_openai <- function(texto, api_key = OPENAI_API_KEY) {
  req <- request("https://api.openai.com/v1/embeddings") %>%
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(list(
      model = "text-embedding-3-small",
      input = texto
    )) %>%
    req_perform()

  resp <- resp_body_json(req)
  as.numeric(unlist(resp$data[[1]]$embedding))
}

# Función de similitud coseno ####

cosine_sim <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

# Prototipos de sentimientos ####

# Definir textos de ejemplo para cada categoría de sentimiento
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

    # Negativos
    "Muy decepcionado, no cumplió mis expectativas.",
    "Pésima calidad, no lo recomiendo para nada.",
    "Horrible experiencia, nunca más vuelvo.",
    "Terrible servicio, una pérdida de tiempo.",
    "Completamente insatisfecho, muy mala experiencia.",

    # Neutros
    "El producto es normal, cumple lo básico.",
    "Ni bueno ni malo, es promedio.",
    "Funciona correctamente, sin grandes sorpresas.",
    "Es estándar, similar a otros del mercado.",
    "Adecuado para uso básico, nada especial."
  )
)

# Generar embeddings de prototipos ####

if (METODO == "ollama") {
  # Usar Ollama para embeddings
  prototipos_con_embeddings <- prototipos %>%
    mutate(
      # map(): aplica la función get_embedding_ollama() a cada elemento del vector ejemplo
      # possibly(): envuelve la función para que retorne NULL en caso de error en lugar de detener la ejecución
      # .progress = TRUE: muestra barra de progreso durante la ejecución
      # Resultado: para cada texto prototipo, obtenemos su vector de embedding
      embedding = map(ejemplo, possibly(get_embedding_ollama, NULL), .progress = TRUE)
    )

} else if (METODO == "openai") {
  # Usar OpenAI para embeddings
  prototipos_con_embeddings <- prototipos %>%
    mutate(
      # map(): aplica la función get_embedding_openai() a cada elemento del vector ejemplo
      # Funciona igual que en el caso de Ollama pero usando la API de OpenAI
      embedding = map(ejemplo, possibly(get_embedding_openai, NULL), .progress = TRUE)
    )
}

# Verificar que todos los embeddings se generaron correctamente
prototipos_con_embeddings <- prototipos_con_embeddings %>%
  # map_lgl(): aplica is.null() a cada embedding y retorna un vector lógico (TRUE/FALSE)
  # Filtramos para mantener solo los embeddings que NO son NULL
  filter(!map_lgl(embedding, is.null))

# Generar embeddings de textos a clasificar ####

if (METODO == "ollama") {
  # Usar Ollama para embeddings
  datos_con_embeddings <- datos %>%
    mutate(
      # map(): aplica get_embedding_ollama() a cada texto a clasificar
      # Genera un embedding vectorial para cada comentario
      embedding = map(texto, possibly(get_embedding_ollama, NULL), .progress = TRUE)
    )

} else if (METODO == "openai") {
  # Usar OpenAI para embeddings
  datos_con_embeddings <- datos %>%
    mutate(
      # map(): aplica get_embedding_openai() a cada texto a clasificar
      embedding = map(texto, possibly(get_embedding_openai, NULL), .progress = TRUE)
    )
}

# Clasificar textos basándose en similitud con prototipos ####

# Para cada texto, calcular similitud con todos los prototipos
resultados <- datos_con_embeddings %>%
  mutate(
    # map(): para cada embedding de texto, calcula similitudes con todos los prototipos
    # Retorna un tibble con las similitudes promedio y máximas por categoría
    similitudes = map(embedding, function(emb_texto) {
      if (is.null(emb_texto)) return(NULL)

      # map_dbl(): aplica cosine_sim() a cada prototipo y retorna vector numérico
      # Calcula la similitud coseno entre el embedding del texto y cada prototipo
      sims <- map_dbl(prototipos_con_embeddings$embedding,
                      ~ cosine_sim(.x, emb_texto))

      # Crear tibble con categorías y similitudes
      prototipos_con_embeddings %>%
        mutate(similitud = sims) %>%
        group_by(categoria) %>%
        summarise(
          sim_promedio = mean(similitud),
          sim_max = max(similitud),
          .groups = "drop"
        ) %>%
        arrange(desc(sim_promedio))
    }),

    # map_chr(): extrae la categoría (string) con mayor similitud de cada elemento
    # Retorna un vector de caracteres con la predicción para cada texto
    pred_embeddings = map_chr(similitudes,
                               ~ if(is.null(.x)) NA_character_ else .x$categoria[1]),

    # map_dbl(): extrae la confianza (número) de cada predicción
    # Retorna un vector numérico con la similitud promedio de la categoría predicha
    confianza_embeddings = map_dbl(similitudes,
                                    ~ if(is.null(.x)) NA_real_ else .x$sim_promedio[1]),

    # map_dbl(): extrae la similitud con la categoría "positivo" para cada texto
    sim_positivo = map_dbl(similitudes, ~ {
      if(is.null(.x)) return(NA_real_)
      sim_cat <- .x %>% filter(categoria == "positivo")
      if(nrow(sim_cat) > 0) sim_cat$sim_promedio[1] else NA_real_
    }),

    # map_dbl(): extrae la similitud con la categoría "negativo" para cada texto
    sim_negativo = map_dbl(similitudes, ~ {
      if(is.null(.x)) return(NA_real_)
      sim_cat <- .x %>% filter(categoria == "negativo")
      if(nrow(sim_cat) > 0) sim_cat$sim_promedio[1] else NA_real_
    }),

    # map_dbl(): extrae la similitud con la categoría "neutro" para cada texto
    sim_neutro = map_dbl(similitudes, ~ {
      if(is.null(.x)) return(NA_real_)
      sim_cat <- .x %>% filter(categoria == "neutro")
      if(nrow(sim_cat) > 0) sim_cat$sim_promedio[1] else NA_real_
    })
  ) %>%
  select(-embedding, -similitudes)

# Calcular accuracy si existe etiqueta real ####

if ("etiqueta_real" %in% colnames(resultados)) {
  accuracy <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_embeddings)) %>%
    summarise(
      accuracy = mean(pred_embeddings == etiqueta_real, na.rm = TRUE),
      total = n(),
      correctos = sum(pred_embeddings == etiqueta_real, na.rm = TRUE)
    )

  # Matriz de confusión
  matriz_confusion <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_embeddings)) %>%
    count(etiqueta_real, pred_embeddings) %>%
    pivot_wider(names_from = pred_embeddings, values_from = n, values_fill = 0)

  # Métricas por clase
  metricas_clase <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_embeddings)) %>%
    group_by(etiqueta_real) %>%
    summarise(
      total = n(),
      correctos = sum(pred_embeddings == etiqueta_real),
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
    paste0("3_embeddings/3_embeddings_resultados_", METODO, ".xlsx")
  )
} else {
  # Guardar solo resultados
  write_xlsx(resultados, paste0("3_embeddings/3_embeddings_resultados_", METODO, ".xlsx"))
}

# Visualización de distribución de predicciones ####

resumen_pred <- resultados %>%
  filter(!is.na(pred_embeddings)) %>%
  count(pred_embeddings) %>%
  mutate(porcentaje = n / sum(n) * 100)

ggplot(resumen_pred, aes(x = pred_embeddings, y = n, fill = pred_embeddings)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
            vjust = -0.5) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = paste("Distribución de Sentimientos - Embeddings", toupper(METODO)),
    x = "Sentimiento",
    y = "Cantidad de comentarios"
  ) +
  theme_minimal()

ggsave(paste0("3_embeddings/3_distribucion_", METODO, ".png"), width = 10, height = 6, dpi = 300)

# Visualización de confianza de predicciones estática ####

ggplot(resultados, aes(x = id, y = confianza_embeddings, color = pred_embeddings)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = paste("Confianza de Predicciones - Embeddings", toupper(METODO)),
    x = "ID del comentario",
    y = "Similitud promedio con categoría",
    color = "Sentimiento"
  ) +
  theme_minimal()

ggsave(paste0("3_embeddings/3_confianza_", METODO, ".png"), width = 12, height = 6, dpi = 300)

# Visualización interactiva de confianza con plotly ####

grafica_confianza_interactiva <- plot_ly(
  data = resultados,
  x = ~id,
  y = ~confianza_embeddings,
  color = ~pred_embeddings,
  colors = c("positivo" = "#4CAF50", "negativo" = "#F44336", "neutro" = "#9E9E9E"),
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  # El texto que aparece en el tooltip al pasar el mouse
  text = ~paste0(
    "ID: ", id, "<br>",
    "Confianza: ", round(confianza_embeddings, 3), "<br>",
    "Predicción: ", pred_embeddings, "<br>",
    "Sim Positivo: ", round(sim_positivo, 3), "<br>",
    "Sim Negativo: ", round(sim_negativo, 3), "<br>",
    "Sim Neutro: ", round(sim_neutro, 3), "<br>",
    "Comentario: ", texto
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = paste("Confianza de Predicciones - Embeddings", toupper(METODO), "(Interactivo)"),
    xaxis = list(title = "ID del comentario"),
    yaxis = list(title = "Similitud promedio con categoría"),
    hovermode = "closest"
  )

htmlwidgets::saveWidget(grafica_confianza_interactiva, paste0("3_embeddings/3_confianza_interactivo_", METODO, ".html"), selfcontained = TRUE)

# Visualización de similitudes por categoría ####

datos_similitudes <- resultados %>%
  select(id, pred_embeddings, sim_positivo, sim_negativo, sim_neutro) %>%
  pivot_longer(
    cols = c(sim_positivo, sim_negativo, sim_neutro),
    names_to = "categoria",
    values_to = "similitud",
    names_prefix = "sim_"
  )

ggplot(datos_similitudes, aes(x = categoria, y = similitud, fill = categoria)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = paste("Distribución de Similitudes por Categoría - Embeddings", toupper(METODO)),
    x = "Categoría",
    y = "Similitud coseno",
    fill = "Categoría"
  ) +
  theme_minimal()

ggsave(paste0("3_embeddings/3_similitudes_boxplot_", METODO, ".png"), width = 10, height = 6, dpi = 300)

# Visualización de matriz de confusión ####

if ("etiqueta_real" %in% colnames(resultados)) {
  matriz_conf_long <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_embeddings)) %>%
    count(etiqueta_real, pred_embeddings) %>%
    group_by(etiqueta_real) %>%
    mutate(porcentaje = n / sum(n) * 100)

  ggplot(matriz_conf_long, aes(x = etiqueta_real, y = pred_embeddings, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
              color = "white", size = 4) +
    scale_fill_gradient(low = "#E3F2FD", high = "#1976D2") +
    labs(
      title = paste("Matriz de Confusión - Embeddings", toupper(METODO)),
      x = "Etiqueta Real",
      y = "Predicción",
      fill = "Cantidad"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  ggsave(paste0("3_embeddings/3_matriz_confusion_", METODO, ".png"), width = 8, height = 6, dpi = 300)
}
