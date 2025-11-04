# Clasificación de Sentimientos con LLMs ####

library(tidyverse)
library(readxl)
library(writexl)
library(httr2)
library(jsonlite)
library(plotly)

# Configuración ####

# Seleccionar método: "ollama" o "openai"
METODO <- "ollama"

# Si usas Ollama, selecciona el modelo: "llama3.1:latest" o "qwen2.5:latest"
MODELO_OLLAMA <- "llama3.1:latest"

# Configuración de Ollama
OLLAMA_HOST <- "http://localhost:11434"

# Configuración de OpenAI
# Si usas OpenAI, configura tu API key aquí o en variable de entorno
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
MODELO_OPENAI <- "gpt-4o-mini"

# Cargar datos ####

datos <- read_excel("legacy/dataset_sentimientos.xlsx") %>%
  select(-pred_lexicon, -pred_emb_local, -pred_emb_openai, -pred_llama31, -pred_qwen25)

# Función para clasificar con Ollama ####

clasificar_ollama <- function(texto, model = MODELO_OLLAMA, host = OLLAMA_HOST) {
  # Crear prompt para clasificación
  prompt <- paste0(
    "Clasifica el siguiente texto en una de estas categorías: positivo, negativo, neutro.\n\n",
    "Texto: \"", texto, "\"\n\n",
    "Responde SOLO con una palabra: positivo, negativo o neutro.\n",
    "Respuesta:"
  )

  # Hacer petición a Ollama
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

  # Extraer sentimiento de la respuesta
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

# Función para clasificar con OpenAI ####

clasificar_openai <- function(texto, model = MODELO_OPENAI, api_key = OPENAI_API_KEY) {
  # Crear prompt para clasificación
  prompt <- paste0(
    "Clasifica el siguiente texto en una de estas categorías: positivo, negativo, neutro.\n\n",
    "Texto: \"", texto, "\"\n\n",
    "Responde SOLO con una palabra: positivo, negativo o neutro."
  )

  # Hacer petición a OpenAI
  req <- request("https://api.openai.com/v1/chat/completions") %>%
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = "Eres un clasificador de sentimientos. Responde solo con: positivo, negativo o neutro."),
        list(role = "user", content = prompt)
      ),
      temperature = 0.1,
      max_tokens = 10
    )) %>%
    req_perform()

  resp <- resp_body_json(req)
  respuesta <- tolower(trimws(resp$choices[[1]]$message$content))

  # Extraer sentimiento de la respuesta
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

# Clasificar textos ####

if (METODO == "ollama") {
  # Usar Ollama para clasificación
  resultados <- datos %>%
    mutate(
      # map_chr(): aplica la función clasificar_ollama() a cada texto y retorna vector de caracteres
      # ~ indica función anónima donde .x representa cada elemento del vector texto
      # possibly(): envuelve la función para retornar NA en caso de error
      # .progress = TRUE: muestra barra de progreso durante la ejecución
      # Resultado: para cada comentario, obtenemos su clasificación (positivo/negativo/neutro)
      pred_llm = map_chr(
        texto,
        possibly(~ clasificar_ollama(.x), NA_character_),
        .progress = TRUE
      )
    )

} else if (METODO == "openai") {
  # Usar OpenAI para clasificación
  resultados <- datos %>%
    mutate(
      # map_chr(): aplica la función clasificar_openai() a cada texto
      # Funciona igual que en el caso de Ollama pero usando la API de OpenAI
      pred_llm = map_chr(
        texto,
        possibly(~ clasificar_openai(.x), NA_character_),
        .progress = TRUE
      )
    )
}

# Calcular accuracy si existe etiqueta real ####

if ("etiqueta_real" %in% colnames(resultados)) {
  accuracy <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    summarise(
      accuracy = mean(pred_llm == etiqueta_real, na.rm = TRUE),
      total = n(),
      correctos = sum(pred_llm == etiqueta_real, na.rm = TRUE)
    )

  # Matriz de confusión
  matriz_confusion <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    count(etiqueta_real, pred_llm) %>%
    pivot_wider(names_from = pred_llm, values_from = n, values_fill = 0)

  # Métricas por clase
  metricas_clase <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    group_by(etiqueta_real) %>%
    summarise(
      total = n(),
      correctos = sum(pred_llm == etiqueta_real),
      precision = correctos / total
    )

  # Análisis por tipo de caso
  if ("tipo_caso" %in% colnames(resultados)) {
    metricas_tipo <- resultados %>%
      filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
      group_by(tipo_caso) %>%
      summarise(
        total = n(),
        correctos = sum(pred_llm == etiqueta_real),
        accuracy = correctos / total
      )
  } else {
    metricas_tipo <- NULL
  }

  # Guardar resultados con métricas
  if (!is.null(metricas_tipo)) {
    write_xlsx(
      list(
        resultados = resultados,
        accuracy = accuracy,
        matriz_confusion = matriz_confusion,
        metricas_clase = metricas_clase,
        metricas_tipo = metricas_tipo
      ),
      paste0("4_llms/4_llm_resultados_", METODO,
             if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
             ".xlsx")
    )
  } else {
    write_xlsx(
      list(
        resultados = resultados,
        accuracy = accuracy,
        matriz_confusion = matriz_confusion,
        metricas_clase = metricas_clase
      ),
      paste0("4_llms/4_llm_resultados_", METODO,
             if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
             ".xlsx")
    )
  }
} else {
  # Guardar solo resultados
  write_xlsx(
    resultados,
    paste0("4_llms/4_llm_resultados_", METODO,
           if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
           ".xlsx")
  )
}

# Visualización de distribución de predicciones ####

resumen_pred <- resultados %>%
  filter(!is.na(pred_llm)) %>%
  count(pred_llm) %>%
  mutate(porcentaje = n / sum(n) * 100)

nombre_modelo <- if (METODO == "ollama") MODELO_OLLAMA else MODELO_OPENAI

ggplot(resumen_pred, aes(x = pred_llm, y = n, fill = pred_llm)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
            vjust = -0.5) +
  scale_fill_manual(values = c(
    "positivo" = "#4CAF50",
    "negativo" = "#F44336",
    "neutro" = "#9E9E9E"
  )) +
  labs(
    title = paste("Distribución de Sentimientos - LLM", toupper(METODO)),
    subtitle = paste("Modelo:", nombre_modelo),
    x = "Sentimiento",
    y = "Cantidad de comentarios"
  ) +
  theme_minimal()

ggsave(
  paste0("4_llms/4_distribucion_", METODO,
         if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
         ".png"),
  width = 10, height = 6, dpi = 300
)

# Visualización de matriz de confusión ####

if ("etiqueta_real" %in% colnames(resultados)) {
  matriz_conf_long <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    count(etiqueta_real, pred_llm) %>%
    group_by(etiqueta_real) %>%
    mutate(porcentaje = n / sum(n) * 100)

  ggplot(matriz_conf_long, aes(x = etiqueta_real, y = pred_llm, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(n, "\n(", round(porcentaje, 1), "%)")),
              color = "white", size = 4) +
    scale_fill_gradient(low = "#E3F2FD", high = "#1976D2") +
    labs(
      title = paste("Matriz de Confusión - LLM", toupper(METODO)),
      subtitle = paste("Modelo:", nombre_modelo),
      x = "Etiqueta Real",
      y = "Predicción",
      fill = "Cantidad"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  ggsave(
    paste0("4_llms/4_matriz_confusion_", METODO,
           if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
           ".png"),
    width = 8, height = 6, dpi = 300
  )
}

# Visualización de accuracy por tipo de caso ####

if ("etiqueta_real" %in% colnames(resultados) && "tipo_caso" %in% colnames(resultados)) {
  metricas_tipo_viz <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    group_by(tipo_caso) %>%
    summarise(
      total = n(),
      correctos = sum(pred_llm == etiqueta_real),
      accuracy = correctos / total,
      .groups = "drop"
    )

  ggplot(metricas_tipo_viz, aes(x = reorder(tipo_caso, accuracy), y = accuracy, fill = tipo_caso)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d/%d)", accuracy * 100, correctos, total)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    ylim(0, 1.1) +
    labs(
      title = paste("Accuracy por Tipo de Caso - LLM", toupper(METODO)),
      subtitle = paste("Modelo:", nombre_modelo),
      x = "Tipo de caso",
      y = "Accuracy"
    ) +
    theme_minimal()

  ggsave(
    paste0("4_llms/4_accuracy_tipo_", METODO,
           if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
           ".png"),
    width = 10, height = 6, dpi = 300
  )
}

# Visualización comparativa de precisión por clase ####

if ("etiqueta_real" %in% colnames(resultados)) {
  metricas_clase_viz <- resultados %>%
    filter(!is.na(etiqueta_real), !is.na(pred_llm)) %>%
    group_by(etiqueta_real) %>%
    summarise(
      total = n(),
      correctos = sum(pred_llm == etiqueta_real),
      precision = correctos / total,
      .groups = "drop"
    )

  ggplot(metricas_clase_viz, aes(x = etiqueta_real, y = precision, fill = etiqueta_real)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d/%d)", precision * 100, correctos, total)),
              vjust = -0.5) +
    scale_fill_manual(values = c(
      "positivo" = "#4CAF50",
      "negativo" = "#F44336",
      "neutro" = "#9E9E9E"
    )) +
    ylim(0, 1.1) +
    labs(
      title = paste("Precisión por Clase - LLM", toupper(METODO)),
      subtitle = paste("Modelo:", nombre_modelo),
      x = "Clase",
      y = "Precisión"
    ) +
    theme_minimal()

  ggsave(
    paste0("4_llms/4_precision_clase_", METODO,
           if(METODO == "ollama") paste0("_", str_replace_all(MODELO_OLLAMA, ":", "_")) else "",
           ".png"),
    width = 10, height = 6, dpi = 300
  )
}
