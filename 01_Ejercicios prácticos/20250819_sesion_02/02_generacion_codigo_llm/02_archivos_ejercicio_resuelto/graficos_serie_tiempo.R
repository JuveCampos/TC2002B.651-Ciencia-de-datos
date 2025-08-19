# Script para Gráficos de Series de Tiempo de Pobreza por Entidad
# Autor: Programador R
# Fecha: 2025-08-19

# Cargar librerías necesarias (sin mensajes)
suppressPackageStartupMessages({
  library(ggplot2)      # Para gráficos elegantes
  library(dplyr)        # Para manipulación de datos
  library(readr)        # Para lectura eficiente de CSV
  library(scales)       # Para formateo de escalas
})

# ==============================================================================
# FUNCIÓN PRINCIPAL: GRÁFICO DE SERIE DE TIEMPO POR ESTADO
# ==============================================================================

#' Genera gráfico de serie de tiempo de indicadores de pobreza por estado
#' 
#' @param cve_estado Código de entidad (1-32) del estado a graficar
#' @param indicador Variable a graficar (por defecto "pobreza")
#' @param archivo_csv Ruta al archivo CSV (por defecto "df_pob_ent.csv")
#' @param mostrar_puntos Mostrar puntos en la serie (por defecto TRUE)
#' @param color_linea Color de la línea del gráfico (por defecto "steelblue")
#' 
#' @return Objeto ggplot con la serie de tiempo
#' 
#' @examples
#' # Gráfico de pobreza para el Estado de México (código 15)
#' grafico_serie_tiempo(15)
#' 
#' # Gráfico de pobreza extrema para Chiapas (código 7)
#' grafico_serie_tiempo(7, indicador = "pobreza_e")
#' 
grafico_serie_tiempo <- function(cve_estado, 
                                 indicador = "pobreza", 
                                 archivo_csv = "df_pob_ent.csv",
                                 mostrar_puntos = TRUE,
                                 color_linea = "steelblue") {
  
  # Validar parámetros de entrada
  if (!is.numeric(cve_estado) || cve_estado < 1 || cve_estado > 32) {
    stop("El código de estado debe ser un número entre 1 y 32")
  }
  
  # Intentar cargar los datos
  if (!file.exists(archivo_csv)) {
    stop(paste("No se encontró el archivo:", archivo_csv))
  }
  
  # Cargar datos
  cat("Cargando datos desde:", archivo_csv, "\n")
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Verificar que el indicador existe en los datos
  if (!indicador %in% names(datos)) {
    cat("Indicadores disponibles:\n")
    print(names(datos)[2:33])  # Mostrar columnas de indicadores
    stop(paste("El indicador", indicador, "no existe en los datos"))
  }
  
  # Filtrar datos para el estado específico
  datos_estado <- datos %>%
    filter(cve_ent == cve_estado) %>%
    arrange(anio)
  
  # Verificar que hay datos para ese estado
  if (nrow(datos_estado) == 0) {
    stop(paste("No se encontraron datos para el estado con código:", cve_estado))
  }
  
  # Obtener nombre del estado (mapeo básico)
  nombres_estados <- c(
    "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
    "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas",
    "8" = "Chihuahua", "9" = "Ciudad de México", "10" = "Durango",
    "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
    "15" = "Estado de México", "16" = "Michoacán", "17" = "Morelos",
    "18" = "Nayarit", "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla",
    "22" = "Querétaro", "23" = "Quintana Roo", "24" = "San Luis Potosí",
    "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco", "28" = "Tamaulipas",
    "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán", "32" = "Zacatecas"
  )
  
  nombre_estado <- nombres_estados[as.character(cve_estado)]
  if (is.na(nombre_estado)) nombre_estado <- paste("Estado", cve_estado)
  
  # Crear etiquetas más legibles para indicadores
  etiquetas_indicadores <- c(
    "pobreza" = "Pobreza (%)",
    "pobreza_e" = "Pobreza Extrema (%)",
    "pobreza_m" = "Pobreza Moderada (%)",
    "vul_car" = "Vulnerabilidad por Carencias (%)",
    "vul_ing" = "Vulnerabilidad por Ingresos (%)",
    "carencias" = "Población con Carencias (%)",
    "ic_rezedu" = "Rezago Educativo (%)",
    "ic_asalud" = "Sin Acceso a Salud (%)",
    "ic_segsoc" = "Sin Seguridad Social (%)",
    "ic_cv" = "Calidad Vivienda (%)",
    "ic_sbv" = "Servicios Básicos Vivienda (%)",
    "ic_ali_nc" = "Acceso a Alimentación (%)"
  )
  
  etiqueta_y <- etiquetas_indicadores[[indicador]]
  if (is.null(etiqueta_y)) etiqueta_y <- indicador
  
  # Mostrar información sobre los datos
  cat(strrep("=", 60), "\n")
  cat("INFORMACIÓN DEL DATASET\n")
  cat(strrep("=", 60), "\n")
  cat("Estado:", nombre_estado, "(Código:", cve_estado, ")\n")
  cat("Indicador:", etiqueta_y, "\n")
  cat("Años disponibles:", min(datos_estado$anio), "-", max(datos_estado$anio), "\n")
  cat("Número de observaciones:", nrow(datos_estado), "\n")
  cat("Rango de valores:", round(min(datos_estado[[indicador]], na.rm = TRUE), 2), 
      "-", round(max(datos_estado[[indicador]], na.rm = TRUE), 2), "\n")
  
  # Crear el gráfico
  p <- ggplot(datos_estado, aes(x = anio, y = .data[[indicador]])) +
    geom_line(color = color_linea, linewidth = 1.2) +
    theme_minimal() +
    labs(
      title = paste("Serie de Tiempo:", etiqueta_y),
      subtitle = paste("Estado:", nombre_estado, "| Período:", 
                      min(datos_estado$anio), "-", max(datos_estado$anio)),
      x = "Año",
      y = etiqueta_y,
      caption = "Fuente: df_pob_ent.csv"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.major.y = element_line(color = "grey90")
    ) +
    scale_x_continuous(breaks = datos_estado$anio, 
                      labels = datos_estado$anio) +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  # Agregar puntos si se solicita
  if (mostrar_puntos) {
    p <- p + geom_point(color = color_linea, size = 3)
  }
  
  # Agregar anotaciones de valores en los puntos
  p <- p + geom_text(aes(label = paste0(round(.data[[indicador]], 1), "%")),
                     vjust = -0.8, size = 3.5, color = "gray30")
  
  return(p)
}

# ==============================================================================
# FUNCIÓN AUXILIAR: COMPARAR MÚLTIPLES ESTADOS
# ==============================================================================

#' Compara un indicador entre múltiples estados en serie de tiempo
#' 
#' @param estados_codigos Vector de códigos de entidades a comparar
#' @param indicador Variable a graficar
#' @param archivo_csv Ruta al archivo CSV
#' 
#' @return Objeto ggplot con múltiples series
#' 
comparar_estados <- function(estados_codigos, 
                           indicador = "pobreza",
                           archivo_csv = "df_pob_ent.csv") {
  
  # Cargar datos
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Nombres de estados
  nombres_estados <- c(
    "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
    "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas",
    "8" = "Chihuahua", "9" = "Ciudad de México", "10" = "Durango",
    "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
    "15" = "Estado de México", "16" = "Michoacán", "17" = "Morelos",
    "18" = "Nayarit", "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla",
    "22" = "Querétaro", "23" = "Quintana Roo", "24" = "San Luis Potosí",
    "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco", "28" = "Tamaulipas",
    "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán", "32" = "Zacatecas"
  )
  
  # Filtrar y preparar datos
  datos_comparacion <- datos %>%
    filter(cve_ent %in% estados_codigos) %>%
    mutate(estado_nombre = nombres_estados[as.character(cve_ent)]) %>%
    arrange(anio)
  
  # Etiquetas para indicadores
  etiquetas_indicadores <- c(
    "pobreza" = "Pobreza (%)",
    "pobreza_e" = "Pobreza Extrema (%)",
    "pobreza_m" = "Pobreza Moderada (%)"
  )
  
  etiqueta_y <- etiquetas_indicadores[[indicador]]
  if (is.null(etiqueta_y)) etiqueta_y <- indicador
  
  # Crear gráfico de comparación
  p <- ggplot(datos_comparacion, aes(x = anio, y = .data[[indicador]], 
                                    color = estado_nombre)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    theme_minimal() +
    labs(
      title = paste("Comparación de", etiqueta_y, "entre Estados"),
      x = "Año",
      y = etiqueta_y,
      color = "Estado",
      caption = "Fuente: df_pob_ent.csv"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) +
    scale_x_continuous(breaks = unique(datos_comparacion$anio)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  return(p)
}

# ==============================================================================
# FUNCIÓN PARA MOSTRAR ESTADÍSTICAS RESUMEN
# ==============================================================================

#' Muestra estadísticas resumidas para un estado
#' 
#' @param cve_estado Código de entidad del estado
#' @param archivo_csv Ruta al archivo CSV
#' 
estadisticas_estado <- function(cve_estado, archivo_csv = "df_pob_ent.csv") {
  
  # Cargar datos
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Filtrar para el estado
  datos_estado <- datos %>%
    filter(cve_ent == cve_estado) %>%
    arrange(anio)
  
  # Nombres de estados
  nombres_estados <- c(
    "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
    "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas",
    "8" = "Chihuahua", "9" = "Ciudad de México", "10" = "Durango",
    "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
    "15" = "Estado de México", "16" = "Michoacán", "17" = "Morelos",
    "18" = "Nayarit", "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla",
    "22" = "Querétaro", "23" = "Quintana Roo", "24" = "San Luis Potosí",
    "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco", "28" = "Tamaulipas",
    "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán", "32" = "Zacatecas"
  )
  
  nombre_estado <- nombres_estados[as.character(cve_estado)]
  
  cat(strrep("=", 60), "\n")
  cat("ESTADÍSTICAS RESUMEN -", nombre_estado, "\n")
  cat(strrep("=", 60), "\n")
  
  # Indicadores principales
  indicadores <- c("pobreza", "pobreza_e", "pobreza_m", "vul_car", "vul_ing")
  nombres_ind <- c("Pobreza Total", "Pobreza Extrema", "Pobreza Moderada", 
                   "Vuln. Carencias", "Vuln. Ingresos")
  
  for (i in 1:length(indicadores)) {
    valores <- datos_estado[[indicadores[i]]]
    cat(sprintf("%-20s: Promedio %.1f%% | Rango: %.1f%% - %.1f%%\n",
                nombres_ind[i],
                mean(valores, na.rm = TRUE),
                min(valores, na.rm = TRUE),
                max(valores, na.rm = TRUE)))
  }
  
  # Tendencia de pobreza
  if (nrow(datos_estado) > 1) {
    cambio_pobreza <- datos_estado$pobreza[nrow(datos_estado)] - datos_estado$pobreza[1]
    cat("\nTendencia pobreza total:", 
        ifelse(cambio_pobreza > 0, "↗ Aumento", "↘ Reducción"),
        "de", round(abs(cambio_pobreza), 1), "puntos porcentuales\n")
  }
}

# ==============================================================================
# EJEMPLOS DE USO
# ==============================================================================

cat("Funciones cargadas exitosamente!\n")
cat(strrep("=", 50), "\n")
cat("EJEMPLOS DE USO:\n")
cat(strrep("=", 50), "\n")
cat("# Gráfico básico de pobreza para Chiapas (código 7):\n")
cat("grafico_serie_tiempo(7)\n\n")
cat("# Pobreza extrema para Estado de México (código 15):\n")
cat("grafico_serie_tiempo(15, indicador = 'pobreza_e')\n\n")
cat("# Comparar pobreza entre 3 estados:\n")
cat("comparar_estados(c(7, 15, 19), indicador = 'pobreza')\n\n")
cat("# Ver estadísticas de un estado:\n")
cat("estadisticas_estado(7)\n\n")
cat("Estados disponibles: 1-32 (Aguascalientes a Zacatecas)\n")
cat("Indicadores principales: pobreza, pobreza_e, pobreza_m, vul_car, vul_ing\n")

# ==============================================================================
# EJEMPLO EJECUTABLE AUTOMÁTICO
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EJECUTANDO EJEMPLO: Serie de Tiempo - Morelos (Código 17)\n")
cat(strrep("=", 60), "\n")

# Verificar si el archivo existe antes de ejecutar
if (file.exists("df_pob_ent.csv")) {
  tryCatch({
    # Generar gráfico de ejemplo para Morelos (código 17)
    cat("Generando gráfico de serie de tiempo...\n")
    p_ejemplo <- grafico_serie_tiempo(17, "pobreza")
    print(p_ejemplo)
    
    cat("\n¡Gráfico generado exitosamente!\n")
    cat("Para ver más ejemplos, ejecute: source('ejemplo_uso.R')\n")
    
  }, error = function(e) {
    cat("Error al generar el gráfico:", conditionMessage(e), "\n")
    cat("Detalle del error:", class(e)[1], "\n")
    cat("Verifique que el archivo df_pob_ent.csv esté en el directorio actual\n")
  })
} else {
  cat("ATENCIÓN: El archivo df_pob_ent.csv no se encuentra en el directorio actual.\n")
  cat("Por favor, asegúrese de que el archivo esté en el mismo directorio que este script.\n")
}
