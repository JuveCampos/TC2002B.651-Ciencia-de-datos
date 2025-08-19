# ====================================
# TIPO DE GRÁFICA: Mapa Coroplético
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)
library(sf)

# Establecer semilla para reproducibilidad
set.seed(987)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Indicador socioeconómico por estado
estados_mx <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
               "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila", "Colima", 
               "Durango", "Estado de México", "Guanajuato", "Guerrero", "Hidalgo", 
               "Jalisco", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", 
               "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", 
               "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

# Simular datos con patrón geográfico realista
set.seed(987)
datos_mapa <- tibble(
  estado = estados_mx,
  cve_ent = sprintf("%02d", 1:32),
  valor = c(
    # Norte (valores más altos)
    rnorm(7, 75, 8), # BC, BCS, Coahuila, Chihuahua, NL, Sonora, Tamaulipas
    # Centro (valores medios)
    rnorm(10, 65, 10), # AGS, CDMX, Colima, Edomex, Guanajuato, Hidalgo, Jalisco, Morelos, Querétaro, Tlaxcala
    # Sur (valores más bajos)
    rnorm(15, 45, 12) # Resto
  )[sample(32)]  # Mezclar para simular variabilidad
) %>%
  mutate(
    valor = pmax(pmin(valor, 95), 15), # Limitar entre 15 y 95
    categoria = case_when(
      valor >= 70 ~ "Alto",
      valor >= 50 ~ "Medio",
      TRUE ~ "Bajo"
    ),
    etiqueta = paste0(estado, "\n", round(valor, 1))
  )

# Para este ejemplo, creamos un shapefile simplificado simulado
# En un caso real, se usaría un shapefile de México
# Nota: Este es un ejemplo conceptual - en la práctica se necesitaría el shapefile real

# Función para crear polígonos de ejemplo (rectangulares para simplificación)
crear_estado_ejemplo <- function(cve, nombre, x_centro, y_centro, ancho = 1, alto = 1) {
  tibble(
    cve_ent = cve,
    estado = nombre,
    x = c(x_centro - ancho/2, x_centro + ancho/2, x_centro + ancho/2, x_centro - ancho/2, x_centro - ancho/2),
    y = c(y_centro - alto/2, y_centro - alto/2, y_centro + alto/2, y_centro + alto/2, y_centro - alto/2),
    grupo = paste0(cve, "_", 1:5)
  )
}

# Crear geometrías simplificadas para algunos estados clave
mapa_ejemplo <- bind_rows(
  crear_estado_ejemplo("09", "Ciudad de México", 0, 0, 0.8, 0.8),
  crear_estado_ejemplo("15", "Estado de México", 0, 1.2, 1.2, 1.0),
  crear_estado_ejemplo("19", "Nuevo León", 2, 3, 1.0, 1.0),
  crear_estado_ejemplo("14", "Jalisco", -2, 1, 1.2, 1.2),
  crear_estado_ejemplo("30", "Veracruz", 1, -1, 0.8, 2.0),
  crear_estado_ejemplo("07", "Chiapas", -1, -2.5, 1.5, 1.0)
) %>%
  left_join(datos_mapa %>% filter(estado %in% c("Ciudad de México", "Estado de México", 
                                                "Nuevo León", "Jalisco", "Veracruz", "Chiapas")), 
            by = c("cve_ent", "estado"))

# Colores para el mapa
colores_mapa <- colorRampPalette(c(mcv_semaforo[4], mcv_semaforo[3], mcv_semaforo[1]))(100)

# Etiquetas y títulos
titulo <- "Índice de Desarrollo Socioeconómico por Estado"
subtitulo <- "Medición estatal del indicador compuesto, 2024"

# VISUALIZACIÓN
g <- ggplot(mapa_ejemplo, aes(x = x, y = y, group = cve_ent, fill = valor)) +
  # Polígonos de estados
  geom_polygon(color = "white", linewidth = 0.5) +
  
  # Etiquetas de estados (solo para algunos)
  geom_text(
    data = mapa_ejemplo %>% group_by(cve_ent) %>% summarise(
      x = mean(x), y = mean(y), 
      valor = first(valor), estado = first(estado)
    ),
    aes(x = x, y = y, label = paste0(substr(estado, 1, 8), "\n", round(valor, 1))),
    inherit.aes = FALSE,
    family = "Ubuntu", fontface = "bold", size = 3.5, 
    color = "white"
  ) +
  
  # ESCALAS
  scale_fill_gradientn(
    name = "Índice",
    colors = colores_mapa,
    limits = c(min(datos_mapa$valor), max(datos_mapa$valor)),
    labels = number_format(accuracy = 1),
    guide = guide_colorbar(
      barwidth = 15, barheight = 0.8,
      title.position = "top", title.hjust = 0.5
    )
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    caption = "Elaborado por México, ¿cómo vamos? con datos simulados\nNota: Mapa simplificado para fines de ejemplo"
  ) +
  
  # TEMA
  theme_void() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", margin = margin(0,0,20,0), hjust = 0.5),
    plot.caption = element_text(size = 15, colour = mcv_blacks[3], hjust = 0.5, margin = margin(20,0,0,0)),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 15)
  ) +
  
  # Coordenadas iguales para mantener proporciones
  coord_equal()

# TABLA RESUMEN POR CATEGORÍAS
tabla_resumen <- datos_mapa %>%
  count(categoria, name = "n_estados") %>%
  mutate(
    porcentaje = round(n_estados / sum(n_estados) * 100, 1),
    etiqueta = paste0(categoria, ": ", n_estados, " estados (", porcentaje, "%)")
  )

cat("\\nRESUMEN POR CATEGORÍAS:\\n")
cat(paste(tabla_resumen$etiqueta, collapse = "\\n"))

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/mapa_coropletico.png", 
       width = 16, height = 12, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)
print(tabla_resumen)