# ====================================
# TIPO DE GRÁFICA: Series de Tiempo Múltiples
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)
library(ggrepel)

# Establecer semilla para reproducibilidad
set.seed(321)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Comparación México vs otros países
fechas <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
n_obs <- length(fechas)

# Simular datos para México, EUA y promedio OCDE
set.seed(321)
datos_multiples <- tibble(
  fecha = rep(fechas, 3),
  pais = rep(c("México", "Estados Unidos", "Promedio OCDE"), each = n_obs),
  valor = c(
    # México - más volátil
    2.5 + cumsum(rnorm(n_obs, 0, 0.3)) + 2*sin(2*pi*(1:n_obs)/12),
    # Estados Unidos - más estable
    2.0 + cumsum(rnorm(n_obs, 0, 0.2)) + 1*sin(2*pi*(1:n_obs)/12),
    # Promedio OCDE - intermedio
    2.2 + cumsum(rnorm(n_obs, 0, 0.25)) + 1.5*sin(2*pi*(1:n_obs)/12)
  )
) %>%
  # Ajustar para que todos terminen en rangos realistas
  group_by(pais) %>%
  mutate(
    valor = pmax(valor, 0.5), # Mínimo 0.5%
    valor = pmin(valor, 8.0)   # Máximo 8.0%
  ) %>%
  ungroup() %>%
  # Simular efecto COVID
  mutate(
    valor = case_when(
      fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-06-01") & pais == "México" ~ valor + 1.5,
      fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-06-01") & pais == "Estados Unidos" ~ valor + 0.8,
      fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-06-01") & pais == "Promedio OCDE" ~ valor + 1.0,
      TRUE ~ valor
    )
  )

# Colores específicos para cada país
colores_paises <- c("México" = mcv_discrete[4], 
                   "Estados Unidos" = mcv_discrete[3], 
                   "Promedio OCDE" = mcv_discrete[2])

# Etiquetas y títulos
titulo <- "Tasa de Inflación: Comparación Internacional"
subtitulo <- paste0("Serie mensual, ", format(max(datos_multiples$fecha), "%B %Y"))
eje_y <- "Tasa anual (%)"

# VISUALIZACIÓN
g <- ggplot(
  datos_multiples,
  aes(x = fecha, y = valor, color = pais, group = pais)
) +
  # Líneas principales
  geom_line(linewidth = 1.8, alpha = 0.9) +
  
  # Puntos en último valor
  geom_point(
    data = datos_multiples %>% 
      group_by(pais) %>% 
      filter(fecha == max(fecha)),
    size = 4, stroke = 2, fill = "white", shape = 21
  ) +
  
  # Etiquetas de último valor con repel
  geom_text_repel(
    data = datos_multiples %>% 
      group_by(pais) %>% 
      filter(fecha == max(fecha)),
    aes(label = paste0(pais, "\n", round(valor, 1), "%")),
    nudge_x = 30, direction = "y", hjust = 0,
    family = "Ubuntu", fontface = "bold", size = 5,
    show.legend = FALSE
  ) +
  
  # Línea de referencia en 3% (meta típica de inflación)
  geom_hline(yintercept = 3, linetype = "dashed", 
             color = mcv_blacks[3], linewidth = 1) +
  
  # Área sombreada para período COVID
  annotate("rect", 
           xmin = as.Date("2020-03-01"), xmax = as.Date("2020-07-01"),
           ymin = -Inf, ymax = Inf, 
           fill = mcv_semaforo[4], alpha = 0.15) +
  
  # Anotación de meta de inflación
  annotate("text", x = as.Date("2018-06-01"), y = 3.3,
           label = "Meta de inflación (3%)", family = "Ubuntu", 
           color = mcv_blacks[3], size = 4) +
  
  # ESCALAS
  scale_color_manual(name = "", values = colores_paises) +
  scale_x_date(
    breaks = seq.Date(min(datos_multiples$fecha), max(datos_multiples$fecha), by = "6 month"),
    date_labels = "%b %Y",
    expand = expansion(c(0.02, 0.15))
  ) +
  scale_y_continuous(
    expand = expansion(c(0.05, 0.1)),
    breaks = pretty_breaks(n = 6),
    labels = percent_format(scale = 1)
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    y = eje_y,
    x = NULL,
    caption = "Elaborado por México, ¿cómo vamos? con datos simulados"
  ) +
  
  # TEMA
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 30, colour = "#777777", margin = margin(0,0,10,0)),
    plot.caption = element_text(size = 15, colour = mcv_blacks[3]),
    plot.margin = margin(0.3, 0.3, 1.4, 0.3, "cm"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 20),
    legend.position = "none" # Usamos etiquetas directas en lugar de leyenda
  )

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/series_multiples.png", 
       width = 16, height = 9, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)