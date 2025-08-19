# ====================================
# TIPO DE GRÁFICA: Series de Tiempo - Línea
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)

# Establecer semilla para reproducibilidad
set.seed(123)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Indicador económico mexicano típico
fechas <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
n_obs <- length(fechas)

# Simular serie con tendencia y estacionalidad (como IGAE, inflación, etc.)
trend <- seq(100, 115, length.out = n_obs)
seasonal <- 3 * sin(2 * pi * (1:n_obs) / 12)
noise <- rnorm(n_obs, 0, 1.5)

datos_serie <- tibble(
  fecha = fechas,
  valores = trend + seasonal + noise,
  anio = year(fecha),
  mes = month(fecha)
) %>%
  # Simular efecto COVID (caída en 2020)
  mutate(
    valores = ifelse(
      fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-06-01"),
      valores - runif(n(), 8, 12),
      valores
    )
  )

# Etiquetas y títulos
titulo <- "Evolución del Indicador Económico Simulado"
subtitulo <- paste0("Serie mensual, ", 
                   format(max(datos_serie$fecha), "%B %Y"))
eje_y <- "Índice base 2018=100"

# VISUALIZACIÓN
g <- ggplot(
  datos_serie,
  aes(x = fecha, y = valores, group = 1)
) +
  # Línea principal
  geom_line(linewidth = 1.5, color = mcv_discrete[1], lineend = "round") +
  
  # Punto destacado en último valor
  geom_point(
    aes(y = ifelse(fecha == max(fecha), valores, NA)),
    size = 4, color = mcv_discrete[1], fill = "white",
    stroke = 2, shape = 21
  ) +
  
  # Etiqueta del último valor
  geom_text(
    aes(
      label = ifelse(fecha == max(fecha), 
                    paste0(format(fecha, "%b-%y"), "\n", round(valores, 1)), 
                    NA)
    ),
    vjust = -0.5, hjust = 0.5, family = "Ubuntu", 
    fontface = "bold", size = 5, color = mcv_discrete[1]
  ) +
  
  # Línea de referencia en 100
  geom_hline(yintercept = 100, linetype = "dashed", 
             color = mcv_semaforo[3], linewidth = 1) +
  
  # Área sombreada para período COVID
  annotate("rect", 
           xmin = as.Date("2020-03-01"), xmax = as.Date("2020-07-01"),
           ymin = -Inf, ymax = Inf, 
           fill = mcv_semaforo[4], alpha = 0.2) +
  
  # Anotación COVID
  annotate("text", x = as.Date("2020-05-01"), y = max(datos_serie$valores) - 2,
           label = "Período\nCOVID-19", family = "Ubuntu", 
           fontface = "bold", color = mcv_semaforo[4], size = 4) +
  
  # ESCALAS
  scale_x_date(
    breaks = seq.Date(min(datos_serie$fecha), max(datos_serie$fecha), by = "6 month"),
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    expand = expansion(c(0.05, 0.1)),
    breaks = pretty_breaks(n = 6)
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
    axis.text.x = element_text(size = 20, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 20)
  )

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/serie_tiempo_linea.png", 
       width = 16, height = 9, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)