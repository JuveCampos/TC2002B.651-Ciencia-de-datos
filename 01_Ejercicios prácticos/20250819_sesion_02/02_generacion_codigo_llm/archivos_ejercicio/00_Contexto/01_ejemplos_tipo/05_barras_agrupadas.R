# ====================================
# TIPO DE GRÁFICA: Barras Agrupadas
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)

# Establecer semilla para reproducibilidad
set.seed(654)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Participación laboral por sexo y entidad
entidades <- c("Nacional", "CDMX", "Nuevo León", "Jalisco", "Quintana Roo", "Chiapas")

datos_agrupados <- tibble(
  entidad = rep(entidades, each = 2),
  sexo = rep(c("Hombres", "Mujeres"), length(entidades)),
  participacion = c(
    # Nacional
    77.8, 45.2,
    # CDMX  
    75.3, 48.7,
    # Nuevo León
    79.1, 46.8,
    # Jalisco
    78.5, 44.9,
    # Quintana Roo
    81.2, 49.3,
    # Chiapas
    76.9, 38.4
  )
) %>%
  mutate(
    entidad = factor(entidad, levels = entidades),
    brecha = ifelse(sexo == "Hombres", NA, participacion - lead(participacion)),
    destacar_nacional = ifelse(entidad == "Nacional", "sí", "no")
  )

# Calcular brecha por entidad
brechas <- datos_agrupados %>%
  group_by(entidad) %>%
  summarise(
    brecha = first(participacion) - last(participacion),
    pos_x = first(entidad),
    pos_y = max(participacion) + 2
  )

# Colores por sexo
colores_sexo <- c("Hombres" = mcv_discrete[3], "Mujeres" = mcv_discrete[1])

# Etiquetas y títulos
titulo <- "Tasa de Participación Laboral por Sexo"
subtitulo <- "Porcentaje de la población económicamente activa por entidad, 2024"
eje_y <- "Tasa de participación (%)"

# VISUALIZACIÓN
g <- ggplot(
  datos_agrupados,
  aes(x = entidad, y = participacion, fill = sexo)
) +
  # Barras agrupadas
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  
  # Etiquetas de valores en las barras
  geom_text(
    aes(label = paste0(participacion, "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5, family = "Ubuntu", fontface = "bold",
    size = 4, color = mcv_blacks[1]
  ) +
  
  # Etiquetas de brecha de género
  geom_text(
    data = brechas,
    aes(x = pos_x, y = pos_y, label = paste0("Brecha: ", round(brecha, 1), "pp")),
    inherit.aes = FALSE,
    family = "Ubuntu", color = mcv_blacks[3], size = 3.5,
    fontface = "italic"
  ) +
  
  # Línea de referencia para promedio nacional
  geom_hline(
    data = datos_agrupados %>% filter(entidad == "Nacional"),
    aes(yintercept = participacion, color = sexo),
    linetype = "dashed", linewidth = 0.8, alpha = 0.7
  ) +
  
  # ESCALAS
  scale_fill_manual(name = "", values = colores_sexo) +
  scale_color_manual(values = colores_sexo, guide = "none") +
  scale_y_continuous(
    expand = expansion(c(0, 0.15)),
    breaks = pretty_breaks(n = 6),
    labels = percent_format(scale = 1)
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    y = eje_y,
    x = NULL,
    caption = "Elaborado por México, ¿cómo vamos? con datos simulados\npp = puntos porcentuales"
  ) +
  
  # TEMA
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 30, colour = "#777777", margin = margin(0,0,10,0)),
    plot.caption = element_text(size = 15, colour = mcv_blacks[3]),
    plot.margin = margin(0.3, 0.3, 1.4, 0.3, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.margin = margin(t = 20),
    axis.line.x = element_line(color = mcv_blacks[2])
  )

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/barras_agrupadas.png", 
       width = 16, height = 9, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)