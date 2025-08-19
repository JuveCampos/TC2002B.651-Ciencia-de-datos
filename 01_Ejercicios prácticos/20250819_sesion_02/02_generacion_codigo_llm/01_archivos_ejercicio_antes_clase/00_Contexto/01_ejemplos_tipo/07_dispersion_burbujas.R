# ====================================
# TIPO DE GRÁFICA: Gráfico de Dispersión con Burbujas
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)
library(ggrepel)

# Establecer semilla para reproducibilidad
set.seed(147)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Relación PIB per cápita, Educación y Población por estado
estados_select <- c("Ciudad de México", "Nuevo León", "Querétaro", "Baja California Sur",
                   "Sonora", "Coahuila", "Jalisco", "Aguascalientes", "Estado de México",
                   "Puebla", "Veracruz", "Michoacán", "Oaxaca", "Chiapas", "Guerrero")

set.seed(147)
datos_burbujas <- tibble(
  estado = estados_select,
  pib_per_capita = c(
    # Estados con PIB alto
    320, 280, 180, 170, 160, 155, 140, 135, 120,
    # Estados con PIB medio-bajo
    95, 85, 78, 65, 58, 52
  ) * runif(15, 0.9, 1.1), # Añadir variabilidad
  educacion_promedio = c(
    # Años de educación promedio
    11.2, 10.8, 10.1, 9.8, 9.5, 9.7, 9.2, 9.9, 8.8,
    8.2, 7.9, 7.5, 6.8, 6.2, 5.9
  ) + rnorm(15, 0, 0.3), # Añadir ruido
  poblacion_millones = c(
    # Población en millones
    9.2, 5.8, 2.4, 0.8, 3.0, 3.2, 8.3, 1.4, 17.4,
    6.6, 8.1, 4.7, 4.1, 5.5, 3.5
  ),
  region = c(
    "Centro", "Norte", "Centro", "Norte", "Norte", "Norte", "Centro", "Centro", "Centro",
    "Centro", "Sur", "Centro", "Sur", "Sur", "Sur"
  )
) %>%
  mutate(
    # Clasificar por tamaño de población
    tam_poblacion = case_when(
      poblacion_millones >= 8 ~ "Grande",
      poblacion_millones >= 3 ~ "Mediana", 
      TRUE ~ "Pequeña"
    ),
    # Etiquetas solo para casos relevantes
    etiqueta = case_when(
      estado %in% c("Ciudad de México", "Nuevo León", "Estado de México", "Chiapas", "Oaxaca") ~ estado,
      TRUE ~ ""
    )
  )

# Colores por región
colores_region <- c("Norte" = mcv_discrete[3], "Centro" = mcv_discrete[1], "Sur" = mcv_discrete[4])

# Etiquetas y títulos
titulo <- "Relación entre Desarrollo Económico y Educativo"
subtitulo <- "PIB per cápita vs. años promedio de educación por entidad federativa, 2024"
eje_x <- "Años promedio de educación"
eje_y <- "PIB per cápita (miles de pesos)"

# VISUALIZACIÓN
g <- ggplot(
  datos_burbujas,
  aes(x = educacion_promedio, y = pib_per_capita, 
      size = poblacion_millones, color = region, fill = region)
) +
  # Burbujas principales
  geom_point(alpha = 0.7, stroke = 1.5, shape = 21) +
  
  # Línea de tendencia
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, 
              color = mcv_blacks[3], fill = mcv_blacks[2], 
              alpha = 0.3, linewidth = 1, linetype = "dashed") +
  
  # Etiquetas para estados destacados
  geom_text_repel(
    aes(label = etiqueta),
    size = 4, family = "Ubuntu", fontface = "bold",
    box.padding = 0.5, point.padding = 0.3,
    color = mcv_blacks[1], show.legend = FALSE
  ) +
  
  # Líneas de referencia
  geom_vline(xintercept = mean(datos_burbujas$educacion_promedio), 
             linetype = "dotted", color = mcv_blacks[3], alpha = 0.7) +
  geom_hline(yintercept = mean(datos_burbujas$pib_per_capita), 
             linetype = "dotted", color = mcv_blacks[3], alpha = 0.7) +
  
  # Anotaciones de cuadrantes
  annotate("text", x = 5.5, y = 280, label = "Bajo desarrollo\neducativo,\nalto PIB", 
           family = "Ubuntu", size = 3.5, color = mcv_blacks[3], alpha = 0.8) +
  annotate("text", x = 11, y = 60, label = "Alto desarrollo\neducativo,\nbajo PIB", 
           family = "Ubuntu", size = 3.5, color = mcv_blacks[3], alpha = 0.8) +
  
  # ESCALAS
  scale_color_manual(name = "Región:", values = colores_region) +
  scale_fill_manual(name = "Región:", values = colores_region) +
  scale_size_continuous(
    name = "Población\\n(millones):",
    range = c(3, 15),
    breaks = c(1, 5, 10, 15),
    labels = c("1", "5", "10", "15+")
  ) +
  scale_x_continuous(
    expand = expansion(c(0.1, 0.1)),
    breaks = pretty_breaks(n = 6)
  ) +
  scale_y_continuous(
    expand = expansion(c(0.1, 0.1)),
    breaks = pretty_breaks(n = 6),
    labels = number_format(scale = 0.001, suffix = "k")
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    x = eje_x,
    y = eje_y,
    caption = "Elaborado por México, ¿cómo vamos? con datos simulados\\nTamaño de burbuja = población estatal"
  ) +
  
  # TEMA
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 28, colour = "#777777", margin = margin(0,0,20,0)),
    plot.caption = element_text(size = 15, colour = mcv_blacks[3]),
    plot.margin = margin(0.3, 0.3, 1.4, 0.3, "cm"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = 20)
  ) +
  
  # Guías de leyenda personalizadas
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 6)),
    fill = guide_legend(order = 1, override.aes = list(size = 6)),
    size = guide_legend(order = 2, override.aes = list(color = mcv_blacks[3]))
  )

# ESTADÍSTICAS CORRELACIÓN
correlacion <- cor(datos_burbujas$educacion_promedio, datos_burbujas$pib_per_capita)
cat("\\nCoeficiente de correlación:", round(correlacion, 3))

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/dispersion_burbujas.png", 
       width = 16, height = 12, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)