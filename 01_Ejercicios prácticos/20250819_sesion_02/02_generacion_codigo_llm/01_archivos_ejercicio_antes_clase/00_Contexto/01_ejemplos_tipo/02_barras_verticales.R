# ====================================
# TIPO DE GRÁFICA: Gráfico de Barras Verticales
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)

# Establecer semilla para reproducibilidad
set.seed(456)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Comparación por estados (top 10)
estados <- c("Ciudad de México", "Estado de México", "Nuevo León", 
             "Jalisco", "Veracruz", "Puebla", "Guanajuato", 
             "Chihuahua", "Michoacán", "Oaxaca")

datos_barras <- tibble(
  estado = factor(estados, levels = estados),
  valor = c(15.2, 12.8, 11.3, 9.7, 8.4, 7.9, 7.2, 6.8, 6.1, 5.4),
  color_grupo = c("alto", "alto", "alto", "medio", "medio", 
                  "medio", "bajo", "bajo", "bajo", "bajo")
) %>%
  # Destacar valor nacional
  mutate(
    destacar = ifelse(estado == "Nuevo León", "sí", "no"),
    etiqueta = paste0(valor, "%")
  )

# Colores para grupos
colores_grupos <- c("alto" = mcv_semaforo[4], "medio" = mcv_semaforo[3], "bajo" = mcv_semaforo[1])

# Etiquetas y títulos
titulo <- "Indicador Socioeconómico por Entidad Federativa"
subtitulo <- "Estados con mayor participación, 2024"
eje_y <- "Porcentaje (%)"

# VISUALIZACIÓN
g <- ggplot(
  datos_barras,
  aes(x = estado, y = valor, fill = color_grupo)
) +
  # Barras principales
  geom_col(width = 0.7, alpha = 0.8) +
  
  # Destacar una barra específica
  geom_col(
    data = datos_barras %>% filter(destacar == "sí"),
    aes(x = estado, y = valor),
    fill = mcv_discrete[1], width = 0.7
  ) +
  
  # Etiquetas de valores
  geom_text(
    aes(label = etiqueta),
    vjust = -0.5, family = "Ubuntu", fontface = "bold",
    size = 5, color = mcv_blacks[1]
  ) +
  
  # Línea de referencia
  geom_hline(yintercept = mean(datos_barras$valor), 
             linetype = "dashed", color = mcv_blacks[3], linewidth = 1) +
  
  # Anotación de promedio
  annotate("text", x = 8, y = mean(datos_barras$valor) + 0.5,
           label = paste0("Promedio: ", round(mean(datos_barras$valor), 1), "%"),
           family = "Ubuntu", color = mcv_blacks[3], size = 4) +
  
  # ESCALAS
  scale_fill_manual(values = colores_grupos, guide = "none") +
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
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 20),
    axis.line.x = element_line(color = mcv_blacks[2])
  )

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/barras_verticales.png", 
       width = 16, height = 9, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)