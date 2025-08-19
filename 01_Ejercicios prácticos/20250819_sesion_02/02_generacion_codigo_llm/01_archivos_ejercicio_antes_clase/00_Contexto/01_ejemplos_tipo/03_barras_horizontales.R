# ====================================
# TIPO DE GRÁFICA: Gráfico de Barras Horizontales
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(scales)
library(extrafont)

# Establecer semilla para reproducibilidad
set.seed(789)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Ranking de sectores económicos
sectores <- c("Industria manufacturera", "Comercio", "Servicios inmobiliarios", 
              "Construcción", "Transporte", "Servicios financieros",
              "Agricultura", "Minería", "Servicios públicos", "Otros servicios")

datos_ranking <- tibble(
  sector = sectores,
  valor = c(23.4, 18.7, 12.3, 9.8, 8.2, 7.1, 5.9, 4.8, 4.2, 5.6),
  categoria = c("primario", "secundario", "terciario", "secundario", "terciario",
                "terciario", "primario", "primario", "terciario", "terciario")
) %>%
  # Ordenar por valor descendente
  arrange(desc(valor)) %>%
  mutate(
    sector = factor(sector, levels = sector),
    destacar = ifelse(sector == "Industria manufacturera", "sí", "no"),
    etiqueta_pos = ifelse(valor > 15, "interna", "externa"),
    hjust_val = ifelse(etiqueta_pos == "interna", 1.1, -0.1),
    color_etiqueta = ifelse(etiqueta_pos == "interna", "white", mcv_blacks[1])
  )

# Colores por categoría
colores_categoria <- c("primario" = mcv_discrete[3], 
                      "secundario" = mcv_discrete[4], 
                      "terciario" = mcv_discrete[2])

# Etiquetas y títulos
titulo <- "Contribución al PIB por Sector Económico"
subtitulo <- "Participación porcentual en el PIB nacional, 2024"
eje_x <- "Porcentaje del PIB (%)"

# VISUALIZACIÓN
g <- ggplot(
  datos_ranking,
  aes(x = valor, y = reorder(sector, valor), fill = categoria)
) +
  # Barras principales
  geom_col(width = 0.7, alpha = 0.8) +
  
  # Destacar la barra principal
  geom_col(
    data = datos_ranking %>% filter(destacar == "sí"),
    aes(x = valor, y = sector),
    fill = mcv_discrete[1], width = 0.7
  ) +
  
  # Etiquetas de valores - externas
  geom_text(
    data = datos_ranking %>% filter(etiqueta_pos == "externa"),
    aes(label = paste0(valor, "%"), hjust = hjust_val),
    family = "Ubuntu", fontface = "bold", size = 5,
    color = mcv_blacks[1]
  ) +
  
  # Etiquetas de valores - internas
  geom_text(
    data = datos_ranking %>% filter(etiqueta_pos == "interna"),
    aes(label = paste0(valor, "%"), hjust = hjust_val),
    family = "Ubuntu", fontface = "bold", size = 5,
    color = "white"
  ) +
  
  # Línea vertical de referencia
  geom_vline(xintercept = mean(datos_ranking$valor), 
             linetype = "dashed", color = mcv_blacks[3], linewidth = 1) +
  
  # ESCALAS
  scale_fill_manual(
    name = "Sector:",
    values = colores_categoria,
    labels = c("Primario", "Secundario", "Terciario")
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.15)),
    breaks = pretty_breaks(n = 6),
    labels = percent_format(scale = 1)
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    x = eje_x,
    y = NULL,
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
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    axis.line.y = element_line(color = mcv_blacks[2])
  )

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/barras_horizontales.png", 
       width = 16, height = 9, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)