# ====================================
# TIPO DE GRÁFICA: Gráficos con Facetas Múltiples
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)

# Establecer semilla para reproducibilidad
set.seed(258)

# CONFIGURACIÓN
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# PALETAS DE COLORES MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_blacks <- c("black", "#D2D0CD", "#777777")

# DATOS SIMULADOS - Evolución de indicadores laborales por sexo
fechas <- seq(as.Date("2019-01-01"), as.Date("2024-12-01"), by = "quarter")
n_obs <- length(fechas)

# Simular tres indicadores laborales diferentes
set.seed(258)
datos_facetas <- expand_grid(
  fecha = fechas,
  sexo = c("Hombres", "Mujeres"),
  indicador = c("Tasa de Participación", "Tasa de Ocupación", "Tasa de Desocupación")
) %>%
  mutate(
    # Valores base por indicador y sexo
    valor_base = case_when(
      indicador == "Tasa de Participación" & sexo == "Hombres" ~ 78,
      indicador == "Tasa de Participación" & sexo == "Mujeres" ~ 45,
      indicador == "Tasa de Ocupación" & sexo == "Hombres" ~ 96,
      indicador == "Tasa de Ocupación" & sexo == "Mujeres" ~ 95,
      indicador == "Tasa de Desocupación" & sexo == "Hombres" ~ 4,
      indicador == "Tasa de Desocupación" & sexo == "Mujeres" ~ 5
    ),
    # Añadir tendencia temporal y variabilidad
    tendencia = case_when(
      indicador == "Tasa de Participación" ~ row_number() * 0.05,
      indicador == "Tasa de Ocupación" ~ -row_number() * 0.02,
      indicador == "Tasa de Desocupación" ~ row_number() * 0.01
    ),
    # Efecto estacional
    estacional = 0.8 * sin(2 * pi * (quarter(fecha) - 1) / 4),
    # Ruido aleatorio
    ruido = rnorm(n(), 0, 0.5),
    # Valor final
    valor = valor_base + tendencia + estacional + ruido
  ) %>%
  # Efecto COVID más marcado
  mutate(
    valor = case_when(
      fecha >= as.Date("2020-04-01") & fecha <= as.Date("2020-10-01") & 
        indicador == "Tasa de Desocupación" ~ valor + 2.5,
      fecha >= as.Date("2020-04-01") & fecha <= as.Date("2020-10-01") & 
        indicador == "Tasa de Participación" ~ valor - 2.0,
      fecha >= as.Date("2020-04-01") & fecha <= as.Date("2020-10-01") & 
        indicador == "Tasa de Ocupación" ~ valor - 1.5,
      TRUE ~ valor
    ),
    # Ajustar límites lógicos
    valor = case_when(
      indicador == "Tasa de Desocupación" ~ pmax(valor, 1),
      TRUE ~ pmax(valor, 0)
    ),
    valor = pmin(valor, 100)
  ) %>%
  select(fecha, sexo, indicador, valor)

# Colores por sexo
colores_sexo <- c("Hombres" = mcv_discrete[3], "Mujeres" = mcv_discrete[1])

# Etiquetas y títulos
titulo <- "Evolución de Indicadores del Mercado Laboral"
subtitulo <- "Indicadores trimestrales por sexo, 2019-2024"
eje_y <- "Porcentaje (%)"

# VISUALIZACIÓN
g <- ggplot(
  datos_facetas,
  aes(x = fecha, y = valor, color = sexo, group = sexo)
) +
  # Líneas principales
  geom_line(linewidth = 1.2, alpha = 0.9) +
  
  # Puntos en último valor
  geom_point(
    data = datos_facetas %>% 
      group_by(indicador, sexo) %>% 
      filter(fecha == max(fecha)),
    size = 3, stroke = 1.5, fill = "white", shape = 21
  ) +
  
  # Etiquetas de último valor
  geom_text(
    data = datos_facetas %>% 
      group_by(indicador, sexo) %>% 
      filter(fecha == max(fecha)),
    aes(label = paste0(round(valor, 1), "%")),
    vjust = -1, hjust = 0.5, family = "Ubuntu", 
    fontface = "bold", size = 3.5, show.legend = FALSE
  ) +
  
  # Área sombreada para período COVID
  annotate("rect", 
           xmin = as.Date("2020-04-01"), xmax = as.Date("2020-10-01"),
           ymin = -Inf, ymax = Inf, 
           fill = mcv_semaforo[4], alpha = 0.15) +
  
  # Facetas por indicador
  facet_wrap(~ indicador, scales = "free_y", ncol = 1) +
  
  # ESCALAS
  scale_color_manual(name = "", values = colores_sexo) +
  scale_x_date(
    breaks = seq.Date(min(datos_facetas$fecha), max(datos_facetas$fecha), by = "6 month"),
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    expand = expansion(c(0.05, 0.15)),
    labels = percent_format(scale = 1)
  ) +
  
  # ETIQUETAS
  labs(
    title = titulo,
    subtitle = subtitulo,
    y = eje_y,
    x = NULL,
    caption = "Elaborado por México, ¿cómo vamos? con datos simulados\\nÁrea sombreada: período más intenso de COVID-19"
  ) +
  
  # TEMA
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 30, colour = "#777777", margin = margin(0,0,20,0)),
    plot.caption = element_text(size = 15, colour = mcv_blacks[3]),
    plot.margin = margin(0.3, 0.3, 1.4, 0.3, "cm"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    text = element_text(family = "Ubuntu"),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 18),
    # Configuración de facetas
    strip.text = element_text(size = 20, face = "bold", color = "#6950D8",
                             margin = margin(5, 5, 5, 5)),
    strip.background = element_rect(fill = mcv_blacks[2], color = NA, alpha = 0.3),
    panel.spacing = unit(1, "lines"),
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.margin = margin(t = 15)
  )

# ESTADÍSTICAS RESUMEN
resumen_ultimo <- datos_facetas %>%
  group_by(indicador, sexo) %>%
  filter(fecha == max(fecha)) %>%
  pivot_wider(names_from = sexo, values_from = valor) %>%
  mutate(brecha = Hombres - Mujeres) %>%
  select(indicador, Hombres, Mujeres, brecha)

cat("\\nVALORES MÁS RECIENTES Y BRECHAS DE GÉNERO:\\n")
print(resumen_ultimo)

# EXPORTACIÓN
ggsave("07_ejemplos_tipo/facetas_multiples.png", 
       width = 16, height = 14, dpi = 300, bg = "white")

# Mostrar gráfica
print(g)