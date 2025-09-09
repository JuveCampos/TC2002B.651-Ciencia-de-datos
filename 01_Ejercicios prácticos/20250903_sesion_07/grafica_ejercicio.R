
# Librerias ----
library(tidyverse)
library(caret)
library(class)
library(readxl)
library(GGally)

# Llamamos a los datos de vino ----
datos_vino <- read_excel("datos_vino.xlsx") %>% 
  mutate(id = 1:300)

# Explorar los datos ----
summary(datos_vino)

# Visualizar la distribución de las variables en X ---- 
datos_vino %>% 
  mutate(id = 1:300) %>% 
  select(-calidad) %>% 
  pivot_longer(cols = acidez:ph) %>% 
  ggplot(aes(x = name, y = value)) + 
  geom_boxplot()

datos_vino %>% 
  GGally::ggpairs()

# Calculo de correlaciones ----
matriz_corr <- cor(datos_vino %>% select(-c(id, calidad)))
nombres <- rownames(matriz_corr)
matriz_corr <- matriz_corr %>% 
  as_tibble() %>% 
  mutate(nombre_variables = nombres) %>% 
  pivot_longer(cols = acidez:ph)

matriz_corr %>% 
  ggplot(aes(x = nombre_variables, y = name, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 2)), 
            color = "white", 
            family = "Arial") + 
  scale_fill_gradient2(low  = "red",
                       high = "blue",
                       mid = "white", 
                      midpoint = 0, 
                      limit = c(-1,1)) + 
  labs(title = "Matríz de correlación de variables de vinos")
