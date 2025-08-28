
library(readxl)
library(tidyverse)

# 1. Cargue los datos de pobreza que se encuentran en su carpeta del ejercicio bajo el nombre df_pob_ent2.xlsx.
pobreza <- read_excel("df_pob_ent2.xlsx")

# 2. Utilizando los verbos del tidyverse vistos, responda las
# siguientes preguntas:
#   ¿Cuántas columnas tiene? ¿Cuántas filas?

dim(pobreza)
ncol(pobreza)
nrow(pobreza)
pobreza

# ¿Qué representa cada fila en la tabla?
# R: Un registro de estado por año. 

# ¿Cuál es el estado con mayor porcentaje de población
# de pobreza (pobreza) en 2024?

pobreza %>% 
  filter(anio == 2024) %>% 
  select(entidad, pobreza) %>% 
  arrange(-pobreza)
  
pobreza %>% 
  filter(anio == 2024) %>% 
  select(entidad, pobreza) %>% 
  filter(pobreza == max(pobreza))

names(pobreza)

# ¿Cuál es el estado con menor porcentaje de carencia
# por acceso a la salud?
pobreza %>% 
  filter(anio == 2024) %>% 
  select(anio, entidad, ic_asalud) %>% 
  filter(ic_asalud == min(ic_asalud))

pobreza %>% 
  filter(anio == 2024) %>% 
  select(anio, entidad, ic_asalud) %>% 
  arrange(ic_asalud)

# ¿Cuales son los 10 estados con mayor porcentaje de
# pobreza extrema (pobreza_e) en 2024?

pobreza %>% 
  select(anio, entidad, pobreza_e) %>% 
  filter(anio == 2024) %>% 
  arrange(-pobreza_e) %>% 
  slice(1:10)

# ¿Cuál es el promedio simple del porcentaje de pobreza
# moderada (pobreza_m) de los estados del centro del país
# (CDMX, Puebla, EDOMEX, Morelos e Hidalgo) en 2024?

pobreza %>% 
  filter(entidad %in% c("Ciudad de México", "Puebla", "México", "Morelos", "Hidalgo")) %>% 
  filter(anio == 2024) %>% 
  select(anio, entidad, pobreza_m) %>% 
  summarise(promedio_centro_pais = mean(pobreza_m))
  
# ¿En qué año alcanzó Chiapas su porcentaje más alto de
# población con carencia por acceso a la seguridad social?
pobreza %>%
  filter(entidad == "Chiapas") %>%
  select(entidad, anio, ic_segsoc) %>%
  arrange(-ic_segsoc)

# ¿Cuál es el valor más bajo de pobreza que ha alcanzado
# algún estado (con los datos de la tabla)?

pobreza %>% 
  filter(pobreza == min(pobreza)) %>% 
  select(anio, entidad, pobreza)

# Obtenga el promedio y la desviación estándar del
# porcentaje de pobreza para todos los estados, para cada
# año

pobreza %>% 
  group_by(anio) %>% 
  summarise(promedio = mean(pobreza), 
            desv_est = sd(pobreza))

# Genere una gráfica de barras para visualizar el porcentaje
# de población en situación de pobreza para cada entidad
# para el año 2024. Que las barras de los 10 estados con
# mayor porcentaje aparezcan en un tono de rojo, los 10
# estados con menor porcentaje aparezcan en un tono de
# verde, y el resto que aparezcan en alguna variante de
# amarillo.

bd_grafica <- pobreza %>% 
  filter(anio == 2024)

bd_grafica %>% 
  ggplot(aes(x = reorder(entidad, pobreza), y = pobreza)) + 
  geom_col() + coord_flip()

# LO QUE FALTA LO HACEMOS EN CLASE LA SEMANA QUE VIENE

