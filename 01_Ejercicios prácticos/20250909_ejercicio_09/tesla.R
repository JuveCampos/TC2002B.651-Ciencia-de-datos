

library(rvest)
library(tidyverse)

numeros <- seq(1,48*4, 48)

# Para fines del ejemplo voy a matar el registro del carro 1. 
# Si lo dejo, me da error, porque me manda a la página 1 que tiene publicidad, y se trae también información extra. 
# Por eso inicio la secuencia en el 2. 
numeros[1] <- 2 # Cambiamos el primer numero de la secuencia al 2

# Generamos una tabla vacía 
tabla <- tibble()

# Hacemos el bucle de extracción 
for(no in numeros){
  
  url <- str_c("https://autos.mercadolibre.com.mx/tesla/tesla_Desde_", no, "_NoIndex_True_VEHICLE*TYPE_398351?sb=all_mercadolibre")
  
  html <- rvest::read_html(url)
  
  precio <- html %>% 
    html_nodes(".poly-price__current") %>% 
    html_text()
  
  descripcion <- html %>% 
    html_nodes(".poly-component__title-wrapper") %>% 
    html_text()
  
  datos_anio_km <- html %>% 
    html_nodes(".poly-component__attributes-list") %>% 
    html_nodes("ul") %>% 
    html_text()
  
  anio <- datos_anio_km %>% 
    str_extract(pattern = "^\\d{4}")
  
  kilometraje <- datos_anio_km %>% 
    str_remove(pattern = "^\\d{4}")
  
  lugar <- html %>% 
    html_nodes(".poly-component__location") %>% 
    html_text()
  
  resultados <- tibble(descripcion, anio, kilometraje, lugar)
  
  tabla <- rbind(tabla, resultados)
  print(str_c("Listo registros iniciando del ", no, " al ", no + 47))
  
}

# Modificamos la tabla ----
tabla <- tabla %>% 
  mutate(kilometraje_numerico = str_remove_all(kilometraje, pattern = ",|\\sKm") %>% 
           as.numeric(), 
         anio = as.numeric(anio))

# Explore la tabla ----
tabla


