# setwd("~/Documents/Tec de Monterrey/5 semestre/R Ciencia de Datos/Tareas/Tarea 2")

# LIBRERIAS ---------------------------------------------------------------

if (require(rvest) == FALSE) {
  install.packages("rvest")
  library(rvest)
} else {
  library(rvest)
}

if (require(tidyverse) == FALSE) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

# DATA --------------------------------------------------------------------

# GENERAMOS UNA TABLA VACÍA 
tabla <- tibble()

#Elegí La Jornada, y el estado de Jalisco, pero según yo, no tiene más de una página. 
url <- str_c("https://www.jornada.com.mx/tag/inseguridad%20cdmx")

inseguridad_cdmx <- read_html(url) 
# EXTRAEMOS EL CONTENIDO DE LA PÁGINA PRINCIPAL --------------------------------------------------------------

# EXTRAEMOS LA FUENTE
Fuente <- inseguridad_cdmx %>% 
  html_node(".logo img") %>% 
  html_attr("alt")

# EXTRAEMOS LAS RAMAS DE LOS TÍTULOS
Titulos <- inseguridad_cdmx %>% 
  html_nodes(".nota-titulo") %>% 
  html_text()

#EXTRAEMOS LAS FECHAS DE REDACCIÓN
Fechas <- inseguridad_cdmx %>% 
  html_nodes(".nota-fecha") %>% 
  html_text()

# EXTRAEMOS LOS LINKS
URL <- inseguridad_cdmx %>% 
  html_nodes(".nota-titulo") %>%   # Selecciona el contenedor principal
  html_nodes("a") %>%              # Busca el enlace dentro del contenedor
  html_attr("href")                # Extrae el atributo href (el link)

# ARMAMOS LA TABLA
# Se define la estructura manualmente porque al no usar un bucle for, rbind no genera las columnas automáticamente en la primera iteración.
tabla <- tibble(Titulos = character(), 
                URL = character(), 
                Fechas = character(),
                Fuente = character()
)

#tabla <- rbind(tabla, tibble(titulos, fecha, links))
tabla <- rbind(tabla, tibble(Fuente, Titulos, URL, Fechas))

# EXTRAEMOS EL CONTENIDO DE LOS ARTÍCULOS DE MANERA INDIVIDUAL ----------------------------------------------------------


# GENERAMOS LAS TABLAS VACÍAS PARA PONER LOS RESULTADOS 
autor <- tibble(Autores = character())
texto <- tibble(Textos = character())
articulo_n <- 1


for(articulo_n in 1:nrow(tabla)){
  liga_articulo <- tabla$URL[articulo_n]
  
  articulo <- read_html(liga_articulo)
  
  # EXTRAEMOS LOS PARRAFOS
  Textos <- articulo %>% 
    html_nodes("#content_nitf p, #content_nitf div, .article-text p, .body-text p") %>% 
    html_text()
  
  todos_Textos <- str_c(Textos, collapse = "\n\n") 
  
  # EXTRAEMOS LOS AUTORES
  Autores <- articulo %>% 
    html_nodes(".nota-autor a") %>%
    html_text()
  
  todos_Autores <- str_c(Autores, collapse = "\n\n")
  
  texto <- rbind(texto, tibble(Textos = todos_Textos))
  autor <- rbind(autor, tibble(Autores = todos_Autores))
  
  print(str_c("Listo artículo ", articulo_n))
}


# JUNTAMOS TODO -----------------------------------------------------------

cdmx_inseguridad <- cbind(tabla,
                autor = autor[,1],
                textos = texto[,1]
                ) %>% 
  as_tibble()



View(cdmx_inseguridad)

openxlsx::write.xlsx(cdmx_inseguridad, "tabla_Raspado_web.xlsx")
