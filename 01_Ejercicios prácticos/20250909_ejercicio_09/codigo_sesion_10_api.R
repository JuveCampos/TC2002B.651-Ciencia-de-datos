
# Librerias ----
library(tidyverse)
library(httr)
library(jsonlite)

peticion <- "https://pokeapi.co/api/v2/pokemon/1"
solicitud <- GET(peticion)
contenido <- content(solicitud, "text")
json <- fromJSON(contenido)
peso <- json$weight
altura <- json$height
imagen <- json$sprites$front_default

# url = "https://api.open-meteo.com/v1/forecast"
# params = {
#   "latitude": 52.52,
#   "longitude": 13.41,
#   "hourly": "temperature_2m",
# }


solicitud_clima <- GET(url = "https://api.open-meteo.com/v1/forecast", 
    query = list(
      "latitude"= 19.28,
      "longitude"= -99.13,
      "hourly"= "temperature_2m"
    ))

clima <- content(solicitud_clima, "text")
json_clima <- fromJSON(clima)

tiempo <- json_clima$hourly$time
temperatura <- json_clima$hourly$temperature_2m

tibble(tiempo, temperatura)
