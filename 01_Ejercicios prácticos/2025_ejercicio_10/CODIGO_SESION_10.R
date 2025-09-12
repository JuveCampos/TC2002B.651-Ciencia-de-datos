
# Acá viene todo el código de la clase de Web-scraping y de APIs
# Por favor, verificar que funcione bien. 

# PARTE 1. EXTRACCIÓN DE ARTÍCULOS DE JUVENAL ----

# CARGAMOS LAS LIBRERÍAS PARA WEB SCRAPING
library(rvest)
library(tidyverse)
library(openxlsx)

# GENERAMOS UNA TABLA VACÍA 
tabla <- tibble()

# HACEMOS EL BUCLE DE EXTRACCIÓN DE LAS 10 PAGINAS QUE CONTIENEN LOS ARTÍCULOS 
for(n in 1:10){
  
  # GENERAMOS LA URL DE LA PAGINA QUE VAMOS A EXTRAER
  url <- str_c("https://atiempo.tv/author/juvenal-campos/page/", n, "/")
  
  # LEEMOS EL CÓDIGO DE LA PÁGINA
  pagina <- read_html(url)
  
  # EXTRAEMOS LAS RAMAS DE LOS TÍTULOS
  titulos <- pagina %>% 
    html_nodes(".entry-title") %>% 
    html_text()
  
  # EXTRAEMOS LOS LINKS
  links <- pagina %>% 
    html_nodes(".entry-title") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # EXTRAEMOS LAS FECHAS
  fecha <- pagina %>% 
    html_nodes(".entry-date") %>% 
    html_text() %>% 
    unique()
  
  # ARMAMOS LA TABLA
  tabla <- rbind(tabla, tibble(titulos, fecha, links))
  
  # MENSAJE PARA SABER COMO VA AVANZANDO EL CÓDIGO
  print(str_c("Ya está listo el contenido de la página ", n))
}

# AHORA; EXTRAEMOS EL CONTENIDO DE LOS ARTÍCULOS DE MANERA INDIVIDUAL
# GENERAMOS LAS TABLAS VACÍAS PARA PONER LOS RESULTADOS 
textos <- tibble()
imagenes2 <- tibble()

# i = 2 # PARÁMETRO DE PRUEBA. ES UN NUMERITO QUE DECLARAMOS PARA PROBAR QUE EL BUCLE FUNCIONE BIEN. LO DEJO SILENCIADO PORQUE DESPUÉS DE LAS PRUEBAS YA NO SIRVE PARA NADA

# BUCLE DE EXTRACCIÓN DE CONTENIDO DE ARTÍCULOS
for(i in 1:nrow(tabla)){
  
  liga_articulo <- tabla$links[i]
  
  articulo <- read_html(liga_articulo)
  
  parrafos <- articulo %>% 
    html_nodes(".entry-content") %>% 
    html_nodes("p") %>% 
    html_text()
  
  todos_parrafos <- str_c(parrafos, collapse = "\n\n") 
  
  imagenes <- articulo %>% 
    html_nodes(".entry-content") %>% 
    html_nodes("img") %>% 
    html_attr("src") %>% 
    str_c(collapse = "\n")
  
  textos <- rbind(textos,todos_parrafos )
  imagenes2 <- rbind(imagenes2, imagenes)
  
  print(str_c("Listo articulo ", i))
}

# JUNTAMOS LOS RESULTADOS EN UNA SOLA TABLA 
tabla2 <- cbind(tabla, 
                textos = textos[,1], 
                imagenes2 = imagenes2[,1])

# GUARDAMOS LOS RESULTADOS EN UN EXCEL
openxlsx::write.xlsx(tabla2, "tabla_archivos_juve.xlsx")


# PARTE 2. EXTRAER DATOS DE TESLAS DE MERCADO LIBRE ----
# PARECE QUE ESTE CÓDIGO NO FUNCIONÓ EN EL TEC, PERO AHORA SI ME FUNCIONÓ EN MI CASA. 
# REVISEN A VER SI LES PASA LO MISMO. 
# PARA ESTE EJERCICIO, DONDE SOLO TIENE EL CÓDIGO DE EXTRACCIÓN DE DATOS, LE SUGIERO QUE VAYA A LA PÁGINA E IDENTIFIQUE LAS RAMAS SELECCIONADAS PARA CADA DATO

library(rvest)
library(tidyverse)

# SECUENCIA DE NUMEROS PARA GENERAR LA CONSULTA 
numeros <- seq(from = 1, to = 48*4,by =  48)

# Para fines del ejemplo voy a matar el registro del carro 1. 
# Si lo dejo, me da error, porque me manda a la página 1 que tiene publicidad, y se trae también información extra. 
# Por eso inicio la secuencia en el 2. 
numeros[1] <- 2 # Cambiamos el primer numero de la secuencia al 2

# GENERAMOS UNA TABLA VACÍA PARA IRLE VERTIENDO LA INFORMACIÓN 
tabla <- tibble()

# Hacemos el bucle de extracción 
for(no in numeros){
  
  # GENERAMOS LA URL PARA CONSULTAR 
  url <- str_c("https://autos.mercadolibre.com.mx/tesla/tesla_Desde_", no, "_NoIndex_True_VEHICLE*TYPE_398351?sb=all_mercadolibre")
  
  # LEEMOS EL CONTENIDO DE LA PÁGINA WEB
  html <- rvest::read_html(url)
  
  # EXTRAEMOS EL PRECIO DEL NODO MARCADO
  precio <- html %>% 
    html_nodes(".poly-price__current") %>% 
    html_text()
  
  # EXTRAEMOS LA DESCRIPCIÓN DEL NODO MARCADO
  descripcion <- html %>% 
    html_nodes(".poly-component__title-wrapper") %>% 
    html_text()
  
  # EXTRAEMOS LOS DATOS DE AÑO Y KILOMETRAJE DEL NODO MARCADO 
  # VIENEN PEGADOS; LOS SEPARAREMOS MÁS ADELANTE CON EXPRESIONES REGULARES
  datos_anio_km <- html %>% 
    html_nodes(".poly-component__attributes-list") %>% 
    html_nodes("ul") %>% 
    html_text()
  
  # EXTRAEMOS EL DATO DEL AÑO (DEL TEXTO ANTERIOR, CON EXPRESION REGULAR)
  anio <- datos_anio_km %>% 
    str_extract(pattern = "^\\d{4}") # "LOS PRIMEROS CUATRO DÍGITOS DEL TEXTO"
  
  # EXTRAEMOS EL DATO DEL KILOMETRAJE (DEL TEXTO ANTERIOR, CON EXPRESION REGULAR)
  kilometraje <- datos_anio_km %>% 
    str_remove(pattern = "^\\d{4}") # "LOS PRIMEROS CUATRO DÍGITOS DEL TEXTO"
  
  # EXTRAEMOS LA LOCALIZACIÓN DEL NODO SELECCIONADO
  lugar <- html %>% 
    html_nodes(".poly-component__location") %>% 
    html_text()
  
  # JUNTAMOS TODO EN UNA TABLA
  resultados <- tibble(descripcion,precio, anio, kilometraje, lugar)
  
  # LE PEGAMOS LOS DATOS A LA TABLA VACÍA
  tabla <- rbind(tabla, resultados)
  
  # MANDAMOS EL MN
  print(str_c("Listo registros iniciando del ", no, " al ", no + 47))
  
}

# Explore la tabla
tabla

# [EXTRA] MODIFICAMOS LA TABLA CON EXPRESIONES REGULARES
tabla2 <- tabla %>% 
  mutate(precio_numerico = str_remove_all(precio, pattern = "\\$|\\,") %>% 
           as.numeric()) %>% 
  mutate(kilometraje_numerico = 
           str_remove_all(kilometraje, pattern = ",|\\sKm") %>% # UBICA O LAS COMAS O EL PATRON "ESPACIO" (\\s) MÁS LA PALABRA "Km" (Y BORRALOS TODOS)
           as.numeric(), 
         anio = as.numeric(anio)) %>% # Año numérico
  mutate(antiguedad = 2025 - anio) %>% 
  mutate(modelo = str_extract(descripcion, pattern = "Cybertruck|Model?(\\w) \\w")) %>% 
  mutate(modelo = ifelse(is.na(modelo),
                         yes = str_c("Model ", str_extract(descripcion, pattern = "X|S|3")),
                         no = modelo)) %>% 
  mutate(modelo = str_replace(modelo, pattern = "Modelo", replacement = "Model")) %>% 
  filter(!is.na(modelo))


tabla2 %>% 
  ggplot(aes(x = antiguedad, 
             y = precio_numerico,
             color = modelo)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Relación entre antiguedad y precio de los modelos de Tesla vendidos en México")

tabla2 %>% 
  filter(kilometraje_numerico < 250000) %>% # Eliminamos el outlier
  ggplot(aes(x = kilometraje_numerico, 
             y = precio_numerico)) + 
  geom_point(aes(color = modelo)) + 
  geom_smooth(method = "loess") + 
  theme_minimal() + 
  labs(title = "Relación entre kilometraje y precio de los modelos de Tesla vendidos en México") 


# PARTE 3, APIS ----

# Librerias 
library(tidyverse)
library(httr)
library(jsonlite)
library(ggimage)

# ARMAMOS LA PETICIÓN COMO SE ESPECIFICÓ EN LA PÁGINA DE LA POKEAPI
peticion <- "https://pokeapi.co/api/v2/pokemon/1"

# HACEMOS LA SOLICITUD GET, COMO DECÍA EN LA DOCUMENTACIÓN 
solicitud <- GET(peticion)

# EXTRAEMOS EL CONTENIDO DE LA SOLICITUD (NOS SALE UN JSON)
contenido <- content(solicitud, "text")

# CONVERTIMOS ESE JSON A LISTA
json <- fromJSON(contenido)

# CON EL SÍMBOLO "$", VAMOS EXPLORANDO LA INFORMACIÓN SOLICITADA
# EN ESTE CASO, NOS QUEDAMOS CON LOS DATOS DE PESO, ALTURA Y LA IMAGEN DE FRENTE
peso <- json$weight
altura <- json$height
imagen <- json$sprites$front_default


# Hacemos la tabla con el resto de los pokemon 
datos_pkmn <- lapply(1:151, function(i){
  
  peticion <- str_glue("https://pokeapi.co/api/v2/pokemon/{i}")
  solicitud <- GET(peticion)
  contenido <- content(solicitud, "text")
  json <- fromJSON(contenido)
  
  nombre <- json$name %>% str_to_sentence()
  peso <- json$weight
  altura <- json$height
  imagen <- json$sprites$front_default
  
  tabla_datos <- tibble(numero = i,nombre, peso, altura, imagen)
  return(tabla_datos) # Forzamos a que el bucle nos regrese justo la tabla como resultado del lapply
  print(str_c("Lista descarga para ", nombre))
  
})

# Ver que el resultado del lapply es una lista
datos_pkmn

# Convertimos la lista a una tibble 
datos_pokemon <- datos_pkmn %>% 
  do.call(rbind, .) # do.call aplica la función rbind a toda (.) la lista previa. El (.) es un placeholder que significa "toda" la tabla. 
# Si recuerdan, la pipa pasa como primer argumento el objeto previo a la siguiente función. 
# El punto le dice a la pipa que no mande el objeto previo como primer argumento, sino como SEGUNDO argumento. 

# Gráfica 
datos_pokemon %>% 
  ggplot(aes(x = peso, y = altura)) + 
  geom_point() + 
  geom_image(aes(image = imagen), 
             size = 0.1) + 
  labs(title = "Distribución peso/altura de los primeros 151 pokémon")
  

# PARTE 4. API DE OPEN METEO API ----

# 1. HACEMOS LA SOLICITUD (IMITANDO LA SOLICITUD DE PYTHON)
solicitud_clima <- GET(url = "https://api.open-meteo.com/v1/forecast", 
                       query = list(
                         "latitude"= 19.28,
                         "longitude"= -99.13,
                         "hourly"= "temperature_2m"
                       ))

# EXTRAEMOS DATOS DE CLIMA 
clima <- content(solicitud_clima, "text")

# EXTRAEMOS DATOS DEL JSON DEL PASO ANTERIOR
json_clima <- fromJSON(clima)

# EXTRAEMOS LA TABLA CON DATOS DE TIEMPO Y CLIMA
tiempo <- json_clima$hourly$time
temperatura <- json_clima$hourly$temperature_2m

# ARMAMOS LA TABLA 
CLIMA_TEC <- tibble(tiempo, temperatura)

# EXTRA: GRÁFICA DEL CLIMA. 
CLIMA_TEC %>% 
  mutate(tiempo = as_datetime(tiempo, format = "%Y-%m-%dT%H:%M")) %>% 
  ggplot(aes(x = tiempo, y = temperatura)) + 
  geom_line() + 
  scale_x_datetime(breaks = "12 hours", date_labels = "%d/%m\n%H:%M") +
  labs(title = "Temperatura esperada para el Tec de Monterrey, Campus Ciudad de México", 
       subtitle = str_c("Para los próxima semana"))

# NOTA: Dado que el API se actualiza conforme se actualiza la base de datos, esta gráfica no va a ser igual si se corre hoy o en un año, ya que trabajará con los datos de la semana en que se ejecute el código.

# TAREA (no se revisa pero se sugiere)
# Obtenga la llave de las APIs para inegi, spotify, youtube y gemini

library(tidyverse)
library(inegiR)
library(spotifyr)
library(tuber)
library(ellmer)

# 1. INEGI 
mi_token_inegi <- "682ad7f9-19ge-47f2-abcd-e4c4ab2g2938" 
# Esta clave INEGI es falsa, la dejo para que vean como se debe de ver cuando consigan la suya

pib_aguascalientes <- inegiR::inegi_series(series_id = 746098,
                                           token = mi_token_inegi,
                                           database = "BIE")

# 2. Spotify
client_id <- "a26c2ce0d3cf4e13a39a81db63f5fba1"
client_secret <- "2cd654b1a69454d18a23h88997c81d7a"
# Estas claves son falsas, las dejo para que vean como se deben de ver cuando ustedes consigan las suyas

access_token <- get_spotify_access_token(client_id = client_id, client_secret = client_secret)
datos_shakira <- search_spotify("Shakira", type = "track", limit = 50) %>% arrange(-popularity)

# 3. Youtube 
# https://www.youtube.com/watch?v=9ZyFkQv1bKo # Video sobre el impuesto a videojuegos

yt_oauth(app_id     = "845276260238-td7nñqptbrndggsg07tm7r770hj05rat.apps.googleusercontent.com",
         app_secret = "JOKSPJ-i2sCqtpTRhTito7x7pJ5UXuo16uq")
# Estas claves son falsas, las dejo para que vean como se deben de ver cuando ustedes consigan las suyas

comentarios_video <- tuber::get_all_comments(video_id = "9ZyFkQv1bKo")
openxlsx::write.xlsx(comentarios_video, "comentarios_video")

# Sugerencia: vean este video: https://www.youtube.com/watch?v=EPeDTRNKAVo
# O este video: https://www.youtube.com/watch?v=79stB0NZkaA 
# O los dos. 

# 4. Gemini
chat <- chat_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = "AIzuSyA559K-XkHuf615M15XaI2Ajfk12hhzD5u"
)
# Estas claves son falsas, las dejo para que vean como se deben de ver cuando ustedes consigan las suyas

chat$chat("Hola Gemini! ¿Cómo estás?")

