
# Repaso de APIs

# Librerias ----
library(tidyverse) # Manejo de datos
library(inegiR)    # Acceso al API de INEGI (cliente)
library(spotifyr)  # Acceso al API de spotify (cliente)
library(tuber)     # Acceso al API de Youtube (cliente)
library(ellmer)    # Acceso a chatbots de IA
library(usethis)   # Acceso a variables de sistema

# Acceso a variables de ambiente: 
usethis::edit_r_environ()

# 1. Ejemplo: actividad de inegiR. ----
mi_token_inegi <- "682ad7f9-19ge-47f2-abcd-e4c4ab2g2938"
  # Sys.getenv("INEGI_TOKEN_2") # Acceso a mi token de INEGI que tengo guardado en mi sistema. 
# Liga al API de INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html
# Los datos del PIBE (Pib estatal) estan en Constructor de consultas > BIE > Cuentas nacionales > Producto interno bruto por entidad federativa, base 2018 > Por actividad económica y entidad federativa > Valores a precios constantes de 2018 > Producto interno bruto, a precios de mercado


# Actividad 1. #
# Obtenga el PIB de cada estado. 
# Obtenga la proporción del PIB nacional que representa el estado de Morelos. 

# Este código solo se trae el dato de Aguascalientes. 

pib_estatal <- inegiR::inegi_series(series_id = 746098,
                                           token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948",
                                           database = "BIE") 

pib_morelos <- inegiR::inegi_series(series_id = 746114,
                                    token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948",
                                    database = "BIE") %>% 
  rename(values_morelos = values)

pib_nacional <- inegiR::inegi_series(series_id = 746097,
                                    token = "682ad7f9-19fe-47f0-abec-e4c2ab2f2948",
                                    database = "BIE") %>% 
  rename(values_nacional = values)

tabla_unida <- left_join(pib_morelos,pib_nacional) %>% 
  mutate(pp = 100*(values_morelos/values_nacional))

# 746097 Nacional
# 746098 Aguascalientes
# 746099 Baja California
# 746129 Zacatecas
length(746098:746129)

# 2. spotifyr ----
# Obtenga las 50 canciones más populares de Shakira: 

SPOTIFY_CLIENT_ID = "a25b2ce0d3ce4e13a39a70db83f5fba5"
SPOTIFY_CLIENT_SECRET = "1cd654b8a78544d18a23f88987c86c7a"

access_token <- get_spotify_access_token(client_id = SPOTIFY_CLIENT_ID, client_secret = SPOTIFY_CLIENT_SECRET)
datos_shakira <- search_spotify("Shakira", type = "track", limit = 50) %>% arrange(-popularity)
datos_shakira$name
?get_spotify_access_token

# 3. tuber ----

# Autenticacion 
# Limpia tokens viejos si es necesario
if (file.exists(".httr-oauth")) unlink(".httr-oauth")
yt_oauth(app_id     = Sys.getenv("YOUTUBE_APP_ID"),
         app_secret = Sys.getenv("YOUTUBE_APP_SECRET"))

# Visite este video en YT: https://www.youtube.com/watch?v=5zDY5DIiMoM ¿De que trata?
comentarios_video <- tuber::get_all_comments(video_id = "5zDY5DIiMoM") # Obtiene los comentarios
openxlsx::write.xlsx(comentarios_video, "comentarios_video_mañanera_22.xlsx") # Guardamos los comentarios

# Otras cosas prácticas para hacer con tuber: 
busqueda_huracan <- yt_search("huracán narda") # Obtenemos busquedas relacionadas

id_grupo_formula <- "UCzmd9Aj2wmPggWZJpLdBB8Q"   # Obtenemos informacion de canales
get_channel_stats(channel_id = id_grupo_formula) 

# Para obtener duracion de video
parse_iso8601_duration <- function(x) {
  m <- str_match(x, "PT(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?")
  h <- ifelse(is.na(m[,2]), 0, as.integer(m[,2]))
  mi <- ifelse(is.na(m[,3]), 0, as.integer(m[,3]))
  s <- ifelse(is.na(m[,4]), 0, as.integer(m[,4]))
  h*3600 + mi*60 + s
}

get_video_duration_seconds <- function(video_id) {
  det <- get_video_details(video_id, part = "contentDetails")
  iso <- det$items[[1]]$contentDetails$duration
  parse_iso8601_duration(iso)
}

# Ejemplo:
secs <- get_video_duration_seconds("dQw4w9WgXcQ")

# 4. ellmer ----
chat <- chat_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv("GEMINI_KEY")
)

chat$chat("Hola Gemini! ¿Cómo estás?")

chatgpt <- chat_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv("GEMINI_KEY")
)

# Estas solo las corre Juve: 
chat_gpt <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"))
chat_gpt$chat("Hola, como estás?")

chat_claudio <- chat_anthropic(api_key = Sys.getenv("ANTHROPIC_API_KEY"))
chat_claudio$chat("Hola, como estás")

# Actividad: Tome los primeros 20 comentarios del video anterior y analice si son discurso de odio o no. 
