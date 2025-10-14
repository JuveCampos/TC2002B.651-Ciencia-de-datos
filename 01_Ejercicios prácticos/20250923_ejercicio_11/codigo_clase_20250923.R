
# ---- Paquetes ----
library(tidyverse)
library(tuber)        # wrapper de YouTube Data API v3
library(jsonlite)
library(purrr)
library(lubridate)
library(janitor)
library(tidytext)
library(cld3)         # detección de idioma
library(stopwords)
library(glue)
library(ellmer)

# ---- 1) Autenticación (OAuth) ----
# Crea credenciales OAuth 2.0 en Google Cloud (Desktop app)
# Credenciales cambiadas para que pongan las suyas
if (file.exists(".httr-oauth")) unlink(".httr-oauth")
yt_oauth(app_id     = "736475268268-tp7nmqptblncggsf07pm8r880np01rat.apps.googleusercontent.com",
         app_secret = "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")

comentarios <- get_all_comments(video_id = "7iJJYG7MrpQ")
saveRDS(comentarios, "comentarios_video_sh.rds")

# hcatn-d-TSo
comentarios <- get_all_comments(video_id = "7iJJYG7MrpQ")

# 4. ellmer ----

# Con este código llamamos a Gemini a la sesión de R
# Ponga sus propias credenciales. Estas están modificadas, ya que no les puedo compartir las mias.
# (por lo que les comenté en clase)
chat <- chat_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = "AIzaSyA339Z-XkHuf415M15YaI3Alek12wksD6O"
)

# Probamos la conexión 
chat$chat("Hola Gemini! ¿Cómo estás?")
chat$chat(str_c("Podrías decir si este comentario es ofensivo o no? Solo dime si SI o si NO",
                comentarios$textDisplay[30]))

respuestas <- lapply(comentarios$textDisplay, function(c){
  chat$chat(str_c("Podrías decir si este comentario es ofensivo o no? Solo dime si SI o si NO",
                  c))
})
# El codigo previo va a tronar porque le estamos diciendo que clasifique casi 4000 comentarios, pero solo tenemos derecho a 10 consultas en la versión gratuita

# Acá lo hacemos con la de ChatGPT. 
# Recuerda que esto es de paga. 
# Si quieres ver el resultado, saltate a la parte donde guardamos los resultados en el excel. 

# Estas solo las corre Juve: 
# Estos son mis tokens (modificadas)
# Si decides contratar la API de paga, tendrás que poner acá tus contraseñas propias. 
chat_gpt <- chat_openai(api_key = "sk-proj-jazZdRCfeS_jeEqrAdnTmTngwECRACylQ3GdYkqPPWtk6a4C7kxjzN6Sk3VFQNt91GMWCT3BlbkFwXuraJxSWuZACFualb6fGbOBWpf8pP8bLlkEtr32FWhNjQ2qIrric16fxSOQL4JT0QA")
chat_gpt$chat("Hola, como estás?")

# Actividad: Tome los primeros 100 comentarios del video del chicharito y analice si están a favor o en contra de su postura
cien_comentarios <- comentarios[1:100,] %>% as_tibble()
cien_comentarios
cien_comentarios$textDisplay

i = cien_comentarios$textDisplay[1] # Comentario de prueba. 
# Cuando hacemos un loop, hay que tener manera de probar que lo que estamos haciendo funcione
# Para eso, corremos el cuerpo del loop con un ejemplo de prueba (lo que hace unas clases vimos como el caso n = 1)
# Este código (i = cien_comentarios$textDisplay[1] # Comentario de prueba. ) no sirve ya de nada, pero en la clase lo usamos para probar el prompt
# Lo dejo vivo por si quieren hacer pruebas SOLO corriendo el cuerpo del loop. 
# Si tienen dudas de como funciona el lapply y como se diferencia de los loops for, revisen la presentación 11. 

resultados <- lapply(cien_comentarios$textDisplay, function(i){
  
  # Modificamos el prompt con respecto al de la clase para dar un poco más de contexto al modelo. 
  chat_gpt$chat("El siguiente es un comentario de un video en el que el Chicharito (jugador de futbol) emite un mensaje polémico.
                Clasifica los siguientes comentarios como a FAVOR o en CONTRA del Chicharito. 
                Solo devuelve la clasificacion. Si no lo puedes clasificar expresa 'No clasifica' 
                El comentario es el siguiente: ", 
                i)
  
})

# Esto nos costó aprox $0.2 USD. 

# Hacemos estadística con los resultados: 
resultados %>% as.character() %>% table()

# Guardamos el resultado
cien_comentarios_2 <- cien_comentarios %>% 
  select(textDisplay) %>% 
  mutate(clasificacion = resultados %>% as.character())
openxlsx::write.xlsx(cien_comentarios_2, "chicharito.xlsx")

# Cargamos el resultado (acá nos vamos si no le queremos poner las credenciales)
comentarios <- readxl::read_xlsx("chicharito.xlsx")

comentarios %>% 
  group_by(clasificacion) %>% 
  count() %>% 
  ggplot(aes(x =clasificacion, y = n, fill = clasificacion)) + 
  geom_col() + 
  geom_text(aes(label = n), 
            fontface = "bold", 
            vjust = -0.4) + 
  labs(y = "Total", x = "Tipo de respuesta", fill = "Tipo de respuesta", 
       title = "Clasificación de 100 comentarios al video: ", 
       subtitle = 'El polémico (y machista) mensaje de "Chicharito" a las mujeres: "Están erradicando la masculinidad"\nhttps://www.youtube.com/watch?v=7iJJYG7MrpQ', 
       caption = "Clasificación realizada con el API de ChatGPT de OPENAI.") + 
  scale_fill_manual(values = c("olivedrab", "brown", "navy")) + 
  scale_y_continuous(expand = expansion(c(0, 0.2))) + 
  theme_minimal()


