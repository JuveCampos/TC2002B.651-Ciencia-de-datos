
library(tidyverse)
library(httr2)
library(jsonlite)

comentarios <- readxl::read_xlsx("01_Datos/ofensivo_no_ofensivo.xlsx") %>% 
  unique()

# Prompt o instrucción
# prompt_text <- "¿Consideras que este comentario es ofensivo? Estaba en un video en el que se discutía la nueva propuesta de impuestos a videojuegos violentos para el próximo año:"


evaluar <- function(comentario, modelo_sel = "llama3.1:latest"){
  
    prompt_text <- str_c("¿Considerarías que el siguiente comentario es ofensivo? 
                         Es decir, que se exprese de forma vulgar, soez, agresiva, amenazante u ofensiva hacia otra persona o grupo, de forma sarcastica o irónica
                         Este comentario estaba en un video en el que se discutía la nueva propuesta de impuestos a videojuegos violentos para el próximo año:", "'", 
                         comentario, 
                         "'. Solo menciona si es 'Ofensivo' o 'No ofensivo'. No des explicaciones adicionales. No uses símbolos de puntuación")
  
    # Construimos la petición HTTP a Ollama
    resp <- request("http://localhost:11434/api/generate") |>
      req_body_json(list(
        model = modelo_sel,   # Nombre exacto del modelo que tienes
        prompt = prompt_text,
        stream = FALSE              # FALSE para obtener todo el texto de una vez
      )) |>
      req_perform()
    
    # Procesamos la respuesta JSON
    result <- resp_body_json(resp)
    result$response
    
}

bolsa_vacia <- tibble()

# c = 2
for(c in seq_along(comentarios$textOriginal)){
  
  tibble_respuesta <- tibble(textOriginal = comentarios$textOriginal[c],
                             `Es_Ofensivo` = evaluar(comentarios$textOriginal[c]))
  bolsa_vacia <- rbind(bolsa_vacia,tibble_respuesta)
  print(str_c("Listo ", c))
}

bolsa_vacia


bolsa_vacia2 <- tibble()

# c = 2
for(c in seq_along(comentarios$textOriginal)){
  
  tibble_respuesta <- tibble(textOriginal = comentarios$textOriginal[c],
                             `Es_Ofensivo` = evaluar(comentarios$textOriginal[c], modelo_sel = "qwen2.5:latest"))
  bolsa_vacia2 <- rbind(bolsa_vacia2,tibble_respuesta)
  print(str_c("Listo ", c))
}

bolsa_vacia2


left_join(bolsa_vacia %>% rename("Ollama"="Es_Ofensivo"), 
          bolsa_vacia2 %>% rename("Qwen"="Es_Ofensivo")) %>% 
  mutate(igual = Ollama == Qwen) %>% 
  openxlsx::write.xlsx("01_Datos/comparacion_etiquetas.xlsx")

