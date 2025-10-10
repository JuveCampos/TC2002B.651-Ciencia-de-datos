
library(httr2)
library(jsonlite)

comentarios <- readxl::read_xlsx("claude.xlsx") %>% 
  select(-Clasificación) %>% 
  unique()

# Prompt o instrucción
# prompt_text <- "¿Consideras que este comentario es ofensivo? Estaba en un video en el que se discutía la nueva propuesta de impuestos a videojuegos violentos para el próximo año:"


evaluar <- function(comentario){
  
    prompt_text <- str_c("¿Consideras que este comentario es ofensivo? Estaba en un video en el que se discutía la nueva propuesta de impuestos a videojuegos violentos para el próximo año:", "'", comentario, "'. Solo menciona si es 'Ofensivo' o 'No ofensivo', sin dar explicaciones")
  
    # Construimos la petición HTTP a Ollama
    resp <- request("http://localhost:11434/api/generate") |>
      req_body_json(list(
        model = "llama3.1:latest",   # Nombre exacto del modelo que tienes
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


bolsa_vacia2 <- bolsa_vacia %>%
  mutate(Es_Ofensivo = str_remove_all(Es_Ofensivo, pattern = "\\.")) %>% 
  mutate(Es_Ofensivo = str_replace_all(Es_Ofensivo, c("Sí, lo considero ofensivo" = "Ofensivo", 
                                       "Sí es ofensivo" = "Ofensivo")))
  
bolsa_vacia2 %>%
  openxlsx::write.xlsx("ofensivo_no_ofensivo_2.xlsx")



# bolsa_vacia
# 
# 
# comentarios <- readxl::read_xlsx("claude.xlsx") %>% 
#   filter(Es_Ofensivo != "Error") %>% 
#   select(-Clasificación)
# 
# cc <- rbind(bolsa_vacia, comentarios) %>% 
#   unique()
# 
# 
# cc %>% 
#   openxlsx::write.xlsx("ofensivo_noofensivo.xlsx")


# Comparación entre clasificaciones de modelos. 
# b1 <- readxl::read_excel("ofensivo_no_ofensivo.xlsx") %>% rename(Qwen = Es_Ofensivo)
# b2 <- readxl::read_excel("ofensivo_no_ofensivo_2.xlsx") %>% rename(Llama = Es_Ofensivo)
# 
# b3 <- left_join(b1, b2) %>% 
#   mutate(igual = Qwen == Llama) %>% 
#   mutate(final = case_when(igual ~ Qwen, 
#                            !igual ~ Llama, 
#                            is.na(igual) ~ Qwen))
# 
# # openxlsx::write.xlsx(b3, "ofensivo_no_ofensivo_3.xlsx")
# 
# prop.table(table(b3$igual))
