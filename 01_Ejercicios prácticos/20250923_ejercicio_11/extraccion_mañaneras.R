
# Librerias
library(tidyverse) # Manejo de datos
library(rvest)     # Web scraping en R

# Url de las mañaneras
url <- "https://www.gob.mx/presidencia/es/archivo/articulos?idiom=es&order=DESC&page=2"

# Hasta que página tenemos info de 2025? 
# VERIFIQUEMOS! 

p = 1 # Arrancamos con el valor de 1 para que cheque desde esta página

while (switch == FALSE) {

  url <- str_c("https://www.gob.mx/presidencia/es/archivo/articulos?idiom=es&order=DESC&page=", p)
  html <- read_html(url)  
  
  fechas <- html %>% 
    html_nodes("time") %>% 
    html_attr("date") %>% 
    str_remove_all(pattern = '\\"|\\\\') %>% 
    as_date()
  
  # ¿Hay alguna fecha menor o igual al 31 de diciembre de 2024? 
  switch <- sum(fechas <= "2024-12-31") # FALSE = NO, TRUE = SI. Switch se va a volver diferente de cero cuando encuentre una fecha de 2024. 
  print(str_c("Las versiones estenográficas de la página ", p, " son todas de 2025"))
  
  # Si no encuentra una mañanera de 2024, le decimos que sume uno
  # Hasta que encuentre una mañanera de 2024
  if(switch == 0){
    p <- p + 1
  }
  
}

# Página última que vamos a escrapear
numero_pagina_ultima <- p


# Bucle de descarga
estenograficas <- lapply(1:numero_pagina_ultima, function(p){

    
    url <- str_c("https://www.gob.mx/presidencia/es/archivo/articulos?idiom=es&order=DESC&page=", p)
    html <- read_html(url)  
    
    ligas <- html %>% 
      html_nodes("article") %>% 
      html_nodes("a") %>% 
      html_attr("href") 
    
    ligas <- ligas[c(1:9)] %>% 
      str_remove_all(pattern = '\\\\') %>% 
      str_remove_all(pattern = '\"')
    
    ligas_completas <- str_c("https://www.gob.mx", ligas)
    
    # Acá como pueden ver tenemos un bucle dentro de un bucle. 
    cosecha_subpaginas <- lapply(seq_along(ligas_completas), function(l){
      
        subpagina <- read_html(ligas_completas[l])  
        
        texto <- subpagina %>% 
          html_nodes("p") %>% 
          html_text() %>% 
          str_c(collapse = "\n")
        
        titulo <- subpagina %>% 
          html_nodes("h1") %>% 
          html_text() %>% 
          str_c(collapse = "\n")
        
        fecha <- subpagina %>% 
          html_nodes("div") %>% 
          html_nodes("section") %>% 
          html_text() %>% 
          str_squish() %>% 
          str_remove_all(pattern = "Presidencia de la República") %>% 
          str_remove_all(pattern = "\\s\\|\\s")
        
        print(str_c("Listo articulo ", ligas_completas[l]))
        
        tibble(titulo, texto, fecha) %>% return()
        
    })
    
    print(str_c("Listo página ", p))
    
    tabla <- cosecha_subpaginas %>% 
      do.call(rbind, .) 
    
    tabla %>% 
      return()
    
})

# Ver la presentación 11 para entender que pasa acá
# Pero en resumen, con los datos del bucle anterior (que son una lista) hicimos una tabla
estenograficas_tibble <- estenograficas %>% 
  do.call(rbind, .)

# Limpiamos un poco los datos. Les quitamos paja
estenograficas_tibble2 <- estenograficas_tibble %>% 
  # quitamos los mensajes al final de la página: 
  mutate(texto = str_remove_all(texto, "Leer más $")) %>% 
  mutate(texto = str_remove_all(texto, "Es el portal único de trámites, información y participación ciudadana.")) %>% 
  mutate(texto = str_remove_all(texto, pattern = "Presidencia de la República | \\d+ de \\w+ de \\d+")) %>% 
  mutate(texto = str_remove_all(texto, pattern = "\\s\\|\\s")) %>% 
  mutate(texto = str_replace_all(texto, pattern = "\n", replacement = " ")) %>% 
  mutate(texto = str_squish(texto)) %>% 
  mutate(texto = str_remove_all(texto, " —000— Puedes seleccionar más de uno. Puedes seleccionar más de una opción. Gracias por tu opinión. La legalidad, veracidad y la calidad de la información es estricta responsabilidad de la dependencia, entidad o empresa productiva del Estado que la proporcionó en virtud de sus atribuciones y/o facultades normativas."))
  
# Guardamos las versiones estenográficas en un excel : 
openxlsx::write.xlsx(estenograficas_tibble2, "estenograficas_sheinbaum.xlsx")

# Las guardamos en un RDS: 
saveRDS(estenograficas_tibble2, "estenograficas_sheinbaum.rds")

