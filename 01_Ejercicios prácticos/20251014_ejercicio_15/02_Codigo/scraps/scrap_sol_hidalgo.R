
# Cargar librerías necesarias
library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)

# URL del artículo
url <- "https://oem.com.mx//elsoldemexico/analisis/nombres-nombres-y-nombres-difunde-gobierno-de-eu-reporte-del-clima-de-inversion-aqui-describe-riesgos-de-politica-publica-inseguridad-y-corrupcion-26054390"

# Hacemos la función para la extracción individual: 
extaer_texto_pagina_individual <- function(url){
  
  # Leer la página web
  pagina <- read_html(url) 
  
  titulo <- pagina %>% 
    html_nodes("h1") %>% 
    html_text()
  
  fecha <- pagina %>% 
    html_nodes(".widget-section-and-date-default_wrapper__hHfEY") %>% 
    html_text()
  
  autor <- pagina %>% 
    html_nodes(".Typography_text-l__QLRR8") %>% 
    html_text() %>% 
    .[1]
  
  # Método 1: Extraer scripts que contengan datos JSON
  scripts <- pagina %>%
    html_nodes("script") %>%
    html_text()
  
  # Buscar el script que contiene el contenido del artículo
  json_script <- NULL
  for (script in scripts) {
    if (str_detect(script, "storyline_paragraph")) {
      json_script <- script
      break
    }
  }
  
  print(json_script)
  str_view(json_script, pattern = "storyline_paragraph", html = F)
  
  # json_text <- json_script
  
  # Función para extraer párrafos del JSON
  extraer_parrafos <- function(json_text) {
    # Esta expresión regular busca dentro de un texto (por ejemplo JSON incrustado en HTML)
    # todos los bloques donde el campo "type" sea "storyline_paragraph"
    # y luego extrae el contenido del campo "value" dentro de "paragraph".
    # En otras palabras, localiza los párrafos narrativos y obtiene solo su texto.
    # El grupo de captura `([^"]+)` es el que guarda el valor del párrafo.
    # Se usan `\\s*` para permitir espacios en blanco y `[^}]+?` para saltar el contenido intermedio
    # sin pasarse del cierre de llave. Los dobles backslash `\\` son necesarios porque
    # en R las barras invertidas deben escaparse dentro de las cadenas.
    patron <- '"type"\\s*:\\s*"storyline_paragraph"[^}]+?"paragraph"\\s*:\\s*\\{\\s*"value"\\s*:\\s*"([^"]+)"'
    
    matches <- str_match_all(json_text, patron)[[1]]
    
    if (nrow(matches) > 0) {
      parrafos <- matches[, 2]
      # Limpiar caracteres escapados
      parrafos <- gsub("\\\\n", "\n", parrafos)
      parrafos <- gsub('\\\\"', '"', parrafos)
      return(parrafos)
    }
    
    return(character(0))
  }
  
  # # Método 2: Intentar extraer directamente de elementos HTML
  # parrafos_html <- pagina %>%
  #   html_nodes("p") %>%
  #   html_text() %>%
  #   str_trim() %>%
  #   .[. != ""]
  
  # Extraer párrafos del JSON si existe
  if (!is.null(json_script)) {
    parrafos_json <- extraer_parrafos(json_script)
    
    if (length(parrafos_json) > 0) {
      cat("Párrafos extraídos del JSON:\n")
      cat("Total de párrafos:", length(parrafos_json), "\n\n")
      
      # Mostrar los primeros 3 párrafos
      for (i in 1:min(3, length(parrafos_json))) {
        cat("Párrafo", i, ":\n")
        cat(parrafos_json[i], "\n\n")
      }
      
      # Guardar todos los párrafos en un dataframe
      df_parrafos <- data.frame(
        numero = 1:length(parrafos_json),
        texto = parrafos_json,
        stringsAsFactors = FALSE
      )
      
      # Guardar: 
      texto = df_parrafos %>% as_tibble()
      
    } else {
      cat("No se pudieron extraer párrafos del JSON\n")
    }
  } else {
    cat("No se encontró script con JSON\n")
  }
  
  articulo_individual <- texto %>% 
    summarise(texto = str_c(texto, collapse = "\n")) %>% 
    mutate(titulo = titulo, 
           fecha = fecha, 
           autor = autor) %>% 
    select(titulo, fecha, autor, texto)
  
  return(articulo_individual)
  
}

# Ya con esta función podemos extraer lo que querramos dentro del sol de Toluca. 
extaer_texto_pagina_individual(url = "https://oem.com.mx/elsoldehidalgo/local/tepeji-del-rio-gobierno-de-hidalgo-retoma-mando-de-seguridad-26193630")

# Extraemos listas para extraer más artículos. 

# Checamos la estructura de las páginas: 
"https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=0"
"https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=1"
"https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=2"
"https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=3"

j = 0 # Probamos con el cero
url_general <- str_c("https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=", j) # Código de la página cero

# Preparamos bolsa vacía: 
ligas_a_articulos <- tibble()
# i = 1 # Numero de Prueba para probar el bucle

for(j in 0:10){
  
  url_general <- str_c("https://oem.com.mx/elsoldehidalgo/buscar/?q=inseguridad&page=", j) # Código de la página cero
  
  codigo_html <- read_html(url_general)
  
  titulos <- codigo_html %>% 
    html_nodes(".widget-search-default_grid__zImMM") %>% 
    html_nodes("h3") %>% 
    html_text()
  
  ligas <- codigo_html %>% 
    html_nodes(".widget-search-default_grid__zImMM") %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    unique()
  
  ligas_completas <- str_c("https://oem.com.mx/", ligas)
  
  ligas_escrapeadas <- tibble(titulos, link = ligas_completas)
  # Llenamos la bolsa vacía: 
  ligas_a_articulos <- rbind(ligas_a_articulos, ligas_escrapeadas) 
  print(str_c("Listo ", j))
}

# Verificamos que tengamos las ligas: 
ligas_a_articulos

# Ahora extraemos los datos de todas esas ligas: 
bolsa_vacia_articulos <- tibble()

# i =2 
for(i in 1:nrow(ligas_a_articulos)){
  tryCatch({
    datos <- extaer_texto_pagina_individual(ligas_a_articulos$link[i])%>% 
      mutate(liga = ligas_a_articulos$link[i])
    bolsa_vacia_articulos <- rbind(bolsa_vacia_articulos, datos) 
  }, error = function(e){
    print(str_c("No se pudo descargar la información en ", ligas_a_articulos$link[i]))
  })
}

bolsa_vacia_articulos

# Juntamos todos los artículos: 
total_final <- bolsa_vacia_articulos


# Guardamos en el excel: 
openxlsx::write.xlsx(total_final, "articulos_seguridad_sol_hidalgo.xlsx")
