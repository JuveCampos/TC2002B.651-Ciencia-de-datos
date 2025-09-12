
library(rvest)
library(tidyverse)

# Caso n = 2

n <- 2

tabla <- tibble()

for(n in 1:10){

    url <- str_c("https://atiempo.tv/author/juvenal-campos/page/", n, "/")
    
    pagina <- read_html(url)
    
    titulos <- pagina %>% 
      html_nodes(".entry-title") %>% 
      html_text()
    
    links <- pagina %>% 
      html_nodes(".entry-title") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    fecha <- pagina %>% 
      html_nodes(".entry-date") %>% 
      html_text() %>% 
      unique()
    
    tabla <- rbind(tabla, tibble(titulos, fecha, links))
    print(str_c("Ya está listo el contenido de la página ", n))
}


textos <- tibble()
imagenes2 <- tibble()

i = 2
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

tabla2 <- cbind(tabla, 
                textos = textos[,1], 
                imagenes2 = imagenes2[,1])

openxlsx::write.xlsx(tabla2, "tabla_archivos_juve.xlsx")


