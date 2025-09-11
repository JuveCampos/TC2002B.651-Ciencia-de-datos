
# Librerias ----
library(tidyverse)
library(rvest)


liga <- "https://en.wikipedia.org/wiki/Legality_of_cannabis"

legalidad <-read_html(liga) %>% 
  html_table() %>% 
  pluck(2)

# legalidad %>% 
#   filter(Recreational == "Illegal")

# Ejercicio 02
# url <- "https://atiempo.tv/author/juvenal-campos/"
url <- "https://atiempo.tv/author/juvenal-campos/page/2/"

# Guarda titulos
titulos <- read_html(url) %>% 
  html_nodes(".entry-title") %>% 
  html_text()

# Guarda links
links <- read_html(url) %>% 
  html_nodes(".entry-title") %>% 
  html_nodes("a") %>% 
  html_attr("href")

fechas <- read_html(url) %>% 
  html_nodes(".entry-date") %>% 
  html_text() %>% 
  unique()



