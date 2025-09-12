
library(rvest)
library(tidyverse)

url <- "https://atiempo.tv/author/juvenal-campos/page/2/"

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

tabla <- tibble(titulos, links, fecha)
