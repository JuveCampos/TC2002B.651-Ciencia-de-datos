paquetes <- c('tidyverse', 'tidytext', 'readxl', 'topicmodels', 'tm', 'ggplot2', 'stopwords')
instalados <- paquetes %in% rownames(installed.packages())
faltantes <- paquetes[!instalados]

if(length(faltantes) > 0) {
  cat('Paquetes faltantes:', paste(faltantes, collapse=', '), '\n')
} else {
  cat('Todos los paquetes necesarios estan instalados\n')
}
