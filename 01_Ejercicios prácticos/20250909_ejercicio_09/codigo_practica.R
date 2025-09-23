
library(tidyverse)
library(curl)

a <- "hola"
b <- "adios"

str_c(a,b)

estados <- c(1:32)
link <- "link_de_descarga_"

str_c(link, estados)

"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_01.xls" # Ags
"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_02.xls" # BC
"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_32.xls"


ids_edos <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:32)

ligas_de_descarga <- str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_", 
      ids_edos,
      ".xls"
      )

dir.create("archivos_edu")

# Codigo de descarga
curl_download(ligas_de_descarga[1], 
              destfile = str_c("archivos_edu/", ids_edos[1], ".xls"))

# Codigo de descarga
i = 4
curl_download(ligas_de_descarga[i], 
              destfile = str_c("archivos_edu/", ids_edos[i], ".xls"))

for(i in 1:32){
  curl_download(ligas_de_descarga[i], destfile = str_c("archivos_edu/", ids_edos[i], ".xls"))
  print(str_c("Listo; ya está descargado el archivo ", i))
}





