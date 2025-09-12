

# Librerías / Paquetes ----
library(curl)
library(tidyverse)

"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_01.xls"
"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_02.xls"
"https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_17.xls"

# Generar carpeta con código 

# Caso n = 1, caso = Aguascalientes
dir.create("tabulados_educacion")
curl_download(url = "https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_01.xls", destfile = "tabulados_educacion/01_educación.xls")

# Caso n = 1, caso = Baja California
dir.create("tabulados_educacion")
curl_download(url = "https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_02.xls", destfile = "tabulados_educacion/02_educación.xls")

dir.create("tabulados_educacion")

cve_ent = "32"
str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_", cve_ent, ".xls")

curl_download(url = str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_", cve_ent, ".xls"), destfile = str_c("tabulados_educacion/",cve_ent, "_educación.xls"))

estados <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", 
             10:32)

for(cve_ent in estados){
  curl_download(url = str_c("https://www.inegi.org.mx/contenidos/programas/ccpv/2010/tabulados/Basico/07_14B_MUNICIPAL_", cve_ent, ".xls"), destfile = str_c("tabulados_educacion/",cve_ent, "_educación.xls"))
  print(str_c("Ya está listo el archivo ", cve_ent))
}






