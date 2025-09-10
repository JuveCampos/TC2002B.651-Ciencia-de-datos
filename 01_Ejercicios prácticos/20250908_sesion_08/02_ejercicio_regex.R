
# Cargue las librerías necesarias para manejar texto (stringr o tidyverse)
library(tidyverse)
library(readxl)

# 1. Cargue el archivo "01_datos/dialogos.xlsx". Guardelo en un objeto llamado "dialogos"
dialogos <- read_excel("01_datos/dialogos.xlsx")

# 2. En el objeto "dialogos", genere una nueva columna donde venga el numero que cada persona dice ocupar

dialogos <- dialogos %>% 
  mutate(id = str_extract(dialogo, pattern = "\\d+"))


# 3. En el objeto "dialogos", genere una nueva columna donde venga el numero telefónico
dialogos <- dialogos %>% 
  mutate(dialogo = str_remove_all(dialogo, pattern = "-")) %>% 
  mutate(num_tel = str_extract(dialogo, pattern = "\\d{10}"))

# 4. En el objeto "dialogos", genere una nueva columna donde venga solo el correo electrónico de cada persona

dialogos <- dialogos %>% 
  mutate(correo = str_extract(dialogo, 
  pattern = "\\w+\\.\\w+\\@[\\w+\\.-]+"))

# 5. En el objeto "dialogos", genere una nueva columna donde venga solo el nombre de la persona. 

dialogos <- dialogos %>% 
  mutate(nombre_completo = str_extract(dialogo, 
                                       pattern = "^(.*?):")) %>% 
  mutate(nombre_completo = str_remove(nombre_completo, 
                                      pattern = "\\:$"))



# 6. ¿Quien tiene el nombre más largo? 
dialogos <- dialogos %>% 
  mutate(longitud_texto = str_length(str_remove_all(nombre_completo, pattern = "\\s")))


# 7. ¿Cuantos números tienen lada de la CDMX (55)?
dialogos2 <- dialogos %>% 
  mutate(lada = str_extract(num_tel, pattern = "\\d{2}")) %>% 
  filter(lada == "55")

