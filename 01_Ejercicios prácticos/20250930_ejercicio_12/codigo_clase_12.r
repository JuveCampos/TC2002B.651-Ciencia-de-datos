
# Librerias ----
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(readxl)
library(tm)

# Cargue el excel que contiene el texto de las mañaneras de la presidenta, el cual se encuentra en la carpeta “01_datos/estenograficas_sheinbaum.xlsx”. 

mananeras <- read_excel("01_datos/estenograficas_sheinbaum.xlsx") %>% 
  mutate(titulo = str_remove(titulo, "Versión estenográfica. "))

names(mananeras)

# Genere un objeto con el contenido de la mañanera más reciente, y guardelo en el objeto mananera_reciente

mananera_reciente <- mananeras %>% head(1)

# Para el objeto mananera_reciente, realice las siguientes actividades: 
  # Elimine todos los acentos

mananera_reciente <- mananera_reciente %>% 
  mutate(texto = str_replace_all(texto, c("á" = "a", 
                                          "é" = "e", 
                                          "í" = "i", 
                                          "ó" = "o", 
                                          "ú" = "u", 
                                          "Á" = "A", 
                                          "É" = "E", 
                                          "Í" = "I", 
                                          "Ó" = "O", 
                                          "Ú" = "U")))

mananera_reciente$texto

# Elimine todos los números
mananera_reciente <- mananera_reciente %>% 
  mutate(texto = str_remove_all(string = texto, pattern = "\\d"))

# 3. Descomponga el texto en tokens de frases. 

frases <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, token = "sentences", to_lower = F)

frases_coma <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, 
                token = "regex", pattern = "\\,", to_lower = F)

# 4. Descomponga el texto en tokens de ngramas (3,4 y 5). 

mananera_ngrama_3 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 3)

mananera_ngrama_4 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 4)

mananera_ngrama_5 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 5)

# 4. Descomponga el texto en tokens de palabras. Realice un wordcloud con estas palabras. Guarde este resultado en un objeto llamado “palabras”

palabras <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, 
                token = "words", to_lower = T)

source("02_codigo/wordcloud_juve.r")

create_wordcloud(data = palabras$frases)

# 5. Realice un wordcloud con los ngramas
create_wordcloud(data = str_remove_all(mananera_ngrama_3$ngramas,
                                       pattern = "\\s"), 
                 tamanio = 0.1)

# 6. Elimine las stopwords del objeto palabras 
palabras

estopwords <- tibble(palabras = c(stopwords(kind = "es"), c("presidenta", 
                                                            "mexico", 
                                                            "sheinbaum", 
                                                            "claudia", 
                                                            "pardo")))
palabras2 <- palabras %>% 
  anti_join(estopwords, by = c("frases" = "palabras"))

# 7. Obtenga el stemming y los lemas de las palabras de la mañanera más reciente. Guarde estos en columnas adicionales del texto

palabras2 <- palabras2 %>% 
  mutate(stem = wordStem(words = frases, language = "spanish"))

# 8. Verifique el total de palabras que hay en la columna de palabras, en la columna de términos con el stemming y con la lematización. ¿Cual columna tiene el vocabulario más pequeño? 

length(unique(palabras2$frases)) # 2428 palabras unicas
length(unique(palabras2$stem)) # 1690 palabras podadas unicas

#   9. Cuente las palabras más utilizadas en la primera mañanera. Quédese con las primeras 10 en términos de frecuencia. 
palabras2 %>% 
  group_by(frases) %>% 
  count() %>% 
  # filter(!(frases %in% c("presidenta", 
  #                        "mexico", 
  #                        "sheinbaum", 
  #                        "claudia", 
  #                        "pardo"))) %>% 
  arrange(-n) %>% 
  head(10)


# 10. Realice este procedimiento en bucle para el resto de las conferencias. 
mananeras_10 <- mananeras %>% head(10)

palabras_10 <- mananeras_10 %>% 
  unnest_tokens(output = "palabras", input = texto,
                token = "words", to_lower = T) %>% 
  select(-fecha) %>% 
  anti_join(estopwords, by = c("palabras" = "palabras")) %>% 
  group_by(titulo, palabras) %>% 
  count() %>% 
  arrange(titulo, -n) %>% 
  ungroup() %>% 
  group_by(titulo) %>% 
  mutate(rank = rank(-n, ties.method = "first")) %>% 
  filter(rank <= 10)


# 11. Una vez que tiene las diez palabras más populares de todas las mañaneras, realice un wordcloud con estas palabras. 
# 12. Utilizando zero-shot prompting, trate de determinar de que trató cada conferencia mañanera. Agrupe las mañaneras con temas similares. 


