
# Librerias ----
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(readxl)
library(tm)

# 1. Cargue el excel que contiene el texto de las mañaneras de la presidenta, el cual se encuentra en la carpeta “01_datos/estenograficas_sheinbaum.xlsx”.  ----
mananeras <- read_excel("01_datos/estenograficas_sheinbaum.xlsx") %>% 
  mutate(titulo = str_remove(titulo, "Versión estenográfica. "))

names(mananeras)

# 2. Genere un objeto con el contenido de la mañanera más reciente, y guardelo en el objeto mananera_reciente ----

mananera_reciente <- mananeras %>% head(1) # Cada mañanera esta en un renglón de los datos. Con head(1), nos quedamos con el primer renglón. 

# Para el objeto mananera_reciente, realice las siguientes actividades: 
# Elimine todos los acentos

mananera_reciente <- mananera_reciente %>% 
  # Reemplazamos las letras con acentos por letras sin acentos. 
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
# Verificamos: 
mananera_reciente$texto

# 3. Elimine todos los números ----
mananera_reciente <- mananera_reciente %>% 
  mutate(texto = str_remove_all(string = texto, pattern = "\\d"))

# 4. Descomponga el texto en tokens de frases. ----

frases <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, token = "sentences", to_lower = F)

# Frases con comma; respuesta a duda en clase. 
frases_coma <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, 
                token = "regex", pattern = "\\,", to_lower = F)

# 4. Descomponga el texto en tokens de ngramas (3,4 y 5). 

# No solicitado, pero lo agrego: 
mananera_ngrama_2 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 2)

mananera_ngrama_3 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 3)

mananera_ngrama_4 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 4)

mananera_ngrama_5 <- mananera_reciente %>% 
  unnest_tokens(output = "ngramas", input = texto, 
                token = "ngrams", to_lower = F, n = 5)

# 5. Descomponga el texto en tokens de palabras. Realice un wordcloud con estas palabras. Guarde este resultado en un objeto llamado “palabras” ----

palabras <- mananera_reciente %>% 
  unnest_tokens(output = "frases", input = texto, 
                token = "words", to_lower = T)

mananera_reciente %>% 
  unnest_tokens(output = "palabras", input = texto, 
                token = "words", to_lower = T) %>% 
  group_by(palabras) %>% 
  count() %>% 
  arrange(-n)


# Cargamos el código que contiene las funciones de visualización y que se encuentra en la carpeta 02_código: 
source("02_codigo/wordcloud_juve.r")

create_wordcloud(data = palabras$frases)

# 5. Realice un wordcloud con los ngramas
create_wordcloud(data = 
                   # Esta parte remueve los espacios (\\s) de los ngramas y los pega: 
                   str_remove_all(mananera_ngrama_3$ngramas,pattern = "\\s"), 
                 tamanio = 0.1)

# 6. Elimine las stopwords del objeto palabras ----
palabras

estopwords <- tibble(palabras = c(stopwords(kind = "es"), # stopwords() es una función de la librería {tm} que cargamos al inicio. 
                                  # En esta lista metemos las nuevas palabras que querramos eliminar. 
                                  c("presidenta", "mexico", "sheinbaum", "claudia", "pardo")))

# En el anti_join es donde efectivamente eliminamos las palabras. 
palabras2 <- palabras %>% anti_join(estopwords,
                                    by = c("frases" = "palabras"))
# Acá estamos definiendo la variable llave para la anti-unión. 
# Esto lo hacemos así porque se llama distinto en ambas tablas. 
# Pudimos también haber renombrado una de las dos columnas antes de hacer el anti-join.

# 7. Obtenga el stemming y los lemas de las palabras de la mañanera más reciente. Guarde estos en columnas adicionales del texto ----
palabras2 <- palabras2 %>% 
  mutate(stem = wordStem(words = frases, language = "spanish"))

# L E M A S #
# Esto ya no lo vimos en clase. 
# Lematización: Técnica para reducir las palabras a su forma base o lema, que es la forma que encontraríamos en el diccionario.
# Para más información, ver la presentación de clase. 

# Descargamos el modelo para lematizar: 
udpipe_download_model(language = "spanish")
modelo <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")

# Lematizamos en un objeto independiente
lemas_palabras2 <- udpipe_annotate(modelo, 
                                   x = palabras2$frases, 
                                   doc_id = palabras2$titulo) %>% 
  as_tibble() %>% 
  select(doc_id, 
         frases = sentence, # renombramos la columna sentence a "frases" para que coincida con el nombre anterior
         lemma, # Contiene los lemas correspondientes
         upos, # Extra, Contiene la clasificacion de las palabras
         feats # Extra, contiene características (features) de las palabras
         ) %>% 
  unique()
# Notese que la lematización también nos permite saber que palabras son verbos y que palabras son sustantivos (y otras clasificaciones)

# Juntamos este lema con el objeto palabras2 que si trabajamos en clase: 
palabras2 <- left_join(palabras2, lemas_palabras2, by = c("titulo" = "doc_id", 
                                             "frases" = "frases"))


# 8. Verifique el total de palabras que hay en la columna de palabras, en la columna de términos con el stemming y con la lematización. ¿Cual columna tiene el vocabulario más pequeño?  ----

length(unique(palabras2$frases)) # 2428 palabras unicas
length(unique(palabras2$stem)) # 1690 palabras podadas unicas
length(unique(palabras2$lemma)) # 1941 palabras lematizadas unicas

#   9. Cuente las palabras más utilizadas en la primera mañanera. Quédese con las primeras 10 en términos de frecuencia. 
palabras2 %>% 
  group_by(frases) %>% 
  filter(upos == "NOUN") %>% # Para filtrar todos los sustantivos
  count() %>% 
  filter(!(frases %in% c("si", 
                         "pregunta", 
                         "mas", 
                         "entonces"
                         ))) %>%  # Acá eliminamos las palabras que se nos olvidó eliminar en el anti_join()
  arrange(-n) %>% 
  head(10)


# 10. Realice este procedimiento en bucle para el resto de las conferencias. 
mananeras_10 <- mananeras %>% head(10)

palabras_10 <- mananeras_10 %>% 
  unnest_tokens(output = "palabras", input = texto,
                token = "words", to_lower = T) %>% 
  select(-fecha) %>% 
  anti_join(estopwords, by = c("palabras" = "palabras"))

# Conseguimos los lemas: 
lemas_de_palabras_10 <- udpipe_annotate(modelo, 
                  x = palabras_10$palabras, 
                  doc_id = palabras_10$titulo) %>% 
  as_tibble() %>% 
  select(doc_id, 
         palabras = sentence,
         lemma,
         upos,
         feats) %>% 
  unique()
  
palabras_10 <- left_join(palabras_10, 
                         lemas_de_palabras_10, 
                         by = c("titulo" = "doc_id",
                                "palabras" = "palabras"))  %>% 
  filter(upos == "NOUN") # Filtramos los sustantivos

diez_palabras_por_mananera <- palabras_10 %>%
  filter(!(palabras %in% c("si", 
                         "pregunta", 
                         "mas", 
                         "entonces",
                         "méxico",
                         "layda", 
                         "tema", 
                         "gracias", 
                         "país", 
                         "asisentes"
  ))) %>%  # Acá eliminamos las palabras que se nos olvidó eliminar en el anti_join()
  group_by(titulo, palabras) %>% # Agrupar por mañanera y palabras nos permitirá saber, para cada mañanera, que tanto se repiten las palabras
  count() %>%  # Contamos
  arrange(titulo, -n) %>% # Ordenamos de mayor a menor
  ungroup() %>% # Desagrupamos
  group_by(titulo) %>% # Agrupamos por mañanera
  mutate(rank = rank(-n, ties.method = "first")) %>% # Para cada mañanera, le asignamos un lugar a las palabras en función de su frecuencia (de mayor a menor, por eso el símbolo de "-")
  filter(rank <= 10) # Nos quedamos con los diez primeros lugares de mayor frecuencia

diez_palabras_por_mananera

# 11. Una vez que tiene las diez palabras más populares de todas las mañaneras, realice un wordcloud con estas palabras. 
# ESTO YA NO LO VIMOS EN CLASE: 
create_wordcloud(diez_palabras_por_mananera$palabras)

# 12. Utilizando zero-shot prompting, trate de determinar de que trató cada conferencia mañanera. Agrupe las mañaneras con temas similares. 
palabras_mananera <- diez_palabras_por_mananera %>% 
  group_by(titulo) %>% 
  summarise(vector_palabras = str_c(palabras, collapse = ", ")) 

library(ellmer)

chat <- ellmer::chat_openai() # Cargamos el chat de chatgpt. Ustedes carguen el que vieron con hugging face. 

# palabras = palabras_mananera$vector_palabras[1]
temas_mananera <- lapply(palabras_mananera$vector_palabras, function(palabras){
  
  chat$chat(str_c("A partir del siguiente vector de palabras de mayor frecuencia dichas en la mañanera de la presidenta de México, 
                  dime de que pudo haber tratado la mañanera correspondiente. Solo dime la frase que describa al tema o temas, separados por ';' ", 
                  palabras))
  
})

palabras_mananera$temas_mananera <- as.character(temas_mananera)
palabras_mananera
