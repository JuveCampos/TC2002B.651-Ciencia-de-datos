
# Librerías ----
library(tidyverse) # Manejo de datos
library(udpipe)    # Lematización
library(SnowballC) # Stemming, podado de palabras
library(widyr)     # Comparaciones entre textos
library(tidytext)  # Funciones de procesamiento de texto
library(tm)        # Minería de texto; para obtener nuestros stop_words

# Cargamos los datos: 
total_mananeras <- 10
stop_words <- tibble(palabras = tm::stopwords(kind = "spanish")) 

# Generamos un catálogo de las mañaneras con ID
catalogo_mananeras <- readxl::read_xlsx("01_datos/estenograficas_sheinbaum.xlsx") %>% 
  mutate(titulo = str_remove_all(titulo, "Versión estenográfica.\\s")) %>% 
  select(titulo, fecha) %>% 
  unique() %>% 
  mutate(ID = 1:nrow(.)) %>% 
  relocate(ID, .before = titulo)

# Procesamos las mañaneras (de lo visto en clase pasada)
mananeras <- readxl::read_xlsx("01_datos/estenograficas_sheinbaum.xlsx") %>% 
  head(total_mananeras) %>%
  mutate(titulo = str_remove_all(titulo, "Versión estenográfica.\\s")) %>% 
  mutate(texto = str_replace_all(texto, c("á" = "a", 
                                          "é" = "e", 
                                          "í" = "i", 
                                          "ó" = "o", 
                                          "ú" = "u", 
                                          "Á" = "A", 
                                          "É" = "E", 
                                          "Í" = "I", 
                                          "Ó" = "O", 
                                          "Ú" = "U"))) %>% 
  mutate(texto = str_remove_all(string = texto, pattern = "\\d")) %>% 
  mutate(texto = str_remove_all(texto, pattern = "\\b[A-ZÁÉÍÓÚÜÑ]{2,}\\b")) %>% 
  mutate(texto = str_remove_all(texto, pattern = ": Estimado publico escuchemos el mensaje que nos dirige la Presidenta Constitucional de los Estados Unidos Mexicanos, la Doctora Claudia Sheinbaum Pardo.")) %>% 
  mutate(texto = str_remove_all(texto, pattern = ": Estimado publico, escuchemos el mensaje que nos dirige la Presidenta Constitucional de los Estados Unidos Mexicanos, Doctora Claudia Sheinbaum Pardo")) %>% 
  mutate(texto = str_remove_all(texto, pattern = "[:punct:]")) %>% 
  mutate(texto = str_remove_all(texto, pattern = "\\º")) %>% 
  mutate(texto = str_squish(texto)) %>% 
  mutate(texto = str_to_lower(texto)) %>% 
  mutate(texto = str_replace_all(texto, c("doctora claudia sheinbaum pardo" = "doctora_claudia_sheinbaum_pardo", 
                                          "claudia sheinbaum pardo" = "claudia_sheinbaum_pardo", 
                                          "mil millones" = "mil_millones", 
                                          "andres manuel lopez obrador" = "andres_manuel_lopez_obrador", 
                                          "lopez obrador" = "lopez_obrador", 
                                          "cuarta transformacion" = "cuarta_transformacion",
                                          "grito de independencia" = "grito_de_independencia", 
                                          "salario minimo" = "salario_minimo", 
                                          "ley de aguas" = "ley_de_aguas", 
                                          "ley de aguas nacionales" = "ley_de_aguas_nacionales", 
                                          "aguas nacionales" = "aguas_nacionales",
                                          "ciudad juarez" = "ciudad_juarez", 
                                          "ciudad de mexico" = "ciudad_de_mexico", 
                                          "nuevo leon" = "nuevo_leon", 
                                          "san luis potosi" = "san_luis_potosi", 
                                          "estados unidos" = "estados_unidos", 
                                          "secretaria de gobernacion" = "secretaria_de_gobernacion", 
                                          "defensa nacional" = "defensa_nacional", 
                                          "guardia nacional" = "guardia_nacional", 
                                          "seguro social" = "seguro_social", 
                                          "suprema corte de justicia" = "suprema_corte_de_justicia"
  )))

# 1. Dado el script 13_01_ejercicio.R, realice los siguientes ejercicios: ----
# 1) Descargue el modelo de udpipe necesario para lematizar las palabras utilizadas en español
modelo <- udpipe_download_model(language = "spanish")
udmodel <- udpipe_load_model(modelo$file_model)

# 2) A partir de este modelo, genere a partir de las 10 mañaneras más recientes un objeto que tenga las palabras, los tokens, los lemmas, los stems, el upos (clasificación de la palabra) y los Feats (características de la palabra). Esto lo calcula la funcion udpipe_annotate().
# udpipe_annotate(modelo, x = mananeras$texto)

ud_mananeras <- mananeras %>% 
  unnest_tokens(output = "palabras", input = texto, token = "words", to_lower = T)

ud_mananeras2 <- udpipe_annotate(udmodel, doc_id = ud_mananeras$titulo, x = ud_mananeras$palabras) %>% 
  as_tibble() %>% 
  select(doc_id, token, lemma, feats) %>% 
  mutate(stem = wordStem(words = token, language = "spanish"))

# 3) Cuente el total de términos únicos que hay en la columna de tokens, la columna de lemma y la columna de stem. ¿Cuál tiene menos términos? ¿Cuál usaría para saber cuales son las palabras más relevantes por mañanera? ¿Por qué? 

length(unique(ud_mananeras2$token))
length(unique(ud_mananeras2$lemma))
length(unique(ud_mananeras2$stem))

# 2. OBTENGA EL VALOR DE TF-IDF DE LAS MAÑANERAS ----
# Dado el objeto que guarda las 10 mañaneras más recientes: 
# 1) Calcule la matríz tf-idf para las 10 mañaneras más recientes

tf_idf_mananeras <- ud_mananeras2 %>% 
  anti_join(stop_words, by = c("lemma" = "palabras")) %>% 
  group_by(doc_id, lemma) %>% 
  count() %>% 
  bind_tf_idf(term = lemma, n = n, document = doc_id) %>% 
  arrange(doc_id, -tf_idf) 

# 2) Filtre para quedarse con los 10 términos con el tf-idf más alto. 
diez_palabras_mananera <- tf_idf_mananeras %>% 
  group_by(doc_id) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  filter(ranking <= 10)

# 3) Compare las 10 palabras más frecuentes y las 10 con el tf-idf más alto. ¿Se parecen? ¿Cuál considera que es el mejor método para obtener los términos más relevantes?

# 3. Dadas las 10 mañaneras más recientes ----
# 1) Obtenga la similitud del coseno entre cada una de ellas, a partir del cálculo de TF-IDF, y
# 2) Determine cuales son las mañaneras más parecidas. 

widyr::pairwise_similarity(tbl = tf_idf_mananeras, item = doc_id, 
                           feature = lemma, value = n)

tf_idf_mananeras %>% 
  widyr::pairwise_similarity(item = doc_id, 
                             feature = lemma, 
                             value = n)

ud_mananeras2 %>% 
  group_by(doc_id, token) %>% 
  count() %>% 
  ungroup() %>% 
  bind_tf_idf(term = token, document = doc_id, n = n) %>% 
  widyr::pairwise_similarity(item = doc_id, feature = token, value = n) %>% 
  View()


