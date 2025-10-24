
# ============================================================
# LIBRERÍAS NECESARIAS PARA MODELADO DE TÓPICOS
# ============================================================
# libreria ---
library(tm)            # Text Mining: contiene funciones básicas para minería de texto,
                       # incluyendo stopwords() para obtener palabras vacías en diferentes idiomas
library(topicmodels)   # Topic Models: implementa algoritmos de modelado de tópicos,
                       # principalmente LDA (Latent Dirichlet Allocation)
library(tidytext)      # Tidy Text: facilita el análisis de texto con enfoque tidy,
                       # incluye cast_dtm() para crear matrices documento-término
library(tidyverse)     # Conjunto de paquetes para manipulación y visualización de datos
                       # (dplyr, ggplot2, stringr, etc.)
library(readxl)        # Lectura de archivos Excel (.xlsx)
library(rebus)         # Construcción de expresiones regulares de forma más legible
library(udpipe)        # UDPipe: lematización y etiquetado gramatical usando modelos de
                       # Procesamiento de Lenguaje Natural (NLP)

# ============================================================
# CARGA DE DATOS
# ============================================================
# Cargamos los datos ----
# Los artículos provienen de lo escrapeado en la clase de web scrpaing.
articulos_juve <- read_excel("01_datos/tabla_archivos_juve.xlsx") %>%
  select(titulos, textos) %>%        # Seleccionamos solo las columnas de títulos y textos
  mutate(id = 1:nrow(.)) %>%         # Creamos un ID único para cada documento (artículo)
  relocate(id, .before = titulos)    # Movemos la columna 'id' al principio del dataframe

# ============================================================
# PREPROCESAMIENTO DE TEXTO
# ============================================================
# Preprocesamiento ----

# Paso 1: Generación de stopwords (palabras vacías)
# Generamos las stopwords
stop_words <- stopwords(kind = "spanish")  # Palabras comunes sin valor semántico
                                            # (ej: "el", "la", "de", "en", etc.)

# Paso 2: Agregar límites de palabra a las stopwords
# Les pegamos un límite a la izquierda y a la derecha
stopwords_2 <- str_c("\\b", stop_words, "\\b")  # \\b asegura coincidencia exacta de palabras
                                                 # Evita eliminar partes de palabras más largas

# Paso 3: Pipeline completo de limpieza de texto
# Limpiamos el texto:
articulos_juve2 <- articulos_juve %>%
  # 3.1: Convertir todo el texto a minúsculas (normalización)
  mutate(textos = str_to_lower(textos)) %>%
  # 3.2: Eliminar signos de puntuación ([:punct:] incluye .,;:!?¿¡ etc.)
  mutate(textos = str_remove_all(textos, pattern = "[:punct:]")) %>%
  # 3.3: Eliminar stopwords (or1() crea un patrón regex que busca cualquiera de las stopwords)
  mutate(textos = str_remove_all(textos, c(or1(stopwords_2)))) %>%
  # 3.4: Unir n-gramas (frases de varias palabras) con guiones bajos
  # IMPORTANTE: Esto se hace para que frases con significado conjunto se traten como un solo término
  # Por ejemplo: "inteligencia artificial" → "inteligencia_artificial"
  mutate(textos = str_replace_all(textos, c("redes sociales" = "redes_sociales",
                                            "medio comunicación" = "medio_comunicacion",
                                            "nativo digital" = "nativo_digital",
                                            "inteligencia artificial" = "inteligencia_artificial",
                                            "\\bia\\b" = "inteligencia_artificial",  # Acrónimo común de IA
                                            "nuevo león" = "nuevo_león",
                                            "ciudad méxico" = "ciudad_de_méxico",
                                            "jornada laboral" = "jornada_laboral",
                                            "manolo jiménez" = "manolo_jiménez",
                                            "población económicamente activa" = "población_económicamente_activa"
                                            ))) %>%
  # 3.5: Eliminar espacios múltiples y espacios al inicio/final (str_squish)
  mutate(textos = str_squish(textos)) 
  
# ============================================================
# LEMATIZACIÓN CON UDPIPE
# ============================================================
# CÓDIGO COMENTADO: Análisis de n-gramas (no utilizado en este flujo)
# Este código buscaría trigramas (secuencias de 3 palabras) y contaría su frecuencia
# ngramas <- articulos_juve2 %>%
#   unnest_tokens(output = "palabra",
#                 input = textos,
#                 token = "ngrams", n = 3) %>%
#   group_by(palabra) %>%
#   count() %>%
#   arrange(-n)

# Paso 1: Descargar el modelo de lenguaje español de UDPipe
# Este modelo contiene las reglas gramaticales y diccionarios para español
modelo <- udpipe_download_model(language = "spanish")

# Paso 2: Cargar el modelo descargado en memoria
udmodel <- udpipe_load_model(modelo$file_model)

# Paso 3: Proceso completo de lematización y filtrado
# ¿QUÉ ES LA LEMATIZACIÓN? Convertir palabras a su forma base o raíz
# Por ejemplo: "corrieron" → "correr", "mejores" → "bueno"
lemas_juve <- udpipe_annotate(udmodel, x = articulos_juve2$textos) %>%
  # Convertir el resultado de anotación a formato tibble (tabla tidy)
  as_tibble() %>%
  # Extraer el ID del documento desde doc_id (que tiene formato "doc1", "doc2", etc.)
  mutate(id = str_extract(doc_id, pattern = "\\d+")) %>%
  # Seleccionar columnas relevantes:
  # - token: palabra original del texto
  # - lemma: forma base de la palabra
  # - upos: etiqueta gramatical universal (Universal Part-Of-Speech)
  #         NOUN=sustantivo, VERB=verbo, ADJ=adjetivo, etc.
  select(id, token, lemma, upos) %>%
  # Eliminar signos de puntuación (ya procesados como tokens por udpipe)
  filter(upos != "PUNCT") %>%
  # Identificar y eliminar tokens que son números
  mutate(is.number = str_detect(lemma, pattern = "\\d")) %>%
  filter(!is.number) %>%
  # Filtrado manual adicional: eliminar lemas específicos que no aportan valor
  # (stopwords adicionales personalizadas según el contexto)
  filter(!(lemma %in% c("él", "hacer", "Coahuila",
                        "México","méxico","poder", "primero", "mayor", "año",
                        "bien", "cada", "solo")))


# ============================================================
# CREACIÓN DE LA MATRIZ DOCUMENTO-TÉRMINO (DTM)
# ============================================================
# Matriz documento término
# ¿QUÉ ES UNA DTM? Una matriz donde:
# - Cada FILA representa un documento (artículo)
# - Cada COLUMNA representa un término (palabra/lema único)
# - Cada CELDA contiene la frecuencia de ese término en ese documento
# Esta representación numérica es necesaria para aplicar algoritmos de ML
dtm_juve <- lemas_juve %>%
  # Agrupar por documento y lema para contar ocurrencias
  group_by(id, lemma) %>%
  count() %>%                         # n = frecuencia del lema en el documento
  # cast_dtm() convierte el formato tidy a una matriz DTM (formato DocumentTermMatrix)
  cast_dtm(document = id, term = lemma, value = n)

# Visualización de la estructura de la DTM:
# Así se ven los primeros renglones de la matríz dtm:
# (Convertimos a matriz normal y luego a tibble para visualizar)
dtm_juve %>% as.matrix() %>% as_tibble()

# ============================================================
# MODELADO DE TÓPICOS CON LDA
# ============================================================
# Modelado de tópicos
# ¿QUÉ ES LDA? Latent Dirichlet Allocation es un modelo probabilístico no supervisado
# que descubre temas "ocultos" (latentes) en una colección de documentos.
# Asume que cada documento es una mezcla de temas y cada tema es una mezcla de palabras.
#
# La función LDA es la que va a hacer el modelo no supervizado de
# Latent Dirichlet Allocation
lda_juve <- LDA(x = dtm_juve,           # Matríz DTM (entrada del modelo)
                k = 5,                  # Número de temas/tópicos a descubrir (hiperparámetro)
                method = "Gibbs",       # Método de muestreo de Gibbs para inferencia
                                        # (alternativa: "VEM" = Variational EM)
                control = list(
                  seed = 1111           # Semilla de probabilidad para reproducibilidad
                  ))

# ============================================================
# EXTRACCIÓN DE MATRICES BETA Y GAMMA
# ============================================================
# MATRIZ BETA (β): Distribución de palabras por tema
# Betas:
# Matríz que contiene la probabilidad de cada palabra dado un tema dado
# En notación probabilística: P(palabra|tema)
# Pregunta que responde: "¿Qué tan probable es que aparezca la palabra W en el tema Z?"
betas_juve <- tidy(lda_juve, matrix = "beta")

# MATRIZ GAMMA (γ): Distribución de temas por documento
# Gamas:
# Matríz que contiene la probabilidad de cada tema, dado un documento dado
# En notación probabilística: P(tema|documento)
# Pregunta que responde: "¿Qué proporción del documento D pertenece al tema Z?"
gammas_juve <- tidy(lda_juve, matrix = "gamma")


# ============================================================
# ANÁLISIS Y VISUALIZACIÓN DE LA MATRIZ BETA
# ============================================================
# Extraemos la matríz beta:
# La matríz Beta es la matríz que nos dá la probabilidad de ocurrencia de w dado Z.
# P(w|z)
betas_juve <- tidy(lda_juve, matrix = "beta")
# Ejemplo: visualizar las 50 palabras más probables del tema 2
betas_juve %>%
  filter(topic == 2) %>%      # Filtrar solo el tema 2
  arrange(-beta) %>%           # Ordenar por beta descendente (más probable primero)
  print(n = 50)

# VISUALIZACIÓN: Top 10 palabras por tema
# 10 principales palabras por tema:
bd_plot <- betas_juve %>%
  group_by(topic) %>%          # Agrupar por tema
  slice_max(beta, n = 10)      # Seleccionar las 10 palabras con mayor beta por tema

# Crear gráfico de barras con facetas (un panel por tema)
bd_plot %>%
  # reorder_within() permite ordenar términos independientemente dentro de cada faceta
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +                            # Barras sin leyenda
  facet_wrap(~str_c("Tema: ", topic), scales = "free") +     # Panel por tema, escalas independientes
  coord_flip() +                                             # Voltear para legibilidad
  scale_x_reordered() +                                      # Escala correcta para reorder_within
  labs(title = "Palabras con mayor probabilidad de aparecer en cada uno de los temas")

# ============================================================
# ANÁLISIS Y VISUALIZACIÓN DE LA MATRIZ GAMMA
# ============================================================
# Extraemos la matríz gamma:
# La matríz gamma nos dice la probabilidad de distribución de cada tema, z para cada documento, d

# Paso 1: Crear catálogo de artículos
# Generamos un catálogo de artículos, o sea, una tabla que tenga el id y el título de los artículos
catalogo_articulos <- articulos_juve2 %>%
  select(id, titulo = titulos) %>%
  unique()                               # Eliminar duplicados si existen

# Paso 2: Etiquetar manualmente los temas descubiertos
# Generamos un catálogo de temas, en función de las palabras escogidas por el modelo
catalogo_temas <- tibble(topic = 1:5,
# ESTOS TEMAS FUERON DEFINIDOS A CRITERIO MÍO, VIENDO LAS PALABRAS DE LA GRÁFICA PREVIA
# IMPORTANTE: El analista debe interpretar los temas basándose en las palabras de mayor beta
                         tema = c("Artículos de Elecciones",
                                  "Artículos de IA",
                                  "Estadísticas locales de Coahuila",
                                  "Artículos de Economía, finanzas y cuentas nacionales",
                                  "Estadísticas nacionales y laborales"))

# Paso 3: Enriquecer la matriz gamma con metadatos
# Matríz de gammas: La proporción o probabilidad de cada tema en cada artículo:
gammas_juve <- tidy(lda_juve, matrix = "gamma") %>%
  mutate(document = as.numeric(document)) %>%        # Convertir document a numérico
  arrange(document, -gamma) %>%                      # Ordenar por documento y gamma descendente
  left_join(catalogo_articulos, by = c("document" = "id")) %>%  # Agregar títulos
  left_join(catalogo_temas, by = c())                # Nota: Falta especificar "topic" en by

gammas_juve

# Etiquetemos temas.
betas_juve

# CLASIFICACIÓN DE ARTÍCULOS POR TEMA DOMINANTE
# Clasifiquemos artículos:
# Asignar cada documento al tema con mayor probabilidad (gamma más alto)
lista_articulos_por_tema <- gammas_juve %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%          # Seleccionar el tema con mayor gamma por documento
  left_join(catalogo_temas)

# Verifiquen como si tiene sentido la clasificación
# (Es importante validar manualmente si la asignación de temas tiene sentido)


# VISUALIZACIÓN: Composición de temas en documentos
# Hacemos la gráfica de proporción de temas:
gammas_juve %>%
  filter(document <= 10) %>%           # Visualizar solo los primeros 10 documentos
  ggplot(aes(x = reorder(titulo %>% str_wrap(10), gamma),  # str_wrap corta el texto para legibilidad
             y = gamma,
             fill = tema)) +
  # Gráfico de barras apiladas mostrando la proporción de cada tema por documento
  geom_col(color = "white", position = position_stack(reverse = TRUE)) +
  theme(legend.position = "bottom")


# ============================================================
# DETERMINACIÓN DEL NÚMERO ÓPTIMO DE TÓPICOS CON PERPLEJIDAD
# ============================================================
# ====== DETERMINACIÓN DEL NÚMERO ÓPTIMO DE TÓPICOS ======
# El número k de tópicos es un HIPERPARÁMETRO que debemos optimizar
# ¿Cómo saber cuántos temas son los adecuados? Usamos PERPLEJIDAD

# ¿QUÉ ES LA PERPLEJIDAD?
# Métrica que evalúa qué tan bien el modelo predice los datos
# - Perplejidad BAJA = el modelo predice bien los datos (mejor ajuste)
# - Perplejidad ALTA = el modelo no captura bien los patrones
# Buscamos el valor de k que minimiza la perplejidad

# Definir rango de tópicos a evaluar
# En este caso, se va a correr el modelo desde dos hasta 25 tópicos:
k_values <- seq(2, 25, by = 1)  # Evaluar de 2 a 25 tópicos (el comentario original decía 15 pero es 25)

# GRID SEARCH: Calcular perplejidad para cada valor de k
# Este bucle ejecuta el LDA desde 2 a 25 temas
# ADVERTENCIA: Este proceso es computacionalmente intensivo y puede tardar varios minutos
perplexity_values <- sapply(k_values, function(k) {
  print("Calculando modelo con k =", k, "\n")

  # Ajustar modelo LDA con parámetros más robustos
  lda_model <- LDA(dtm_juve,
                   k = k,
                   method = "Gibbs",         # Muestreo de Gibbs
                   control = list(seed = 1111,
                                  iter = 2000,   # Número de iteraciones (más = mejor convergencia)
                                  thin = 100,    # Thinning: guardar cada 100 iteraciones
                                  burnin = 1000)) # Burn-in: descartar primeras 1000 iteraciones

  # Calcular perplejidad del modelo ajustado
  # CORRECCIÓN: especificar newdata
  perplexity(lda_model, newdata = dtm_juve)
})

# Almacenar resultados en un dataframe
# Crear data frame con resultados
perplexity_results <- tibble(
  k = k_values,                   # Número de tópicos evaluados
  perplexity = perplexity_values  # Perplejidad correspondiente a cada k
)

# ============================================================
# VISUALIZACIÓN DE RESULTADOS DE PERPLEJIDAD
# ============================================================
# Gráfico de perplejidad vs número de tópicos
# Este gráfico ayuda a identificar el "codo" o punto óptimo
perplexity_plot <- ggplot(perplexity_results, aes(x = k, y = perplexity)) +
  geom_line(color = "blue", size = 1) +         # Línea conectando los puntos
  geom_point(color = "red", size = 3) +         # Puntos para cada k evaluado
  scale_x_continuous(breaks = k_values) +       # Mostrar todos los valores de k en el eje X
  labs(title = "Perplejidad vs Número de Tópicos",
       subtitle = "Valores más bajos indican mejor ajuste",
       x = "Número de Tópicos (k)",
       y = "Perplejidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(perplexity_plot)

# Mostrar tabla de resultados
print(perplexity_results)

# ============================================================
# IDENTIFICACIÓN DEL NÚMERO ÓPTIMO DE TÓPICOS
# ============================================================
# Identificar el k con menor perplejidad
optimal_k <- perplexity_results %>%
  filter(perplexity == min(perplexity)) %>%   # Filtrar el mínimo
  pull(k)                                      # Extraer el valor de k

# Mostrar resultados:
cat("Número óptimo de tópicos basado en perplejidad:", optimal_k, "\n")
cat("Perplejidad mínima:", min(perplexity_values), "\n")

# ============================================================
# AJUSTAR MODELO LDA CON EL NÚMERO ÓPTIMO DE TÓPICOS
# ============================================================
# De acuerdo a este método de optimización, así se vería el resultado:
# Entrenar el modelo LDA con el k óptimo identificado
lda_model_optimo <- LDA(dtm_juve,
                 k = optimal_k,              # Usar el k óptimo encontrado
                 method = "Gibbs",
                 control = list(seed = 1111,
                                iter = 2000,  # Número de iteraciones
                                thin = 100,   # Thinning
                                burnin = 1000)) # Burn-in

# Extraer matriz beta del modelo óptimo
betas_optimo <- tidy(lda_model_optimo, matrix = "beta")

# ============================================================
# VISUALIZACIÓN DEL MODELO ÓPTIMO
# ============================================================
# Versión 1: Gráfico básico (sin ordenar términos correctamente)
# NOTA: Esta versión tiene un problema - los términos no se ordenan correctamente
# dentro de cada faceta porque el orden es global, no por tema
betas_optimo %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  print(n = 20) %>%
  ggplot(aes(x = term, y = beta)) +
  geom_col() +
  facet_wrap(~topic, scale = "free") +     # ADVERTENCIA: debería ser "scales" (plural)
  coord_flip()

# Versión 2: Gráfico mejorado (con ordenamiento correcto dentro de cada faceta)
# Esta es la versión CORRECTA - usa reorder_within() para ordenar términos
# independientemente en cada panel
betas_optimo %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%  # Ordenar dentro de cada tema
  print(n = 20) %>%
  ggplot(aes(x = term, y = beta)) +
  geom_col() +
  facet_wrap(~topic, scales = "free") +                 # scales = "free" permite ejes independientes
  coord_flip() +
  scale_x_reordered()                                   # Escala necesaria para reorder_within()

# ============================================================
# CONSIDERACIONES FINALES
# ============================================================
# Con el número óptimo, se aprecian más concretamente ciertos temas, aunque se vuelven
# demasiados temas para manejarlos de forma óptima y adecuada ya en la realidad.
#
# BALANCE ENTRE PERPLEJIDAD Y PRACTICIDAD:
# - Un k muy alto puede dar mejor perplejidad pero temas difíciles de interpretar
# - Un k muy bajo puede ser más interpretable pero perder matices importantes
#
# Ya dependerá del analista ver si así se queda, o si mejor le reducimos algunos tópicos
# al análisis resultante. La decisión final debe considerar:
# 1. Perplejidad (métrica cuantitativa)
# 2. Interpretabilidad de los temas (juicio cualitativo)
# 3. Necesidades del proyecto/negocio 