# Texto de prueba 
# txt <- "bienvenidos esto es fuera de la caja yo soy ... y le agradezco mucho que me escuche en esta revisión de la semana que termina el 29 de septiembre del 2024 yo estoy grabando para usted el día 30 de septiembre último día de Andrés Manuel López Obrador como presidente de la república Y usted espero me estará escuchando a partir ya de el gobierno de Claudia la primera presidenta en México una semana me parece que puede servirnos muy claramente para ejemplificar lo ocurrido durante este este gobierno tres ejemplos que me parecen muy importantes Acapulco Sinaloa y Chiapas Déjenme empezar por el primero Acapulco sufrieron huracán extraordinariamente fuerte otis el año pasado un huracán que se aceleró muy rápidamente incluso por encima de lo que estaban estimando los expertos Y bueno pues eh uno puede evaluar al la actuación del gobierno con cierta calma y decir bueno no podían saber que iba a entrar como huracán categoría 5 esperaban a la mejor que llegara a tres a una así no avisaron con tiempo eh Pero bueno pues la destrucción no podía evitarse eso no hay manera de hacerlo eh lo que no hubo fue una reacción de parte del "

# Librerías
library(tidyverse)
library(wordcloud2)
library(tm)

create_wordcloud <- function(data, 
                             stop_words = c(), 
                             num_words = 100, 
                             background = "white",
                             mask = NULL,
                             tamanio = 0.5,
                             paleta_colores = c("#6950d8", "#3CEAFA", "#00b783", 
                                                "#ff6260", "#ffaf84", "#ffbd41", 
                                                "#0A93C4", "#0Acfce", "#E8B32E", 
                                                "#E8D92E"),
                             tipo_color = "aleatorio",
                             forma = "circle",
                             elipticidad = 0.65,
                             ancho_grid = NULL,
                             alto_grid = NULL,
                             imagen_mascara = NULL) {
  
  # Pre-Función para eliminar símbolos raros
  quitar_signos <- function(x) stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))
  
  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras 
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minúsculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuación (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los números
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiración e interrogación al revés
    corpus <- tm_map(corpus, quitar_signos)    
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuáles, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el número de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra 
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  freq_palabras <<- data
  
  # Make sure a proper num_words is provided
  # Nos aseguramos que un número adecuado de palabras `num_provider` es generado
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un número `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0 | length(data) == 0) {
    return(NULL)
  }
  
  # Definir los colores según el tipo seleccionado
  if (tipo_color == "degradado") {
    # Crear un degradado basado en la frecuencia de las palabras
    # Normalizamos las frecuencias entre 0 y 1
    freq_norm <- (data$freq - min(data$freq)) / (max(data$freq) - min(data$freq))
    
    # Si solo hay una palabra, evitamos división por cero
    if (length(freq_norm) == 1) {
      freq_norm <- 1
    }
    
    # Crear función de interpolación de colores
    # Usamos colorRampPalette para crear un degradado suave entre los colores de la paleta
    color_ramp <- colorRampPalette(paleta_colores)
    colores_asignados <- color_ramp(nrow(data))
    
  } else if (tipo_color == "aleatorio") {
    # Colores aleatorios repitiendo la paleta si es necesario
    set.seed(90)  # Para reproducibilidad
    colores_asignados <- rep(paleta_colores, length.out = nrow(data))
    # Opcional: mezclar los colores aleatoriamente
    # colores_asignados <- sample(colores_asignados)
    
  } else {
    # Si se especifica un tipo no válido, usar aleatorio por defecto
    warning("Tipo de color no válido. Usando 'aleatorio' por defecto.")
    set.seed(90)
    colores_asignados <- rep(paleta_colores, length.out = nrow(data))
  }
  
  # Configurar parámetros base para wordcloud2
  params <- list(
    data = data,
    backgroundColor = background,
    color = colores_asignados,
    fontFamily = "Arial",
    size = tamanio
  )
  
  # Agregar configuración de forma según el tipo especificado
  if (!is.null(imagen_mascara)) {
    # Si se proporciona una imagen como máscara, usarla
    params$figPath <- imagen_mascara
    
  } else if (forma == "custom" && !is.null(mask)) {
    # Si se proporciona una máscara personalizada
    params$figPath <- mask
    
  } else {
    # Usar formas predefinidas
    forma <- tolower(forma)
    
    # Configurar parámetros según la forma seleccionada
    if (forma == "square" || forma == "cuadrado") {
      params$shape <- "square"
      params$ellipticity <- 0  # Hace el cuadrado más definido
      
    } else if (forma == "rectangle" || forma == "rectangulo") {
      params$shape <- "rectangle"
      params$ellipticity <- 0.3
      if (!is.null(ancho_grid)) params$gridSize <- ancho_grid
      
    } else if (forma == "triangle" || forma == "triangulo") {
      params$shape <- "triangle"
      params$ellipticity <- elipticidad
      
    } else if (forma == "diamond" || forma == "diamante" || forma == "rombo") {
      params$shape <- "diamond"
      params$ellipticity <- elipticidad
      
    } else if (forma == "star" || forma == "estrella") {
      params$shape <- "star"
      params$ellipticity <- elipticidad
      
    } else if (forma == "pentagon" || forma == "pentagono") {
      params$shape <- "pentagon"
      params$ellipticity <- elipticidad
      
    } else if (forma == "cardioid" || forma == "corazon" || forma == "heart") {
      params$shape <- "cardioid"
      params$ellipticity <- 0.65
      
    } else {
      # Por defecto usar círculo
      params$shape <- "circle"
      params$ellipticity <- elipticidad
    }
  }
  
  # Ajustar grid size si se especifica
  if (!is.null(ancho_grid)) {
    params$gridSize <- ancho_grid
  }
  
  # Crear el wordcloud
  do.call(wordcloud2, params)
}

# ==========================================
# FUNCIÓN AUXILIAR: Crear letra como forma
# ==========================================

create_letter_wordcloud <- function(data,
                                    letra = "R",
                                    stop_words = c(),
                                    num_words = 100,
                                    background = "white",
                                    tamanio = 0.5,
                                    paleta_colores = c("#6950d8", "#3CEAFA", "#00b783", 
                                                       "#ff6260", "#ffaf84", "#ffbd41"),
                                    tipo_color = "aleatorio") {
  
  # Usar letterCloud específicamente para letras
  # Pre-procesamiento del texto (si es necesario)
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Limitar palabras
  data <- head(data, n = num_words)
  
  # Configurar colores
  if (tipo_color == "degradado") {
    freq_norm <- (data$freq - min(data$freq)) / (max(data$freq) - min(data$freq))
    if (length(freq_norm) == 1) freq_norm <- 1
    color_ramp <- colorRampPalette(paleta_colores)
    colores_asignados <- color_ramp(nrow(data))
  } else {
    set.seed(90)
    colores_asignados <- rep(paleta_colores, length.out = nrow(data))
  }
  
  # Crear wordcloud en forma de letra
  letterCloud(data, 
              word = letra,
              backgroundColor = background,
              color = colores_asignados,
              fontFamily = "Arial",
              size = tamanio)
}

# ==========================================
# EJEMPLOS DE USO
# ==========================================

# Texto de ejemplo
txt_ejemplo <- "El análisis de datos es fundamental en R. La programación en R 
                permite crear visualizaciones increíbles. Los datos pueden 
                transformarse mediante programación. R es excelente para estadística 
                y machine learning. El análisis estadístico es poderoso en R.
                Las visualizaciones de datos son hermosas. La ciencia de datos
                utiliza R extensivamente. Los gráficos y visualizaciones son
                fundamentales para comunicar resultados."

# 1. Wordcloud CUADRADO
# wc_cuadrado <- create_wordcloud(data = txt_ejemplo, 
#                                  stop_words = c("mediante"), 
#                                  num_words = 50, 
#                                  background = "white", 
#                                  tamanio = 0.5,
#                                  forma = "square",
#                                  tipo_color = "degradado")

# 2. Wordcloud TRIANGULAR
# wc_triangulo <- create_wordcloud(data = txt_ejemplo, 
#                                   stop_words = c("mediante"), 
#                                   num_words = 50, 
#                                   background = "black", 
#                                   tamanio = 0.5,
#                                   forma = "triangle",
#                                   tipo_color = "aleatorio")

# 3. Wordcloud DIAMANTE
# wc_diamante <- create_wordcloud(data = txt_ejemplo, 
#                                  num_words = 50, 
#                                  forma = "diamond",
#                                  tipo_color = "degradado",
#                                  paleta_colores = c("#FF6B6B", "#4ECDC4", 
#                                                    "#45B7D1", "#96CEB4"))

# 4. Wordcloud ESTRELLA
# wc_estrella <- create_wordcloud(data = txt_ejemplo,
#                                  num_words = 50,
#                                  forma = "star",
#                                  background = "#f0f0f0",
#                                  elipticidad = 0.7)

# 5. Wordcloud CORAZÓN
# wc_corazon <- create_wordcloud(data = txt_ejemplo,
#                                 num_words = 50,
#                                 forma = "heart",
#                                 paleta_colores = c("#FF69B4", "#FF1493", 
#                                                   "#C71585", "#DB7093"))

# 6. Wordcloud con LETRA
# wc_letra_R <- create_letter_wordcloud(data = txt_ejemplo,
#                                        letra = "R",
#                                        num_words = 50,
#                                        tipo_color = "degradado")

# ==========================================
# FUNCIÓN ADICIONAL: Vista previa de formas
# ==========================================

preview_wordcloud_shapes <- function(texto, num_words = 30) {
  
  formas <- c("circle", "square", "triangle", "diamond", "star", "pentagon")
  
  # Crear una lista para almacenar los wordclouds
  wordclouds <- list()
  
  for (forma in formas) {
    wc <- create_wordcloud(data = texto,
                           num_words = num_words,
                           forma = forma,
                           tamanio = 0.3,
                           tipo_color = "aleatorio")
    wordclouds[[forma]] <- wc
    print(paste("Generando forma:", forma))
    print(wc)
    Sys.sleep(2)  # Pausa entre visualizaciones
  }
  
  return(invisible(wordclouds))
}

# Ejemplo de uso:
# preview_wordcloud_shapes(txt_ejemplo, num_words = 40)

# ==========================================
# NOTAS DE USO
# ==========================================

# FORMAS DISPONIBLES:
# - "circle" o "circulo": Forma circular (por defecto)
# - "square" o "cuadrado": Forma cuadrada
# - "rectangle" o "rectangulo": Forma rectangular
# - "triangle" o "triangulo": Forma triangular
# - "diamond" o "diamante" o "rombo": Forma de diamante
# - "star" o "estrella": Forma de estrella
# - "pentagon" o "pentagono": Forma pentagonal
# - "cardioid" o "corazon" o "heart": Forma de corazón

# PARÁMETROS ADICIONALES:
# - elipticidad: Controla qué tan "redondeada" es la forma (0 = angular, 1 = redondeada)
# - ancho_grid: Controla el espaciado entre palabras
# - imagen_mascara: Path a una imagen para usar como máscara personalizada

# USO CON IMAGEN PERSONALIZADA:
# Para usar tu propia forma (logo, silueta, etc.):
# wc_custom <- create_wordcloud(data = txt_ejemplo,
#                               imagen_mascara = "path/to/your/image.png",
#                               num_words = 100)

# TIPS:
# 1. Para formas más definidas, usa menos palabras (30-50)
# 2. El parámetro 'elipticidad' afecta más a triangle, star y pentagon
# 3. Para cuadrados perfectos, usa elipticidad = 0
# 4. Los colores degradados funcionan mejor con formas simétricas
# 5. Ajusta 'tamanio' según el número de palabras y la forma elegida
