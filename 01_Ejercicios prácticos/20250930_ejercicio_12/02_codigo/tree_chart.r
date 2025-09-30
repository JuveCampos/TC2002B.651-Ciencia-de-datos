
# ==========================================
# WORD TREE CHARTS EN R
# ==========================================

# Librerías necesarias
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(googleVis)
library(htmlwidgets)
library(networkD3)
library(visNetwork)

# ==========================================
# OPCIÓN 1: GOOGLE VIS (Word Tree Clásico)
# ==========================================

create_wordtree_googlevis <- function(texto, 
                                      palabra_raiz = NULL,
                                      max_palabras = 100) {
  
  # Preparar el texto para el formato que requiere googleVis
  # Dividir en oraciones
  sentences <- unlist(strsplit(texto, "[.!?]+"))
  sentences <- trimws(sentences)
  sentences <- sentences[sentences != ""]
  
  # Si se especifica una palabra raíz, filtrar oraciones que la contengan
  if (!is.null(palabra_raiz)) {
    sentences <- sentences[grepl(palabra_raiz, sentences, ignore.case = TRUE)]
    if (length(sentences) == 0) {
      stop(paste("No se encontraron oraciones con la palabra:", palabra_raiz))
    }
  }
  
  # Crear data frame con formato para googleVis
  df_tree <- data.frame(Phrases = sentences, stringsAsFactors = FALSE)
  
  # Crear el word tree
  tree <- gvisWordTree(df_tree, 
                       textvar = "Phrases",
                       options = list(
                         wordtree = paste0("{word: '", palabra_raiz, "',
                                          format: 'implicit',
                                          type: 'double'}"),
                         height = 600,
                         width = 1000,
                         fontName = 'Arial',
                         maxFontSize = 20,
                         colors = "['#6950d8', '#3CEAFA', '#00b783']"
                       ))
  
  return(tree)
}

# ==========================================
# OPCIÓN 2: NETWORK GRAPH (igraph + ggraph)
# ==========================================

create_wordtree_network <- function(texto, 
                                    palabra_central,
                                    ventana_contexto = 5,
                                    min_frecuencia = 2,
                                    max_palabras = 30) {
  
  # Limpiar texto
  texto_limpio <- texto %>%
    tolower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
  
  # Tokenizar
  palabras <- unlist(str_split(texto_limpio, "\\s+"))
  palabras <- palabras[palabras != ""]
  
  # Encontrar índices de la palabra central
  indices <- which(palabras == tolower(palabra_central))
  
  if (length(indices) == 0) {
    stop(paste("No se encontró la palabra:", palabra_central))
  }
  
  # Extraer contexto alrededor de cada ocurrencia
  conexiones <- data.frame(from = character(), to = character(), 
                           weight = numeric(), stringsAsFactors = FALSE)
  
  for (idx in indices) {
    # Palabras antes
    start_before <- max(1, idx - ventana_contexto)
    end_before <- idx - 1
    
    if (end_before >= start_before) {
      palabras_antes <- palabras[start_before:end_before]
      for (p in palabras_antes) {
        if (p != "" && !p %in% stopwords("spanish")) {
          conexiones <- rbind(conexiones, 
                              data.frame(from = p, to = palabra_central, 
                                         weight = 1, stringsAsFactors = FALSE))
        }
      }
    }
    
    # Palabras después
    start_after <- idx + 1
    end_after <- min(length(palabras), idx + ventana_contexto)
    
    if (start_after <= end_after) {
      palabras_despues <- palabras[start_after:end_after]
      for (p in palabras_despues) {
        if (p != "" && !p %in% stopwords("spanish")) {
          conexiones <- rbind(conexiones, 
                              data.frame(from = palabra_central, to = p, 
                                         weight = 1, stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  # Agregar pesos
  conexiones <- conexiones %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight), .groups = 'drop') %>%
    filter(weight >= min_frecuencia) %>%
    arrange(desc(weight)) %>%
    head(max_palabras)
  
  # Crear grafo
  g <- graph_from_data_frame(conexiones, directed = TRUE)
  
  # Configurar atributos visuales
  V(g)$size <- degree(g, mode = "all")
  V(g)$color <- ifelse(V(g)$name == palabra_central, "#ff6260", "#3CEAFA")
  E(g)$width <- E(g)$weight
  
  # Crear visualización con ggraph
  p <- ggraph(g, layout = 'fr') +
    geom_edge_link(aes(width = weight, alpha = weight),
                   arrow = arrow(length = unit(2, 'mm')),
                   end_cap = circle(3, 'mm'),
                   color = "#6950d8") +
    geom_node_point(aes(size = size * 3, color = color)) +
    geom_node_text(aes(label = name), 
                   repel = TRUE, 
                   size = 4,
                   fontface = "bold") +
    scale_color_identity() +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_alpha(range = c(0.3, 0.8)) +
    labs(title = paste("Árbol de Palabras:", palabra_central),
         subtitle = paste("Ventana de contexto:", ventana_contexto, "palabras")) +
    theme_graph() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12))
  
  return(p)
}

# ==========================================
# OPCIÓN 3: ÁRBOL JERÁRQUICO (Dendrogram style)
# ==========================================

create_wordtree_hierarchical <- function(texto, 
                                         palabra_raiz,
                                         profundidad = 3,
                                         min_freq = 2) {
  
  # Limpiar y tokenizar
  texto_limpio <- texto %>%
    tolower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
  
  palabras <- unlist(str_split(texto_limpio, "\\s+"))
  palabras <- palabras[palabras != ""]
  
  # Encontrar secuencias que contienen la palabra raíz
  indices <- which(palabras == tolower(palabra_raiz))
  
  if (length(indices) == 0) {
    stop(paste("No se encontró la palabra:", palabra_raiz))
  }
  
  # Construir secuencias
  secuencias <- list()
  
  for (idx in indices) {
    # Secuencias hacia adelante
    for (d in 1:profundidad) {
      if (idx + d <= length(palabras)) {
        seq_forward <- paste(palabras[idx:(idx + d)], collapse = " ")
        secuencias <- append(secuencias, seq_forward)
      }
    }
    
    # Secuencias hacia atrás
    for (d in 1:profundidad) {
      if (idx - d >= 1) {
        seq_backward <- paste(palabras[(idx - d):idx], collapse = " ")
        secuencias <- append(secuencias, seq_backward)
      }
    }
  }
  
  # Contar frecuencias
  freq_table <- table(unlist(secuencias))
  freq_df <- data.frame(
    secuencia = names(freq_table),
    frecuencia = as.numeric(freq_table),
    stringsAsFactors = FALSE
  ) %>%
    filter(frecuencia >= min_freq) %>%
    arrange(desc(frecuencia))
  
  # Crear estructura jerárquica para visualización
  edges <- data.frame(from = character(), to = character(), 
                      value = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(freq_df)) {
    words <- unlist(str_split(freq_df$secuencia[i], " "))
    if (palabra_raiz %in% words) {
      pos <- which(words == palabra_raiz)[1]
      
      # Conectar palabra raíz con palabra siguiente
      if (pos < length(words)) {
        edges <- rbind(edges, 
                       data.frame(from = palabra_raiz, 
                                  to = words[pos + 1],
                                  value = freq_df$frecuencia[i]))
      }
      
      # Conectar palabra anterior con palabra raíz
      if (pos > 1) {
        edges <- rbind(edges, 
                       data.frame(from = words[pos - 1], 
                                  to = palabra_raiz,
                                  value = freq_df$frecuencia[i]))
      }
    }
  }
  
  # Agregar valores
  edges <- edges %>%
    group_by(from, to) %>%
    summarise(value = sum(value), .groups = 'drop')
  
  # Crear nodos únicos
  all_nodes <- unique(c(edges$from, edges$to))
  nodes <- data.frame(
    id = all_nodes,
    label = all_nodes,
    size = sapply(all_nodes, function(x) {
      sum(edges$value[edges$from == x | edges$to == x])
    }),
    color = ifelse(all_nodes == palabra_raiz, "#ff6260", "#3CEAFA"),
    stringsAsFactors = FALSE
  )
  
  # Normalizar tamaños
  nodes$size <- 10 + (nodes$size / max(nodes$size)) * 30
  
  # Crear visualización interactiva con visNetwork
  visNetwork(nodes, edges) %>%
    visNodes(shape = "dot",
             font = list(size = 16, face = "bold")) %>%
    visEdges(arrows = "to",
             smooth = list(enabled = TRUE, type = "curvedCW")) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(stabilization = TRUE,
               barnesHut = list(gravitationalConstant = -8000,
                                springConstant = 0.001,
                                springLength = 200)) %>%
    visOptions(highlightNearest = list(enabled = TRUE, 
                                       degree = 1,
                                       hover = TRUE),
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE,
                   dragNodes = TRUE,
                   zoomView = TRUE)
}

# ==========================================
# OPCIÓN 4: ÁRBOL RADIAL INTERACTIVO (networkD3)
# ==========================================

create_wordtree_radial <- function(texto, 
                                   palabra_central,
                                   max_niveles = 2,
                                   min_conexiones = 2) {
  
  # Preparar texto
  texto_limpio <- texto %>%
    tolower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
  
  # Crear bigramas y trigramas
  palabras_df <- data.frame(texto = texto_limpio, stringsAsFactors = FALSE)
  
  # Obtener bigramas
  bigramas <- palabras_df %>%
    unnest_tokens(bigram, texto, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 == palabra_central | word2 == palabra_central) %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n >= min_conexiones)
  
  if (nrow(bigramas) == 0) {
    stop(paste("No se encontraron suficientes conexiones para:", palabra_central))
  }
  
  # Preparar datos para networkD3
  # Crear lista de nodos únicos
  all_words <- unique(c(bigramas$word1, bigramas$word2))
  nodes <- data.frame(name = all_words, 
                      group = ifelse(all_words == palabra_central, 1, 2),
                      size = sapply(all_words, function(x) {
                        sum(bigramas$n[bigramas$word1 == x | bigramas$word2 == x])
                      }))
  
  # Crear enlaces con índices
  links <- bigramas %>%
    mutate(source = match(word1, nodes$name) - 1,
           target = match(word2, nodes$name) - 1,
           value = n)
  
  # Crear gráfico de red interactivo
  forceNetwork(Links = links, 
               Nodes = nodes,
               Source = "source", 
               Target = "target",
               Value = "value", 
               NodeID = "name",
               Group = "group",
               Nodesize = "size",
               radiusCalculation = "Math.sqrt(d.nodesize) * 3",
               charge = -120,
               fontSize = 14,
               fontFamily = "Arial",
               linkDistance = 50,
               opacity = 0.9,
               zoom = TRUE,
               legend = FALSE,
               colourScale = JS("d3.scaleOrdinal()
                               .domain([1, 2])
                               .range(['#ff6260', '#3CEAFA'])"))
}

# ==========================================
# FUNCIÓN WRAPPER PRINCIPAL
# ==========================================

create_word_tree <- function(texto,
                             palabra_focal,
                             tipo = "network",
                             ...) {
  
  tipo <- tolower(tipo)
  
  result <- switch(tipo,
                   "googlevis" = create_wordtree_googlevis(texto, palabra_focal, ...),
                   "network" = create_wordtree_network(texto, palabra_focal, ...),
                   "hierarchical" = create_wordtree_hierarchical(texto, palabra_focal, ...),
                   "radial" = create_wordtree_radial(texto, palabra_focal, ...),
                   stop("Tipo no válido. Use: 'googlevis', 'network', 'hierarchical', o 'radial'")
  )
  
  return(result)
}

# ==========================================
# EJEMPLOS DE USO
# ==========================================

# Texto de ejemplo
texto_ejemplo <- "El gobierno presentó nuevas medidas económicas. 
                  El gobierno anunció cambios importantes. 
                  Las medidas del gobierno buscan mejorar la economía.
                  El presidente del gobierno explicó las medidas.
                  Las nuevas medidas económicas del gobierno entrarán en vigor.
                  El gobierno implementará medidas fiscales y medidas sociales.
                  Las medidas fiscales del gobierno son controvertidas."

# 1. Network Graph (ggraph)
# grafico_red <- create_word_tree(texto_ejemplo,
#                                 palabra_focal = "gobierno",
#                                 tipo = "network",
#                                 ventana_contexto = 3,
#                                 min_frecuencia = 1)
# print(grafico_red)

# 2. Árbol Jerárquico Interactivo (visNetwork)
# arbol_interactivo <- create_word_tree(texto_ejemplo,
#                                       palabra_focal = "gobierno",
#                                       tipo = "hierarchical",
#                                       profundidad = 2,
#                                       min_freq = 1)
# arbol_interactivo

# 3. Red Radial (networkD3)
# red_radial <- create_word_tree(texto_ejemplo,
#                                palabra_focal = "gobierno",
#                                tipo = "radial",
#                                min_conexiones = 1)
# red_radial

# 4. Google Vis Word Tree
# tree_gvis <- create_word_tree(texto_ejemplo,
#                               palabra_focal = "gobierno",
#                               tipo = "googlevis")
plot(tree_gvis)

# ==========================================
# FUNCIÓN ADICIONAL: ANÁLISIS DE CO-OCURRENCIAS
# ==========================================

analyze_word_connections <- function(texto, 
                                     palabra_objetivo,
                                     top_n = 10) {
  
  # Limpiar texto
  texto_limpio <- texto %>%
    tolower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
  
  # Tokenizar
  tokens_df <- data.frame(texto = texto_limpio) %>%
    unnest_tokens(word, texto) %>%
    filter(!word %in% stopwords("spanish"))
  
  # Encontrar co-ocurrencias
  word_pairs <- tokens_df %>%
    mutate(section = row_number() %/% 10) %>%  # Dividir en secciones
    filter(word == palabra_objetivo | lead(word) == palabra_objetivo | 
             lag(word) == palabra_objetivo) %>%
    group_by(section) %>%
    summarise(words = list(word), .groups = 'drop')
  
  # Contar frecuencias
  coocurrencias <- unlist(word_pairs$words)
  coocurrencias <- coocurrencias[coocurrencias != palabra_objetivo]
  
  freq_table <- table(coocurrencias)
  resultado <- data.frame(
    palabra = names(freq_table),
    frecuencia = as.numeric(freq_table),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(frecuencia)) %>%
    head(top_n)
  
  # Crear gráfico de barras
  p <- ggplot(resultado, aes(x = reorder(palabra, frecuencia), 
                             y = frecuencia)) +
    geom_col(fill = "#6950d8", alpha = 0.8) +
    coord_flip() +
    labs(title = paste("Palabras más conectadas con:", palabra_objetivo),
         x = "Palabras",
         y = "Frecuencia de co-ocurrencia") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  return(list(datos = resultado, grafico = p))
}

# Ejemplo de uso:
# analisis <- analyze_word_connections(texto_ejemplo, "gobierno", top_n = 5)
# print(analisis$datos)
# print(analisis$grafico)
