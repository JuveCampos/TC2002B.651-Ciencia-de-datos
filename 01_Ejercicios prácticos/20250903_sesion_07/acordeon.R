
# Librerías ----
library(class)
library(caret)
library(ggplot2)
library(tidyverse)

# Cargamos datos de vinos ---
vino <- readxl::read_xlsx("datos/datos_vino.xlsx")
# Calidad es la etiqueta Y
# acidez a alcohol es la matríz X

# Analizamos los datos: 
summary(vino) # Función para sacar estadísticas descriptivas
table(vino$calidad) # Hay 100 de cada tipo

# Analizamos las variables para ver si hay outliers
vino %>% 
  pivot_longer(cols = acidez:ph) %>% 
  ggplot(aes(x = name, y = value, color = name)) + 
  geom_boxplot() + 
  theme(legend.position = "bottom")

# Obtenemos la matríz de correlación 
vino_2 <- vino %>% select(-calidad)
correlacion <- cor(vino_2)
# reshape::melt(correlacion)
rownames(correlacion) 
corr_tidy <- correlacion %>% 
  as_tibble() %>% 
  mutate(renglon = rownames(correlacion)) %>% 
  pivot_longer(cols = acidez:ph)

corr_tidy %>% 
  ggplot(aes(x = renglon, y = name, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value,2))) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1,1))

# Dividimos en dos bases, entrenamiento y validación 
# Función 
# caret::createDataPartition()
?createDataPartition()
set.seed(1234)

no_datos <- nrow(vino)
no_datos_entrenamiento <- no_datos*0.7
no_datos_validacion <- no_datos*0.3

indices_entrenamiento <- sample(1:300, replace = F)[1:no_datos_entrenamiento] %>% sort()
indices_validacion <- (1:300)[!(1:300 %in% indices_entrenamiento)]

X <- vino_2
Y <- vino %>% pull(calidad)

# Creamos cnjuntos de entrenamiento ----
X_entrena <- X[indices_entrenamiento,]
X_prueba <- X[indices_validacion,]
Y_entrena <- Y[indices_entrenamiento]
Y_prueba <- Y[indices_validacion]

# Estandarizamos las características 
medias <- colMeans(X_entrena)
desviaciones <- apply(X_entrena, 2, sd)

X_entrena_norm <- scale(X_entrena, center = medias, scale = desviaciones)
X_prueba_norm <- scale(X_prueba, center = medias, scale = desviaciones)

# Busqueda del hiperparámetro k 
k_candidatos <- seq(3, 31, by = 2)
resultados_cv <- data.frame(k = integer(), 
                            accuracy_media = numeric(), 
                            accuracy_sd = numeric())

# Para cada valor de k
# k = 3
for(k_actual in k_candidatos) {
  # Crear 5 pliegues
  pliegues <- createFolds(Y_entrena, k = 5)
  accuracies_pliegues <- numeric()
  
  # Validación cruzada
  # i = 1
  for(i in 1:5) {
    # Índices de validación
    indices_val <- pliegues[[i]]
    
    # Dividir en train y validación
    X_train_cv <- X_entrena_norm[-indices_val, ]
    X_val_cv <- X_entrena_norm[indices_val, ]
    y_train_cv <- Y_entrena[-indices_val]
    y_val_cv <- Y_entrena[indices_val]
    
    # Entrenar y predecir
    pred_cv <- knn(train = X_train_cv,
                   test = X_val_cv,
                   cl = y_train_cv,
                   k = k_actual)
    
    # Calcular accuracy
    accuracy_pliegue <- sum(pred_cv == y_val_cv) / length(y_val_cv)
    accuracies_pliegues <- c(accuracies_pliegues, accuracy_pliegue)
  }
  
  # Guardar resultados
  resultados_cv <- rbind(resultados_cv,
                         data.frame(k = k_actual,
                                    accuracy_media = mean(accuracies_pliegues),
                                    accuracy_sd = sd(accuracies_pliegues)))
}

# Mostrar resultados de validación cruzada
print(resultados_cv)

# Visualizar resultados con barras de error
plot_cv <- ggplot(resultados_cv, aes(x = k, y = accuracy_media)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  geom_errorbar(aes(ymin = accuracy_media - accuracy_sd, 
                    ymax = accuracy_media + accuracy_sd),
                width = 0.5, alpha = 0.5) +
  geom_label(aes(label = round(accuracy_media, 4)), vjust = -0.3) + 
  labs(title = "Validación Cruzada: Accuracy vs k",
       subtitle = "Con barras de error (± 1 desviación estándar)",
       x = "Número de vecinos (k)",
       y = "Accuracy promedio") +
  theme_minimal() +
  scale_x_continuous(breaks = k_candidatos)

print(plot_cv)

# Seleccionar k óptimo
k_optimo <- resultados_cv$k[which.max(resultados_cv$accuracy_media)]
accuracy_optima <- max(resultados_cv$accuracy_media)
cat("\nk óptimo encontrado:", k_optimo)
cat("\nAccuracy promedio en CV:", round(accuracy_optima * 100, 2), "%\n")

# Así determinamos el valor óptimo de k = 19

# Entrenamos el modelo ----
predicciones <- knn(train = X_entrena_norm, 
                    test = X_prueba_norm, 
                    cl = Y_entrena, 
                    k = k_optimo)

class(predicciones)

# Obtenemos la matríz de confusión
Y_prueba <- factor(Y_prueba, levels = c("Bueno","Malo", "Regular"))
matriz_conf <- confusionMatrix(predicciones, Y_prueba)
matriz_conf_df <- as_tibble(matriz_conf$table)

matriz_conf_df %>% 
  ggplot(aes(x = Reference, y = Prediction, fill = n)) + 
  geom_tile() + 
  geom_text(aes(label = n)) + 
  scale_fill_gradient(low = "white",
                      high = "green")



for(clase in levels(Y_prueba)) {
  indices_clase <- Y_prueba == clase
  tp <- sum(predicciones[indices_clase] == clase)
  fp <- sum(predicciones[!indices_clase] == clase)
  fn <- sum(predicciones[indices_clase] != clase)
  tn <- sum(predicciones[!indices_clase] != clase)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  especificidad <- tn / (tn + fp)
  
  cat("\nClase:", clase)
  cat("\n  Precisión:", round(precision, 3))
  cat("\n  Recall (Sensibilidad):", round(recall, 3))
  cat("\n  Especificidad:", round(especificidad, 3))
  cat("\n  F1-Score:", round(f1, 3))
}

# ---------------------------------------------
# PASO 12: Predicción con probabilidades
# ---------------------------------------------
# k-NN con probabilidades (proporción de votos)
predicciones_prob <- knn(train = X_entrena_norm,
                         test = X_prueba_norm,
                         cl = Y_entrena,
                         k = k_optimo,
                         prob = TRUE)

# Extraer probabilidades
probabilidades <- attr(predicciones_prob, "prob")

# Crear dataframe con resultados y probabilidades
resultados_prob <- data.frame(
  Real = Y_prueba,
  Predicho = predicciones_prob,
  Probabilidad = probabilidades,
  Correcto = Y_prueba == predicciones_prob
)

# Mostrar casos con baja confianza
cat("\n========== PREDICCIONES CON BAJA CONFIANZA ==========\n")
casos_baja_confianza <- which(probabilidades < 0.6)
if(length(casos_baja_confianza) > 0) {
  cat("Vinos clasificados con menos del 60% de confianza:\n")
  print(resultados_prob[casos_baja_confianza, ])
} else {
  cat("Todos los vinos fueron clasificados con confianza >= 60%\n")
}

# Clasificar nuevos vinos 
nuevos_vinos <- readxl::read_xlsx("vinos_nuevos.xlsx")
nuevos_vinos_scale <- scale(nuevos_vinos, center = medias, scale = desviaciones)
nuevos_vinos_scale_df <- as_tibble(nuevos_vinos_scale)

knn(train = X_entrena_norm,
    test = nuevos_vinos_scale_df,
    cl = Y_entrena,
    k = k_optimo,
    prob = TRUE)

# ---------------------------------------------
# PASO 14: Visualización 2D de resultados
# ---------------------------------------------
# Usar PCA para reducir a 2 dimensiones y visualizar
pca_result <- prcomp(X_entrena_norm, scale = FALSE)
X_entrena_pca <- pca_result$x[, 1:2]
X_prueba_pca <- predict(pca_result, X_prueba_norm)[, 1:2]
# ?predict

# Crear dataframe para visualización
datos_viz <- data.frame(
  PC1 = X_prueba_pca[, 1],
  PC2 = X_prueba_pca[, 2],
  Real = Y_prueba,
  Predicho = predicciones,
  Error = Y_prueba != predicciones
)

plot_pca <- ggplot(datos_viz, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Real, shape = Predicho), size = 4, alpha = 0.7) +
  geom_point(data = subset(datos_viz, Error), 
             aes(x = PC1, y = PC2), 
             color = "black", size = 6, shape = 1, stroke = 2) +
  labs(title = paste("Visualización PCA de Resultados k-NN (k =", k_optimo, ")"),
       subtitle = "Círculos negros indican errores de clasificación",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal() +
  scale_color_manual(values = c("Malo" = "#d62728", 
                                "Regular" = "#ff7f0e", 
                                "Bueno" = "#2ca02c"))

print(plot_pca)


