
# ============================================
# ALGORITMO K-NN (K VECINOS MÁS CERCANOS) EN R
# Problema: Clasificación de especies de flores Iris
# ============================================

# ---------------------------------------------
# PASO 1: Cargar librerías necesarias
# ---------------------------------------------
library(class)     # Para el algoritmo knn
library(caret)     # Para partición de datos y matrices de confusión
library(ggplot2)   # Para visualizaciones

# ---------------------------------------------
# PASO 2: Cargar y explorar el dataset
# ---------------------------------------------
# Cargar el dataset Iris (viene incluido en R)
data(iris)

iris %>% 
  openxlsx::write.xlsx("iris.xlsx")

# Explorar la estructura del dataset
str(iris)
# 150 observaciones, 4 variables predictoras y 1 variable objetivo (Species)

# Ver las primeras filas
head(iris)

# Resumen estadístico
summary(iris)

# Contar especies
table(iris$Species)
# 50 flores de cada especie: setosa, versicolor, virginica

# ---------------------------------------------
# PASO 3: Visualizar los datos
# ---------------------------------------------
# Crear una visualización para entender la distribución de las especies
# Usaremos solo 2 variables para poder visualizar en 2D
plot_exploratorio <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Distribución de Especies de Iris",
       x = "Longitud del Pétalo (cm)",
       y = "Ancho del Pétalo (cm)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(plot_exploratorio)

# ---------------------------------------------
# PASO 4: Preparar los datos
# ---------------------------------------------
# Establecer semilla para reproducibilidad
set.seed(123)

# Crear índices para dividir los datos (70% entrenamiento, 30% prueba)
indices_train <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
class(indices_train)
?createDataPartition

# Separar variables predictoras (X) y variable objetivo (y)
X <- iris[, 1:4]  # Las 4 mediciones
y <- iris[, 5]    # La especie

# Dividir en conjuntos de entrenamiento y prueba
X_train <- X[indices_train, ]
X_test <- X[-indices_train, ]
y_train <- y[indices_train]
y_test <- y[-indices_train]

# Verificar las dimensiones
dim(X_train)  # 105 observaciones para entrenamiento
dim(X_test)   # 45 observaciones para prueba

# ---------------------------------------------
# PASO 5: Normalizar los datos
# ---------------------------------------------
# k-NN es sensible a la escala de las variables
# Normalización usando min-max scaling

# Calcular min y max de cada columna en datos de entrenamiento
mins <- apply(X_train, 2, min)
maxs <- apply(X_train, 2, max)

# Normalizar datos de entrenamiento
X_train_norm <- as.data.frame(scale(X_train, center = mins, scale = maxs - mins))

# Normalizar datos de prueba usando los mismos parámetros
X_test_norm <- as.data.frame(scale(X_test, center = mins, scale = maxs - mins))

# Verificar que la normalización funcionó
summary(X_train_norm)  # Valores entre 0 y 1

# ---------------------------------------------
# PASO 6: Encontrar el valor óptimo de k
# ---------------------------------------------
# Probar diferentes valores de k y evaluar el rendimiento

# Vector para almacenar accuracies
accuracies <- numeric()
k_values <- seq(1, 21, by = 2)  # k impar para evitar empates

for(k in k_values) {
  # Hacer predicción con k actual
  pred_k <- knn(train = X_train_norm, 
                test = X_test_norm, 
                cl = y_train, 
                k = k)
  
  # Calcular accuracy
  accuracy <- sum(pred_k == y_test) / length(y_test)
  accuracies <- c(accuracies, accuracy)
}

# Crear dataframe con resultados
resultados_k <- data.frame(k = k_values, accuracy = accuracies)
print(resultados_k)

# Visualizar el efecto de k en la accuracy
plot_k <- ggplot(resultados_k, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Accuracy vs Valor de k",
       x = "Valor de k",
       y = "Accuracy") +
  theme_minimal() +
  scale_x_continuous(breaks = k_values)

print(plot_k)

# Encontrar el k óptimo
k_optimo <- k_values[which.max(accuracies)]
cat("El valor óptimo de k es:", k_optimo, "con accuracy de:", max(accuracies), "\n")

# ---------------------------------------------
# PASO 7: Entrenar el modelo final con k óptimo
# ---------------------------------------------
# Aplicar k-NN con el k óptimo
predicciones_final <- knn(train = X_train_norm,
                          test = X_test_norm,
                          cl = y_train,
                          k = k_optimo)

# ---------------------------------------------
# PASO 8: Evaluar el modelo
# ---------------------------------------------
# Crear matriz de confusión
matriz_confusion <- table(Real = y_test, Predicho = predicciones_final)
print("Matriz de Confusión:")
print(matriz_confusion)

# Calcular métricas de evaluación
accuracy_final <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
cat("\nAccuracy final:", round(accuracy_final * 100, 2), "%\n")

# Calcular precisión, recall y F1 por clase
for(clase in levels(y_test)) {
  tp <- matriz_confusion[clase, clase]
  fp <- sum(matriz_confusion[, clase]) - tp
  fn <- sum(matriz_confusion[clase, ]) - tp
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  cat("\nClase:", clase)
  cat("\n  Precisión:", round(precision, 3))
  cat("\n  Recall:", round(recall, 3))
  cat("\n  F1-Score:", round(f1, 3))
}

# ---------------------------------------------
# PASO 9: Ejemplo de predicción individual
# ---------------------------------------------
# Crear una nueva flor con mediciones inventadas
nueva_flor <- data.frame(
  Sepal.Length = 5.5,
  Sepal.Width = 3.0,
  Petal.Length = 4.0,
  Petal.Width = 1.3
)

cat("\n\nNueva flor a clasificar:")
print(nueva_flor)

# Normalizar la nueva flor con los mismos parámetros
nueva_flor_norm <- as.data.frame(scale(nueva_flor, center = mins, scale = maxs - mins))

# Hacer predicción
prediccion_nueva <- knn(train = X_train_norm,
                        test = nueva_flor_norm,
                        cl = y_train,
                        k = k_optimo)

cat("\nPredicción para la nueva flor: ", as.character(prediccion_nueva), "\n")

# ---------------------------------------------
# PASO 10: Visualizar predicciones vs reales
# ---------------------------------------------
# Crear un dataframe con los resultados
resultados_test <- data.frame(
  Petal.Length = X_test$Petal.Length,
  Petal.Width = X_test$Petal.Width,
  Real = y_test,
  Predicho = predicciones_final
)

# Identificar predicciones correctas e incorrectas
resultados_test$Correcto <- resultados_test$Real == resultados_test$Predicho

# Crear visualización
plot_resultados <- ggplot(resultados_test, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Real, shape = Predicho), size = 4, alpha = 0.7) +
  geom_point(data = subset(resultados_test, !Correcto), 
             aes(x = Petal.Length, y = Petal.Width), 
             color = "red", size = 6, shape = 1, stroke = 2) +
  labs(title = paste("Resultados k-NN con k =", k_optimo),
       subtitle = "Círculos rojos indican predicciones incorrectas",
       x = "Longitud del Pétalo (cm)",
       y = "Ancho del Pétalo (cm)",
       color = "Especie Real",
       shape = "Especie Predicha") +
  theme_minimal()

print(plot_resultados)

# ---------------------------------------------
# RESUMEN FINAL
# ---------------------------------------------
cat("\n========== RESUMEN DEL MODELO k-NN ==========\n")
cat("Dataset: Iris (150 flores, 3 especies)\n")
cat("División: 70% entrenamiento (105), 30% prueba (45)\n")
cat("Características: 4 mediciones normalizadas\n")
cat("Valor óptimo de k:", k_optimo, "\n")
cat("Accuracy final:", round(accuracy_final * 100, 2), "%\n")
cat("Errores de clasificación:", sum(!resultados_test$Correcto), "de", nrow(resultados_test), "\n")
cat("=============================================\n")

