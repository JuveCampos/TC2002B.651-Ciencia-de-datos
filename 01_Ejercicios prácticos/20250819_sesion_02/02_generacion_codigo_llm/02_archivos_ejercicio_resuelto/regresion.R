# Script de Regresión Lineal Simple en R
# Autor: Programador R
# Fecha: 2025-08-19

# Cargar librerías necesarias
library(ggplot2)  # Para gráficos elegantes
library(dplyr)    # Para manipulación de datos
library(broom)    # Para extraer estadísticas del modelo

# ==============================================================================
# PASO 1: CREAR CONJUNTO DE DATOS SINTÉTICO
# ==============================================================================

# Configurar semilla para reproducibilidad
set.seed(123)

# Crear dataset sintético: Relación entre horas de estudio y calificación final
n <- 100  # Número de observaciones

# Variable independiente (X): Horas de estudio por semana
horas_estudio <- runif(n, min = 5, max = 40)  # Entre 5 y 40 horas

# Crear relación lineal con ruido
# Ecuación: calificacion = 60 + 1.2 * horas_estudio + error
calificacion <- 60 + 1.2 * horas_estudio + rnorm(n, mean = 0, sd = 5)

# Crear dataframe
datos <- data.frame(
  horas_estudio = horas_estudio,
  calificacion = calificacion
)

# Mostrar primeras filas del dataset
cat("Primeras 6 filas del dataset:\n")
print(head(datos))

# Estadísticas descriptivas básicas
cat("\nEstadísticas descriptivas:\n")
print(summary(datos))

# ==============================================================================
# PASO 2: IMPLEMENTAR MODELO DE REGRESIÓN LINEAL SIMPLE
# ==============================================================================

# Ajustar modelo de regresión lineal
# Formula: Y ~ X (calificacion en función de horas_estudio)
modelo <- lm(calificacion ~ horas_estudio, data = datos)

# Mostrar resumen del modelo
cat("\n", strrep("=", 60), "\n")
cat("RESUMEN DEL MODELO DE REGRESIÓN LINEAL\n")
cat(strrep("=", 60), "\n")
print(summary(modelo))

# Extraer coeficientes del modelo
coeficientes <- coef(modelo)
intercepto <- coeficientes[1]
pendiente <- coeficientes[2]

cat("\nCoeficientes del modelo:\n")
cat("Intercepto (β₀):", round(intercepto, 3), "\n")
cat("Pendiente (β₁):", round(pendiente, 3), "\n")

# Interpretación de los coeficientes
cat("\nInterpretación:\n")
cat("- El intercepto indica que un estudiante con 0 horas de estudio tendría una calificación de", round(intercepto, 1), "\n")
cat("- Por cada hora adicional de estudio, la calificación aumenta en", round(pendiente, 3), "puntos\n")

# ==============================================================================
# PASO 3: GENERAR GRÁFICO DE DISPERSIÓN CON LÍNEA DE REGRESIÓN
# ==============================================================================

# Crear gráfico principal con ggplot2
p1 <- ggplot(datos, aes(x = horas_estudio, y = calificacion)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +  # Puntos de datos
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +  # Línea de regresión con IC
  labs(
    title = "Regresión Lineal Simple: Horas de Estudio vs Calificación",
    subtitle = paste("Ecuación: Calificación =", round(intercepto, 2), "+", round(pendiente, 2), "× Horas de Estudio"),
    x = "Horas de Estudio por Semana",
    y = "Calificación Final",
    caption = "Datos sintéticos generados para demostración"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  )

print(p1)

# ==============================================================================
# PASO 4: DIAGNÓSTICO DEL MODELO - GRÁFICO DE RESIDUOS
# ==============================================================================

# Calcular residuos y valores ajustados
datos$residuos <- residuals(modelo)
datos$valores_ajustados <- fitted(modelo)

# Gráfico de residuos vs valores ajustados
p2 <- ggplot(datos, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Diagnóstico del Modelo: Residuos vs Valores Ajustados",
    x = "Valores Ajustados",
    y = "Residuos",
    caption = "La línea horizontal indica residuos = 0"
  ) +
  theme_minimal()

print(p2)

# QQ-plot para verificar normalidad de residuos
p3 <- ggplot(datos, aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "QQ-Plot: Verificación de Normalidad de Residuos",
    x = "Cuantiles Teóricos",
    y = "Cuantiles de Residuos"
  ) +
  theme_minimal()

print(p3)

# ==============================================================================
# PASO 5: VISUALIZACIÓN CON INTERVALOS DE CONFIANZA Y PREDICCIÓN
# ==============================================================================

# Crear secuencia de valores para predicción
nuevas_horas <- seq(min(datos$horas_estudio), max(datos$horas_estudio), length.out = 100)
nuevos_datos <- data.frame(horas_estudio = nuevas_horas)

# Predicciones con intervalos de confianza
predicciones_ic <- predict(modelo, nuevos_datos, interval = "confidence", level = 0.95)

# Predicciones con intervalos de predicción
predicciones_ip <- predict(modelo, nuevos_datos, interval = "prediction", level = 0.95)

# Combinar datos para visualización
datos_prediccion <- data.frame(
  horas_estudio = nuevas_horas,
  ajustado = predicciones_ic[, "fit"],
  ic_inferior = predicciones_ic[, "lwr"],
  ic_superior = predicciones_ic[, "upr"],
  ip_inferior = predicciones_ip[, "lwr"],
  ip_superior = predicciones_ip[, "upr"]
)

# Gráfico con intervalos de confianza y predicción
p4 <- ggplot() +
  # Intervalo de predicción (más amplio)
  geom_ribbon(data = datos_prediccion, aes(x = horas_estudio, ymin = ip_inferior, ymax = ip_superior),
              fill = "lightblue", alpha = 0.3) +
  # Intervalo de confianza (más estrecho)
  geom_ribbon(data = datos_prediccion, aes(x = horas_estudio, ymin = ic_inferior, ymax = ic_superior),
              fill = "blue", alpha = 0.4) +
  # Puntos originales
  geom_point(data = datos, aes(x = horas_estudio, y = calificacion), alpha = 0.6) +
  # Línea de regresión
  geom_line(data = datos_prediccion, aes(x = horas_estudio, y = ajustado), color = "red", size = 1) +
  labs(
    title = "Regresión Lineal con Intervalos de Confianza y Predicción",
    subtitle = "Banda azul oscura: IC 95% | Banda azul clara: IP 95%",
    x = "Horas de Estudio por Semana",
    y = "Calificación Final"
  ) +
  theme_minimal()

print(p4)

# ==============================================================================
# PASO 6: ESTADÍSTICAS ADICIONALES DEL MODELO
# ==============================================================================

# Métricas de bondad de ajuste
r_cuadrado <- summary(modelo)$r.squared
r_cuadrado_adj <- summary(modelo)$adj.r.squared
error_estandar <- summary(modelo)$sigma

cat("\n", strrep("=", 50), "\n")
cat("MÉTRICAS DE BONDAD DE AJUSTE\n")
cat(strrep("=", 50), "\n")
cat("R² (Coeficiente de determinación):", round(r_cuadrado, 4), "\n")
cat("R² Ajustado:", round(r_cuadrado_adj, 4), "\n")
cat("Error estándar residual:", round(error_estandar, 3), "\n")
cat("Número de observaciones:", nrow(datos), "\n")

# Interpretación del R²
cat("\nInterpretación del R²:\n")
cat("El modelo explica el", round(r_cuadrado * 100, 1), "% de la variabilidad en las calificaciones\n")

# Análisis de varianza (ANOVA)
cat("\n", strrep("=", 40), "\n")
cat("ANÁLISIS DE VARIANZA (ANOVA)\n")
cat(strrep("=", 40), "\n")
print(anova(modelo))

# Intervalos de confianza para los coeficientes
cat("\nIntervalos de confianza para los coeficientes (95%):\n")
print(confint(modelo))

# ==============================================================================
# EJEMPLO DE PREDICCIÓN
# ==============================================================================

cat("\n", strrep("=", 40), "\n")
cat("EJEMPLO DE PREDICCIÓN\n")
cat(strrep("=", 40), "\n")

# Predecir calificación para un estudiante que estudia 25 horas por semana
horas_ejemplo <- 25
prediccion <- predict(modelo, newdata = data.frame(horas_estudio = horas_ejemplo),
                     interval = "prediction", level = 0.95)

cat("Para un estudiante que estudia", horas_ejemplo, "horas por semana:\n")
cat("Calificación predicha:", round(prediccion[1], 1), "\n")
cat("Intervalo de predicción 95%: [", round(prediccion[2], 1), ",", round(prediccion[3], 1), "]\n")

cat("\n¡Análisis de regresión lineal completado exitosamente!\n")