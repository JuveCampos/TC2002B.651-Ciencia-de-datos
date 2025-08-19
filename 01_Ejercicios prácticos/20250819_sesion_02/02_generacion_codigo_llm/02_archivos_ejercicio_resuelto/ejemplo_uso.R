# Ejemplo de uso de las funciones de series de tiempo
# Cargar las funciones
source("graficos_serie_tiempo.R")

# Establecer directorio de trabajo si es necesario
# setwd("ruta/a/tu/directorio")

# ==============================================================================
# EJEMPLO 1: Gráfico básico de pobreza para Chiapas (estado con mayor pobreza)
# ==============================================================================

cat("Ejemplo 1: Serie de tiempo de pobreza en Chiapas\n")
p1 <- grafico_serie_tiempo(7)  # Chiapas = código 7
print(p1)

# ==============================================================================
# EJEMPLO 2: Pobreza extrema para Estado de México (estado más poblado)
# ==============================================================================

cat("\nEjemplo 2: Serie de tiempo de pobreza extrema en Estado de México\n")
p2 <- grafico_serie_tiempo(15, indicador = "pobreza_e", color_linea = "red")
print(p2)

# ==============================================================================
# EJEMPLO 3: Comparación entre diferentes estados
# ==============================================================================

cat("\nEjemplo 3: Comparación de pobreza entre estados contrastantes\n")
# Comparar Chiapas (alta pobreza), Nuevo León (baja pobreza) y Estado de México (población alta)
p3 <- comparar_estados(c(7, 19, 15), indicador = "pobreza")
print(p3)

# ==============================================================================
# EJEMPLO 4: Ver estadísticas resumidas
# ==============================================================================

cat("\nEjemplo 4: Estadísticas resumidas para diferentes estados\n")

# Estadísticas para Chiapas (mayor pobreza)
estadisticas_estado(7)

# Estadísticas para Nuevo León (menor pobreza)
estadisticas_estado(19)

cat("\n¡Ejemplos ejecutados exitosamente!\n")