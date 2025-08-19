# Biblioteca de Ejemplos de Visualización
## Proyecto: México ¿Cómo Vamos?

Esta biblioteca contiene scripts de ejemplo reproducibles para los tipos de visualización más utilizados en el proyecto "México ¿Cómo Vamos?". Cada script está diseñado como una plantilla reutilizable que sigue los estándares de diseño y mejores prácticas identificados en el análisis del código existente.

## 📊 Tipos de Visualización Incluidos

### 1. Series de Tiempo
- **01_series_tiempo_linea.R**: Líneas simples para indicadores económicos
- **04_series_multiples.R**: Comparaciones entre países/regiones

### 2. Gráficos de Barras
- **02_barras_verticales.R**: Comparaciones por categorías
- **03_barras_horizontales.R**: Rankings y clasificaciones
- **05_barras_agrupadas.R**: Comparaciones por grupos (ej. género)

### 3. Análisis Espacial
- **06_mapa_coropletico.R**: Mapas temáticos por entidad federativa

### 4. Análisis Multivariado
- **07_dispersion_burbujas.R**: Relaciones entre 3+ variables
- **08_facetas_multiples.R**: Comparaciones simultáneas de múltiples indicadores

## 🎨 Elementos de Diseño Identificados

### Paletas de Colores Estándar
```r
# Paleta principal
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")

# Semáforo (para clasificaciones)
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja, rojo

# Escala de grises
mcv_blacks <- c("black", "#D2D0CD", "#777777")
```

### Tipografía
- **Fuente principal**: Ubuntu (via extrafont)
- **Títulos**: 40pt, bold, color #6950D8
- **Subtítulos**: 30pt, color #777777
- **Etiquetas de ejes**: 20-25pt
- **Anotaciones**: 15-18pt

### Espaciado y Proporciones
- **Dimensiones estándar**: 16x9 (landscape), 13x16 (portrait para rankings)
- **DPI**: 200-300 para publicación
- **Márgenes**: margin(0.3, 0.3, 1.4, 0.3, "cm")

## 🔧 Patrones Técnicos Implementados

### 1. Configuración Inicial Estándar
```r
# Configuración de idioma y formato
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

# Reproducibilidad
set.seed(123)
```

### 2. Manejo de Datos
- **Fechas**: Uso de lubridate para manipulación temporal
- **Escalas**: scales::percent_format(), scales::dollar_format()
- **NA handling**: Uso de ifelse() y case_when() para casos edge

### 3. Anotaciones Contextuales
- **Eventos históricos**: Rectángulos sombreados para COVID-19
- **Referencias**: Líneas horizontales/verticales para promedios
- **Destacados**: Puntos especiales para últimos valores

### 4. Etiquetado Inteligente
- **Posicionamiento**: vjust/hjust dinámico según valor
- **Repulsión**: ggrepel para evitar solapamiento
- **Condicional**: Etiquetas solo en datos relevantes

## 📁 Estructura de Cada Script

Todos los scripts siguen esta estructura estándar:

```r
# ====================================
# TIPO DE GRÁFICA: [Nombre]
# PROYECTO: México ¿Cómo Vamos?
# ====================================

# LIBRERÍAS
library(ggplot2)
[otras librerías necesarias]

# CONFIGURACIÓN
[configuración de idioma y semilla]

# PALETAS DE COLORES MCV
[definición de paletas estándar]

# DATOS SIMULADOS
[código de generación de datos]

# VISUALIZACIÓN
[código del gráfico con comentarios]

# EXPORTACIÓN
ggsave("nombre_archivo.png", width = 12, height = 8, dpi = 300)
```

## 🎯 Casos de Uso Típicos

### Para Indicadores Económicos
- **Series de tiempo**: PIB, inflación, tipo de cambio
- **Mapas**: Distribución regional de indicadores
- **Comparaciones**: México vs otros países

### Para Datos Sociales
- **Brechas de género**: Barras agrupadas por sexo
- **Rankings estatales**: Barras horizontales ordenadas
- **Evolución temporal**: Facetas múltiples por indicador

### Para Datos Sectoriales
- **Contribuciones**: Barras por sector económico
- **Relaciones**: Dispersión PIB vs educación vs población

## 🚀 Instrucciones de Uso

### Requisitos Previos
```r
# Instalar paquetes necesarios
install.packages(c("ggplot2", "tidyverse", "scales", "extrafont", 
                   "lubridate", "ggrepel", "sf"))

# Cargar fuentes del sistema
extrafont::font_import()
```

### Personalización Rápida
1. **Cambiar datos**: Reemplazar la sección "DATOS SIMULADOS"
2. **Ajustar títulos**: Modificar variables titulo, subtitulo, eje_y
3. **Adaptar colores**: Usar las paletas MCV o personalizar
4. **Configurar exportación**: Ajustar dimensiones según necesidad

### Ejemplo de Personalización
```r
# Cambiar de datos simulados a datos reales
datos_reales <- tu_base_de_datos %>%
  filter(fecha >= "2020-01-01") %>%
  select(fecha, valor, categoria)

# Adaptar títulos
titulo <- "Tu Título Personalizado"
subtitulo <- "Tu Subtítulo con Contexto"

# Usar paleta específica
colores_custom <- c("Categoría A" = mcv_discrete[1], 
                   "Categoría B" = mcv_discrete[2])
```

## 📈 Mejores Prácticas Identificadas

### 1. Consistencia Visual
- Usar siempre las paletas MCV estándar
- Mantener tipografía Ubuntu en todas las gráficas
- Respetar proporciones 16:9 para presentaciones

### 2. Accesibilidad
- Evitar depender solo del color para transmitir información
- Usar formas y patrones como apoyo
- Incluir etiquetas directas cuando sea posible

### 3. Contexto Mexicano
- Incluir referencias a eventos relevantes (COVID-19, reformas)
- Usar datos que reflejen la realidad mexicana
- Mantener perspectiva comparativa internacional cuando aplique

### 4. Reproducibilidad
- Establecer semillas para datos aleatorios
- Documentar decisiones de diseño
- Incluir verificaciones de calidad

## 🔍 Verificación de Calidad

Antes de usar cualquier gráfica, verificar:

- [ ] **Datos**: Sin valores faltantes no manejados
- [ ] **Escalas**: Rangos apropiados y etiquetas claras  
- [ ] **Colores**: Contraste suficiente y paleta MCV
- [ ] **Texto**: Sin caracteres especiales mal codificados
- [ ] **Exportación**: Dimensiones y resolución correctas

## 📚 Referencias y Créditos

- **Proyecto base**: México ¿Cómo Vamos?
- **Análisis realizado en**: 50+ scripts de 02_códigos/
- **Librerías principales**: ggplot2, tidyverse, scales
- **Inspiración de diseño**: Lineamientos de comunicación MCV

---

**Nota**: Todos los datos en estos ejemplos son simulados y tienen propósitos exclusivamente educativos. Para uso en producción, reemplazar con datos reales del proyecto.