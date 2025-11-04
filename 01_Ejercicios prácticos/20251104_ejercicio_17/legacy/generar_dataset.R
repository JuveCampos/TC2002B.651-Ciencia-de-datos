
# ============================================================
# GENERADOR DE DATASET PARA ANÁLISIS DE SENTIMIENTO
# ============================================================
# Este script genera un dataset de reseñas en español con
# casos variados para probar métodos de clasificación
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(writexl)
})

# ---------------------------
# DATASET DE RESEÑAS
# ---------------------------
cat("Generando dataset de reseñas...\n")

reseñas <- tibble(
  id = 1:60,
  contexto = c(
    # RESTAURANTES (15)
    rep("Restaurante", 15),
    # PRODUCTOS TECNOLOGÍA (15)
    rep("Producto Tech", 15),
    # SERVICIOS (15)
    rep("Servicio", 15),
    # APPS/SOFTWARE (15)
    rep("App/Software", 15)
  ),
  texto = c(
    # ========== RESTAURANTES (15) ==========
    # Positivos (5)
    "La comida estuvo deliciosa, el servicio excelente y el ambiente perfecto. Totalmente recomendado.",
    "Increíble experiencia culinaria. Los platillos superaron mis expectativas. Volveré sin duda.",
    "Excelente atención del personal y la comida llegó rápido y caliente. Muy satisfecho.",
    "Me encantó todo, desde la entrada hasta el postre. Precios justos y gran calidad.",
    "¡Wow! El mejor restaurante al que he ido en mucho tiempo. Todo perfecto.",

    # Negativos (5)
    "Pésimo servicio, la comida llegó fría y el mesero fue grosero. No vuelvo más.",
    "Una hora esperando y cuando llegó la comida estaba horrible. Muy decepcionado.",
    "Carísimo para lo que ofrecen. La comida es mediocre y el lugar estaba sucio.",
    "La peor experiencia gastronómica de mi vida. No lo recomiendo para nada.",
    "El personal parecía molesto por atendernos. La comida sin sabor y cara.",

    # Neutros (2)
    "Un restaurante normal. La comida está bien, nada extraordinario pero cumple.",
    "Es un lugar promedio. Ni bueno ni malo, sirve para salir del paso.",

    # Sarcasmo/Ironía (2)
    "Genial, justo lo que quería: esperar dos horas para comer algo frío.",
    "Qué maravilla de restaurante, me encanta pagar mucho por comida mala.",

    # Mixtos (1)
    "La comida está rica pero el servicio es lentísimo y los precios muy altos.",

    # ========== PRODUCTOS TECNOLOGÍA (15) ==========
    # Positivos (5)
    "Excelente producto, funciona perfecto y llegó antes de tiempo. Muy recomendable.",
    "La mejor compra que he hecho. Calidad increíble y precio justo.",
    "Superó mis expectativas por completo. Funciona de maravilla y es muy intuitivo.",
    "100% satisfecho con mi compra. Llevo un mes usándolo sin problemas.",
    "Producto de alta calidad. Vale cada peso. Lo recomiendo ampliamente.",

    # Negativos (5)
    "No funciona como prometen. Me arrepiento de la compra, es una estafa.",
    "Pésima calidad, se descompuso a la semana. No lo compren.",
    "Horrible producto. No sirve para nada y el soporte técnico es inexistente.",
    "Basura total. No cumple con ninguna de las especificaciones anunciadas.",
    "Me llegó defectuoso y nadie responde para hacer la devolución. Fatal.",

    # Neutros (2)
    "Es un producto básico que cumple su función. Nada especial pero funciona.",
    "Ni bueno ni malo. Hace lo que tiene que hacer, sin más.",

    # Sarcasmo/Ironía (2)
    "Perfecto, me encanta cuando los productos no funcionan al sacarlos de la caja.",
    "Fantástico, justo necesitaba algo que se rompiera en una semana.",

    # Mixtos (1)
    "El producto es bueno pero llegó tarde y el empaque venía dañado.",

    # ========== SERVICIOS (15) ==========
    # Positivos (5)
    "Servicio impecable. Muy profesionales y resolvieron mi problema rápidamente.",
    "Excelente atención al cliente. Me ayudaron en todo momento. Muy agradecido.",
    "Súper eficientes y amables. El mejor servicio que he recibido.",
    "Quedé muy satisfecho con el trabajo realizado. Altamente recomendable.",
    "Profesionales de primera. Cumplieron todo lo prometido y más.",

    # Negativos (5)
    "Pésimo servicio. No resolvieron nada y además fueron groseros.",
    "Nunca contestaron mis llamadas. Servicio al cliente inexistente. Terrible.",
    "Me cobraron de más y cuando reclamé me colgaron el teléfono. Indignante.",
    "El peor servicio que he contratado. Llegaron tarde y el trabajo quedó mal.",
    "No lo recomiendo. Son poco profesionales y el servicio es malísimo.",

    # Neutros (2)
    "Servicio regular. Cumplieron pero sin nada destacable.",
    "Normal. Hicieron lo básico que pedí, nada más.",

    # Sarcasmo/Ironía (2)
    "Increíble servicio, me encanta que me dejen esperando tres horas sin avisar.",
    "Qué atención tan maravillosa, ignorar a los clientes es su especialidad.",

    # Mixtos (1)
    "El trabajo quedó bien pero tardaron el doble de lo acordado.",

    # ========== APPS/SOFTWARE (15) ==========
    # Positivos (5)
    "App excelente, muy intuitiva y rápida. La uso todos los días.",
    "La mejor aplicación en su categoría. Funciona perfecto y sin bugs.",
    "Me encanta esta app. Interface limpia y cumple perfectamente su función.",
    "Súper útil y bien diseñada. Vale la pena la versión premium.",
    "Aplicación fantástica. Ha mejorado mucho mi productividad.",

    # Negativos (5)
    "App horrible. Se cuelga constantemente y pierde mi información.",
    "No sirve. Llena de anuncios y funciona muy mal. La desinstalé.",
    "Pésima experiencia. Crashea todo el tiempo y consume mucha batería.",
    "Basura de aplicación. No funciona ni la mitad de las funciones.",
    "La peor app que he usado. Interface confusa y llena de errores.",

    # Neutros (2)
    "App normal. Hace lo básico pero nada impresionante.",
    "Está bien para uso ocasional. Ni buena ni mala.",

    # Sarcasmo/Ironía (2)
    "Genial, me fascina cuando una app se cierra sola cada cinco minutos.",
    "Perfecta, justo lo que buscaba: pagar por una app que no funciona.",

    # Mixtos (1)
    "La app tiene buenas funciones pero es muy lenta y tiene muchos anuncios."
  ),
  etiqueta_real = c(
    # Restaurantes
    "positivo", "positivo", "positivo", "positivo", "positivo",
    "negativo", "negativo", "negativo", "negativo", "negativo",
    "neutro", "neutro",
    "negativo", "negativo",  # Sarcasmo
    "negativo",  # Mixto negativo

    # Productos Tech
    "positivo", "positivo", "positivo", "positivo", "positivo",
    "negativo", "negativo", "negativo", "negativo", "negativo",
    "neutro", "neutro",
    "negativo", "negativo",  # Sarcasmo
    "neutro",  # Mixto neutro

    # Servicios
    "positivo", "positivo", "positivo", "positivo", "positivo",
    "negativo", "negativo", "negativo", "negativo", "negativo",
    "neutro", "neutro",
    "negativo", "negativo",  # Sarcasmo
    "neutro",  # Mixto neutro

    # Apps/Software
    "positivo", "positivo", "positivo", "positivo", "positivo",
    "negativo", "negativo", "negativo", "negativo", "negativo",
    "neutro", "neutro",
    "negativo", "negativo",  # Sarcasmo
    "negativo"  # Mixto negativo
  ),
  tipo_caso = c(
    # Restaurantes
    rep("claro_positivo", 5),
    rep("claro_negativo", 5),
    rep("claro_neutro", 2),
    rep("sarcasmo", 2),
    "mixto",

    # Productos Tech
    rep("claro_positivo", 5),
    rep("claro_negativo", 5),
    rep("claro_neutro", 2),
    rep("sarcasmo", 2),
    "mixto",

    # Servicios
    rep("claro_positivo", 5),
    rep("claro_negativo", 5),
    rep("claro_neutro", 2),
    rep("sarcasmo", 2),
    "mixto",

    # Apps/Software
    rep("claro_positivo", 5),
    rep("claro_negativo", 5),
    rep("claro_neutro", 2),
    rep("sarcasmo", 2),
    "mixto"
  ),
  # Columnas vacías para predicciones
  pred_lexicon = NA_character_,
  pred_emb_local = NA_character_,
  pred_emb_openai = NA_character_,
  pred_llama31 = NA_character_,
  pred_qwen25 = NA_character_,
  # Columna para notas
  notas = NA_character_
)

# ---------------------------
# ESTADÍSTICAS DEL DATASET
# ---------------------------
cat("\n=== ESTADÍSTICAS DEL DATASET ===\n")
cat("Total de reseñas:", nrow(reseñas), "\n\n")

cat("Por contexto:\n")
print(table(reseñas$contexto))

cat("\nPor etiqueta:\n")
print(table(reseñas$etiqueta_real))

cat("\nPor tipo de caso:\n")
print(table(reseñas$tipo_caso))

# Distribución cruzada
cat("\nDistribución contexto x etiqueta:\n")
print(table(reseñas$contexto, reseñas$etiqueta_real))

# ---------------------------
# EXPORTAR A EXCEL
# ---------------------------
output_file <- "dataset_sentimientos.xlsx"

cat("\n=== EXPORTANDO A EXCEL ===\n")
write_xlsx(
  reseñas,
  path = output_file,
  col_names = TRUE
)

cat("✓ Dataset exportado a:", output_file, "\n")

# ---------------------------
# CREAR VERSIÓN CSV (BACKUP)
# ---------------------------
csv_file <- "dataset_sentimientos.csv"
write_csv(reseñas, csv_file)
cat("✓ Backup CSV creado:", csv_file, "\n")

# ---------------------------
# INFORMACIÓN ADICIONAL
# ---------------------------
cat("\n=== INFORMACIÓN DEL DATASET ===\n")
cat("Columnas incluidas:\n")
cat("  - id: Identificador único\n")
cat("  - contexto: Categoría de la reseña\n")
cat("  - texto: Texto de la reseña\n")
cat("  - etiqueta_real: Clasificación correcta (positivo/negativo/neutro)\n")
cat("  - tipo_caso: Tipo de dificultad del caso\n")
cat("  - pred_*: Columnas vacías para predicciones de cada método\n")
cat("  - notas: Columna para tus observaciones\n\n")

cat("CASOS ESPECIALES A REVISAR:\n")
cat("  - Sarcasmo (IDs con tipo_caso='sarcasmo'):",
    sum(reseñas$tipo_caso == "sarcasmo"), "casos\n")
cat("  - Mixtos (IDs con tipo_caso='mixto'):",
    sum(reseñas$tipo_caso == "mixto"), "casos\n\n")

cat("RECOMENDACIONES:\n")
cat("1. Revisa las etiquetas_real y ajusta si no estás de acuerdo\n")
cat("2. Puedes agregar más reseñas editando el Excel\n")
cat("3. Las columnas pred_* se llenarán al ejecutar el script de clasificación\n")
cat("4. Usa la columna 'notas' para casos interesantes o ambiguos\n\n")

cat("Para usar este dataset en t3.R, modifica el script para leer:\n")
cat("  textos <- read_excel('dataset_sentimientos.xlsx')\n\n")

cat("========================================\n")
cat("Dataset generado exitosamente!\n")
cat("========================================\n")
