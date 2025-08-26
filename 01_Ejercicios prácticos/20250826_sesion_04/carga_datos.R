
# Carga de archivos 
library(readxl)
library(geojsonR)
library(rjson)
library(sf)
library(jsonlite)
library(foreign)
library(arrow)

arrow::read_parquet("archivos_carga/datos_parquet.parquet")

??read_parquet

"archivos_carga/datos_medicos.sas7bdat"

de_json <- jsonlite::fromJSON("archivos_carga/respuesta_api.json")

# Cargar exceles
empleados <- read_excel("archivos_carga/empleados_empresa.xlsx")

# Carga csv
ventas <- read.csv("archivos_carga/ventas_tienda.csv")

fromJSON("archivos_carga/respuesta_api.json")

ubicaciones <- FROM_GeoJson("archivos_carga/ubicaciones_tiendas.geojson")
class(ubicaciones)
ubicaciones2 <- read_sf("archivos_carga/ubicaciones_tiendas.geojson")

geo <- read_sf("archivos_carga/regiones_comerciales.shp")






