
# Librerias -----
library(tidyverse)
library(DBI)
library(RSQLite)
library(haven)
library(foreign)
library(arrow)
library(sf)
# mas las que se vayan sumando 

# primer archivo 
"archivos_carga/base_datos_clientes.sqlite"
con <- dbConnect(SQLite(), "archivos_carga/base_datos_clientes.sqlite")
dbListTables(con)
dbListFields(con, "clientes")

# segundo archivo 
# parquet 
"archivos_carga/datos_parquet.parquet"
parquet <- arrow::read_parquet("archivos_carga/datos_parquet.parquet")

# tercer archivo 
"archivos_carga/cola.sas7bdat"
sas <- read_sas("archivos_carga/cola.sas7bdat")

# cuarto archivo 
"archivos_carga/datos_financieros.feather"
feather <- read_feather("archivos_carga/datos_financieros.feather")

# quinto archivo 
"archivos_carga/estudio_psicologico.sav"
sav <- read_sav("archivos_carga/estudio_psicologico.sav")

# sexto archivo 
"archivos_carga/shapefile/regiones_comerciales.shp"
sf <- read_sf("archivos_carga/shapefile/regiones_comerciales.shp")

