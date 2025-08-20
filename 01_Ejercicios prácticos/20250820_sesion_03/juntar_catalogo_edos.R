
library(tidyverse)
cat_edos <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv") %>% 
  mutate(cve_ent = as.numeric(cve_ent))
pob <- read_csv("df_pob_ent.csv")
bd <- left_join(pob, cat_edos) %>% 
  relocate(c(anio, entidad, entidad_abr_m), .before = "cve_ent") 
openxlsx::write.xlsx(bd, "df_pob_ent2.csv")
