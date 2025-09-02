
library(tidyverse)

tabla_larga <- tibble::tribble(
   ~Persona, ~Variable, ~Valor,
  "Roberto",    "Edad",    32L,
  "Roberto",    "Peso",   168L,
  "Roberto",  "Altura",   180L,
   "Alicia",    "Edad",    24L,
   "Alicia",    "Peso",   150L,
   "Alicia",  "Altura",   175L,
  "Esteban",    "Edad",    64L,
  "Esteban",    "Peso",   144L,
  "Esteban",  "Altura",   165L,
  "Juvenal",    "Edad",    29L,
  "Juvenal",    "Peso",   200L,
  "Juvenal",  "Altura",   185L
  )


tabla_larga %>% 
  pivot_wider(id_cols = Persona, names_from = Variable, values_from = Valor)

