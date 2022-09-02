comunas_poblacion <- read_csv("planillas_aux/PoblacionComuna.csv") %>% 
  mutate(comuna = str_to_upper(comuna),
         comuna = case_when(
           comuna == "CAMINA" ~ "CAMIÑA",
           comuna == "CHANARAL" ~ "CHAÑARAL",
           comuna == "VICUNA" ~ "VICUÑA",
           comuna == "VINA DEL MAR" ~ "VIÑA DEL MAR",
           comuna == "DONIHUE" ~ "DOÑIHUE",
           comuna == "MARCHIHUE" ~ "MARCHIGUE",
           comuna == "HUALANE" ~ "HUALAÑE",
           comuna == "CANETE" ~ "CAÑETE",
           comuna == "OHIGGINS" ~ "O'HIGGINS",
           comuna == "RIO IBANEZ" ~ "PUERTO IBAÑEZ",
           comuna == "CABO DE HORNOS" ~ "CABO DE HORNOS(EX-NAVARINO)",
           comuna == "NUNOA" ~ "ÑUÑOA",
           comuna == "PENAFLOR" ~ "PEÑAFLOR",
           comuna == "PENALOLEN" ~ "PEÑALOLEN",
           comuna == "NIQUEN" ~ "ÑIQUEN",
           comuna == "TREGUACO" ~ "TREHUACO",
           TRUE ~ comuna
         ))

comunas_pobreza <- read_excel("planillas_aux/Estimaciones_de_Tasa_de_Pobreza_por_Ingresos_por_Comunas_2020.xlsx") %>% 
  mutate(`Nombre comuna` = str_to_upper(`Nombre comuna`),
         `Nombre comuna` = stri_trans_general(str = `Nombre comuna`, id = "Latin-ASCII"),
         `Nombre comuna` = case_when(
           `Nombre comuna` == "TREGUACO" ~ "TREHUACO",
           `Nombre comuna` == "MARCHIHUE" ~ "MARCHIGUE",
           TRUE ~ `Nombre comuna`
         )) %>% 
  rename("cod_casen" = `Código`,
         "com_nom" = `Nombre comuna`,
         "per_pob2020" = `Porcentaje de personas en situación de pobreza por ingresos 2020`) %>% 
  select(cod_casen, com_nom, per_pob2020)