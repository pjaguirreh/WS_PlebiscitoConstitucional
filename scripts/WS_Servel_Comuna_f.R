lista_comunas <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/comunas/all.json")

diccionario_com <- data.frame(cod_com = sapply(lista_comunas, "[[", 1),
                              comuna = sapply(lista_comunas, "[[", 2))

scrap_comunas <- function(x){

  detalle_comunas <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/comunas/",
                                             x,
                                             ".json"))
  data.frame(
    cod_com = x,
    apruebo = as.numeric(str_remove_all(detalle_comunas[[4]][[1]]$c, "\\.")),
    rechazo = as.numeric(str_remove_all(detalle_comunas[[4]][[2]]$c, "\\.")),
    nulo = as.numeric(str_remove_all(detalle_comunas[[5]][[2]]$c, "\\.")),
    blanco = as.numeric(str_remove_all(detalle_comunas[[5]][[3]]$c, "\\."))
    )
}


resultados_comuna <- map(diccionario_com$cod_com, scrap_comunas) %>% 
  bind_rows() %>% 
  left_join(diccionario_com, by = "cod_com") %>% 
  left_join(read_csv("planillas_aux/Lista_RegionDistritoComuna.csv"), by = "comuna") %>% 
  rowwise() %>% 
  mutate(votos_validos = sum(apruebo, rechazo),
         votos_totales = sum(votos_validos, nulo, blanco)#,
         #participacion = vot_tot/padron
  ) %>% 
  ungroup() %>% 
  select(region, comuna, cod_com, apruebo, rechazo, nulo, blanco, votos_validos, votos_totales) %>% 
  mutate(region = str_remove_all(region, "DE "),
         region = str_remove_all(region, "DEL "))

pob_com <- read_excel("planillas_aux/Estimaciones_de_Tasa_de_Pobreza_por_Ingresos_por_Comunas_2020.xlsx") %>% 
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
