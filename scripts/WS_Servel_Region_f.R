lista_regiones <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/regiones/all.json")

diccionario_reg <- data.frame(cod_reg = sapply(lista_regiones, "[[", 1),
                              region = sapply(lista_regiones, "[[", 2))

scrap_reg <- function(x){

  detalle_regiones <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/regiones/",
                                             x,
                                             ".json"))
  data.frame(
    cod_reg = x,
    apruebo = as.numeric(str_remove_all(detalle_regiones[[4]][[1]]$c, "\\.")),
    rechazo = as.numeric(str_remove_all(detalle_regiones[[4]][[2]]$c, "\\.")),
    nulo = as.numeric(str_remove_all(detalle_regiones[[5]][[2]]$c, "\\.")),
    blanco = as.numeric(str_remove_all(detalle_regiones[[5]][[3]]$c, "\\."))
    )
}

resultados_region <- map(diccionario_reg$cod_reg, scrap_reg) %>% 
  bind_rows() %>% 
  left_join(diccionario_reg, by = "cod_reg") %>% 
  rowwise() %>% 
  mutate(votos_validos = sum(apruebo, rechazo),
         votos_totales = sum(votos_validos, nulo, blanco)#,
         #participacion = vot_tot/padron
  ) %>% 
  ungroup() %>% 
  mutate(orden_reg = case_when(
           region == "DE ARICA Y PARINACOTA" ~ 1,
           region == "DE TARAPACA" ~ 2,
           region == "DE ANTOFAGASTA" ~ 3,
           region == "DE ATACAMA" ~ 4,
           region == "DE COQUIMBO" ~ 5,
           region == "DE VALPARAISO" ~ 6,
           region == "METROPOLITANA DE SANTIAGO" ~ 7,
           region == "DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS" ~ 8,
           region == "DEL MAULE" ~ 9,
           region == "DE ÑUBLE" ~ 10,
           region == "DEL BIOBIO" ~ 11,
           region == "DE LA ARAUCANIA" ~ 12,
           region == "DE LOS RIOS" ~ 13,
           region == "DE LOS LAGOS" ~ 14,
           region == "DE AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO" ~ 15,
           region == "DE MAGALLANES Y DE LA ANTARTICA CHILENA" ~ 16 
         )) %>% 
  arrange(orden_reg) %>% 
  mutate(region = str_remove_all(region, "DE "),
         region = str_remove_all(region, "DEL "),
         region = fct_reorder(region, orden_reg)) %>% 
  select(region, cod_reg, apruebo, rechazo, nulo, blanco, votos_validos, votos_totales)

rm(lista_regiones, scrap_reg, diccionario_reg)
