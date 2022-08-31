lista_regiones <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/regiones/all.json")

vot_regiones <- tibble(reg_cod = rep(0, length(lista_regiones)), 
                       reg_nom = rep("", length(lista_regiones)), 
                       padron = rep(0, length(lista_regiones)),
                       vot_apruebo = rep(0, length(lista_regiones)), 
                       vot_rechazo = rep(0, length(lista_regiones)), 
                       vot_nulo = rep(0, length(lista_regiones)), 
                       vot_blanco = rep(0, length(lista_regiones)))
                       
# Loop de extracción de datos
tic() # Para medir tiempo (inicio)
for (i in seq_along(lista_regiones)){
  print(i) # para hacer seguimiento a proceso
  
  # datos de votación
  detalle_regiones <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/regiones/",
                                           lista_regiones[[i]]$c,
                                           ".json"))
  
  # datos de padrón electoral
  #padron_comuna <- fromJSON(file = paste0("https://servelelecciones.cl/data/participacion/computo/comunas/",
  #                                        lista_comunas[[i]]$c,
  #                                        ".json"))
  
  # poner resultados en data frame
  vot_regiones[i,1] <- lista_regiones[[i]]$c
  vot_regiones[i,2] <- lista_regiones[[i]]$d
  #vot_regiones[i,3] <- as.numeric(str_remove_all(padron_comuna[[5]][[1]]$c, "\\."))
  vot_regiones[i,4] <- as.numeric(str_remove_all(detalle_regiones[[4]][[1]]$c, "\\."))
  vot_regiones[i,5] <- as.numeric(str_remove_all(detalle_regiones[[4]][[2]]$c, "\\."))
  vot_regiones[i,6] <- as.numeric(str_remove_all(detalle_regiones[[5]][[2]]$c, "\\."))
  vot_regiones[i,7] <- as.numeric(str_remove_all(detalle_regiones[[5]][[3]]$c, "\\."))
}
toc()

datos_votaciones_plebiscito <- vot_regiones %>% 
  rowwise() %>% 
  mutate(vot_validos = sum(vot_apruebo, vot_rechazo),
         vot_tot = sum(vot_validos, vot_nulo, vot_blanco)#,
         #participacion = vot_tot/padron
  ) %>% 
  ungroup() %>% 
  mutate(orden_reg = case_when(
           reg_nom == "DE ARICA Y PARINACOTA" ~ 1,
           reg_nom == "DE TARAPACA" ~ 2,
           reg_nom == "DE ANTOFAGASTA" ~ 3,
           reg_nom == "DE ATACAMA" ~ 4,
           reg_nom == "DE COQUIMBO" ~ 5,
           reg_nom == "DE VALPARAISO" ~ 6,
           reg_nom == "METROPOLITANA DE SANTIAGO" ~ 7,
           reg_nom == "DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS" ~ 8,
           reg_nom == "DEL MAULE" ~ 9,
           reg_nom == "DE ÑUBLE" ~ 10,
           reg_nom == "DEL BIOBIO" ~ 11,
           reg_nom == "DE LA ARAUCANIA" ~ 12,
           reg_nom == "DE LOS RIOS" ~ 13,
           reg_nom == "DE LOS LAGOS" ~ 14,
           reg_nom == "DE AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO" ~ 15,
           reg_nom == "DE MAGALLANES Y DE LA ANTARTICA CHILENA" ~ 16 
         )) %>% 
  select(reg_nom, reg_cod, everything(), -orden_reg)
