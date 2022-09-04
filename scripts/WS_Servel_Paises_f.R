lista_paises <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/paises/all.json")

diccionario_pais <- data.frame(cod_pais = sapply(lista_paises, "[[", 1),
                               pais = sapply(lista_paises, "[[", 2))

scrap_pais <- function(x){
  
  detalle_paises <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/pais/",
                                             x,
                                             ".json"))
  data.frame(
    cod_pais = x,
    apruebo = as.numeric(str_remove_all(detalle_paises[[4]][[1]]$c, "\\.")),
    rechazo = as.numeric(str_remove_all(detalle_paises[[4]][[2]]$c, "\\.")),
    nulo = as.numeric(str_remove_all(detalle_paises[[5]][[2]]$c, "\\.")),
    blanco = as.numeric(str_remove_all(detalle_paises[[5]][[3]]$c, "\\."))
  )
}

resultados_paises_detalle <- map(diccionario_pais$cod_pais, scrap_pais) %>% 
  bind_rows() %>% 
  left_join(diccionario_pais) %>% 
  select(pais, apruebo, rechazo, nulo, blanco)

resultados_paises <- resultados_paises_detalle %>% 
  summarise(
    apruebo = sum(apruebo, na.rm = TRUE),
    rechazo = sum(rechazo, na.rm = TRUE),
    nulo = sum(nulo, na.rm = TRUE),
    blanco = sum(blanco, na.rm = TRUE)
  ) %>% 
  mutate(votos_validos = sum(apruebo, rechazo),
         votos_totales = sum(votos_validos, nulo, blanco)#,
         #participacion = vot_tot/padron
  )
