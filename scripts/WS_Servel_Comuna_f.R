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

###################################

write_csv(bind_rows(resultados_comuna, 
                    select(mutate(resultados_paises_detalle, region = "Extranjero", comuna = pais, .before = 1), -pais)), 
          glue(
            "parciales/ResultadoParcial_{str_replace_all(str_replace_all(str_replace_all(str_sub(Sys.time(), 1, 19), '-', '_'), ':', '_'), ' ', '_')}.csv"
            ))

write_csv(bind_rows(resultados_comuna, 
                    select(mutate(resultados_paises_detalle, region = "Extranjero", comuna = pais, .before = 1), -pais)), 
          "parciales/ultimo.csv")

