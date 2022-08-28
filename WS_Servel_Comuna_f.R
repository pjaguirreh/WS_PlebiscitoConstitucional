lista_comunas <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/comunas/all.json")

scrap_comunas <- function(x){
  # datos de votación
  detalle_comunas <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/comunas/",
                                             x,
                                             ".json"))
  data.frame(
    reg_cod = x,
    apruebo = as.numeric(str_remove_all(detalle_comunas[[4]][[1]]$c, "\\.")),
    rechazo = as.numeric(str_remove_all(detalle_comunas[[4]][[2]]$c, "\\.")),
    nulo = as.numeric(str_remove_all(detalle_comunas[[5]][[2]]$c, "\\.")),
    blanco = as.numeric(str_remove_all(detalle_comunas[[5]][[3]]$c, "\\.")))
}

b <- purrr::map(sapply(lista_comunas, "[[", 1), scrap_comunas)
bind_rows(b)
