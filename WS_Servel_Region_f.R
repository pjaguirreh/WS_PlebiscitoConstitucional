lista_regiones <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/regiones/all.json")

sapply(lista_regiones, "[[", 1)

scrap_reg <- function(x){
  # datos de votación
  detalle_regiones <- fromJSON(file = paste0("https://servelelecciones.cl/data/elecciones_constitucion/computo/regiones/",
                                             x,
                                             ".json"))
  data.frame(
    reg_cod = x,
    apruebo = as.numeric(str_remove_all(detalle_regiones[[4]][[1]]$c, "\\.")),
    rechazo = as.numeric(str_remove_all(detalle_regiones[[4]][[2]]$c, "\\.")),
    nulo = as.numeric(str_remove_all(detalle_regiones[[5]][[2]]$c, "\\.")),
    blanco = as.numeric(str_remove_all(detalle_regiones[[5]][[3]]$c, "\\.")))
}

a <- purrr::map(sapply(lista_regiones, "[[", 1), scrap_reg)
bind_rows(a)