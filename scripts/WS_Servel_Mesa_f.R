diccionario_mesa_a_reg <- read_csv("planillas_aux/Diccionario_mesa_a_reg.csv")

## TRAER RESULTADOS POR MESA
resultados_mesas <- function(x){
  
  detalle_mesa <- fromJSON(file = glue("https://www.servelelecciones.cl/data/elecciones_constitucion/computomesas/{x}.json"))
  
  data.frame(
    cod_mesa = x,
    apruebo = detalle_mesa[[4]][[1]]$c,
    rechazo = detalle_mesa[[4]][[2]]$c,
    nulo = detalle_mesa[[5]][[2]]$c,
    blanco = detalle_mesa[[5]][[3]]$c
  )
  
}

tic()
resultados_desagregados <- map(diccionario_mesa_a_reg$cod_mesa, resultados_mesas) %>% bind_rows()
toc()