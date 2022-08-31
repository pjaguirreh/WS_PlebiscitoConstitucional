# REGIONES
lista_regiones <- fromJSON(file = "https://www.servelelecciones.cl/data/elecciones_constitucion/filters/regiones/all.json")

diccionario_reg <- data.frame(cod_reg = sapply(lista_regiones, "[[", 1),
                              region = sapply(lista_regiones, "[[", 2))

## COMUNAS POR REGIÓN
obtener_comuna_reg <- function(x){
  
  lista_comunas <- fromJSON(file = glue("https://www.servelelecciones.cl/data/elecciones_constitucion/filters/comunas/byregion/{x}.json"))
  
  data.frame(cod_reg = x,
             cod_com = sapply(lista_comunas, "[[", 1),
             comuna = sapply(lista_comunas, "[[", 2))
  
}

diccionario_com <- map(diccionario_reg$cod_reg, obtener_comuna_reg) %>% 
  bind_rows()

## CIRCUNSCRIPCIONES POR COMUNA
obtener_circ_com <- function(x){
  
  lista_circ <- fromJSON(file = glue("https://www.servelelecciones.cl/data/elecciones_constitucion/filters/circ_electoral/bycomuna/{x}.json"))
  
  data.frame(cod_com = x,
             cod_circ = sapply(lista_circ, "[[", 1),
             circunscripcion = sapply(lista_circ, "[[", 2))
  
}

diccionario_circ <- map(diccionario_com$cod_com, obtener_circ_com) %>% 
  bind_rows()

## LOCALES POR CIRCUNSCRIPCION
obtener_loc_circ <- function(x){
  
  lista_loc <- fromJSON(file = glue("https://www.servelelecciones.cl/data/elecciones_constitucion/filters/locales/bycirc_electoral/{x}.json"))
  
  data.frame(cod_circ = x,
             cod_loc = sapply(lista_loc, "[[", 1),
             local = sapply(lista_loc, "[[", 2))
  
}

diccionario_loc <- map(diccionario_circ$cod_circ, obtener_loc_circ) %>% 
  bind_rows()

## MESAS POR LOCALES
obtener_mesas_loc <- function(x){
  
  lista_mesas <- fromJSON(file = glue("https://www.servelelecciones.cl/data/elecciones_constitucion/filters/mesas/bylocales/{x}.json"))
  
  data.frame(cod_loc = x,
             cod_mesa = sapply(lista_mesas, "[[", 1),
             mesa = sapply(lista_mesas, "[[", 2))
  
}

diccionario_mesas <- map(diccionario_loc$cod_loc, obtener_mesas_loc) %>% 
  bind_rows()

## DICCIONARIO

diccionario_mesa_a_reg <- diccionario_reg %>% 
  left_join(diccionario_com, by = "cod_reg") %>% 
  left_join(diccionario_circ, by = "cod_com") %>% 
  left_join(diccionario_loc, by = "cod_circ") %>% 
  left_join(diccionario_mesas, by = "cod_loc") %>% 
  as_tibble()

write_csv(diccionario_mesa_a_reg, "planillas_aux/Diccionario_mesa_a_reg.csv")