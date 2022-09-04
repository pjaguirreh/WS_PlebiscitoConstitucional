### DATOS NIVEL NACIONAL

resultados_nacional <- resultados_region %>% 
  summarise(
    apruebo = sum(apruebo),
    rechazo = sum(rechazo)
  ) %>% 
  pivot_longer(1:2, names_to = "tipo", values_to = "votos") %>% 
  mutate(tipo = case_when(
    tipo == "apruebo" ~ "Apruebo",
    tipo == "rechazo" ~ "Rechazo"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo")))

###################BORRAR########################
if(sum(resultados_nacional$votos) == 0){
resultados_nacional[1, 2] <- 1
resultados_nacional[2, 2] <- 1
}
###################BORRAR########################

## DATOS EXTRANJERO

resultados_paises <- resultados_paises %>% 
  select(1:2) %>% 
  pivot_longer(1:2, names_to = "tipo", values_to = "votos") %>% 
  mutate(tipo = case_when(
    tipo == "apruebo" ~ "Apruebo",
    tipo == "rechazo" ~ "Rechazo"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo")))

###################BORRAR########################
if(sum(resultados_paises$votos) == 0){
resultados_paises[1, 2] <- 1
resultados_paises[2, 2] <- 1
}
###################BORRAR########################

## DATOS GENERALES

resultados_todo <- bind_rows(resultados_paises, resultados_nacional) %>% 
  group_by(tipo) %>% 
  summarise(votos = sum(votos, na.rm = TRUE))

## DATOS REGIONES

resultados_region <- resultados_region %>% 
  select(-votos_validos, -votos_totales, -cod_reg, -nulo, -blanco) %>% 
  pivot_longer(2:3, names_to = "tipo", values_to = "votos") %>% 
  mutate(tipo = case_when(
    tipo == "apruebo" ~ "Apruebo",
    tipo == "rechazo" ~ "Rechazo"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo"))) 

###################BORRAR########################
if(sum(resultados_region$votos) == 0){
  resultados_region$votos <- rep(c(1, 1), 16)
}
###################BORRAR########################


## DATOS COMUNALES

votos_validos_com <- resultados_comuna %>% 
  distinct(comuna, votos_validos)

###################BORRAR########################
if(sum(votos_validos_com$votos_validos) == 0){
  votos_validos_com$votos_validos <- rpois(346, 3)
  
  com_plebiscito <- resultados_comuna %>% 
    select(region, comuna, apruebo, rechazo) %>% 
    pivot_longer(3:4, names_to = "tipo", values_to = "votos") %>% 
    mutate(tipo = case_when(
      tipo == "apruebo" ~ "Apruebo",
      tipo == "rechazo" ~ "Rechazo"
    )) %>% 
    mutate(tipo = as.factor(tipo),
           tipo = fct_relevel(tipo, c("Apruebo", "Rechazo"))) %>% 
    ungroup() %>% 
    ###################BORRAR########################
  rowwise(.) %>% 
    mutate(votos = sample(1:10, 1)) %>% 
    ###################BORRAR########################
  group_by(region, comuna) %>% 
    mutate(porcentaje = round(votos/sum(votos)*100, 1))
  ###################BORRAR########################
} else{
  
  com_plebiscito <- resultados_comuna %>% 
    select(region, comuna, apruebo, rechazo) %>% 
    pivot_longer(3:4, names_to = "tipo", values_to = "votos") %>% 
    mutate(tipo = case_when(
      tipo == "apruebo" ~ "Apruebo",
      tipo == "rechazo" ~ "Rechazo"
    )) %>% 
    mutate(tipo = as.factor(tipo),
           tipo = fct_relevel(tipo, c("Apruebo", "Rechazo"))) %>% 
    ungroup() %>% 
    group_by(region, comuna) %>% 
    mutate(porcentaje = round(votos/sum(votos)*100, 1))
  
}


com_2v <- datos_2v2021 %>% 
  filter(Candidato %in% c("GABRIEL BORIC FONT", "JOSE ANTONIO KAST RIST")) %>% 
  mutate(Candidato = case_when(
    str_detect(Candidato, "BORIC") ~ "Boric",
    TRUE ~ "Kast"
  )) %>% 
  group_by(region = `Región`, comuna = Comuna, tipo = Candidato) %>% 
  summarise(votos = sum(`Votos TRICEL`, na.rm = TRUE)) %>% 
  mutate(porcentaje = round(votos/sum(votos)*100, 1)) %>% 
  mutate(region = str_remove_all(region, "DE "),
         region = str_remove_all(region, "DEL ")) %>% 
  ungroup()


a <- bind_rows(com_2v, com_plebiscito) %>% 
  mutate(destacar = case_when(
    comuna %in% c("PUENTE ALTO", "MAIPU", "LA FLORIDA", "ANTOFAGASTA",
                  "VIÑA DEL MAR", "VALPARAISO", "SAN BERNARDO", "LAS CONDES",
                  "TEMUCO", "SANTIAGO") ~ "1",
    TRUE ~ "0"
  )) %>% 
  filter(tipo %in% c("Boric", "Apruebo")) %>% 
  select(-votos) %>% 
  pivot_wider(names_from = tipo, values_from = porcentaje) 