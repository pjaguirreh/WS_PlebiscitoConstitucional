datos_2v2021 <- read_rds("planillas_aux/datos_2v2021.rds")

datos_2v2021_total <- read_rds("planillas_aux/datos_2v2021_nac.rds")

datos_2v2021_ext <- datos_2v2021_total %>% 
  filter(Candidato %in% c("GABRIEL BORIC FONT", "JOSE ANTONIO KAST RIST")) %>% 
  select(Candidato, votos = Extranjero) %>% 
  mutate(porcentaje = round((votos/sum(votos))*100, 1)) %>% 
  mutate(Candidato = case_when(
    str_detect(Candidato, "BORIC") ~ "Boric",
    TRUE ~ "Kast"
  )) %>% 
  rename(tipo = Candidato)

datos_2v2021_nac <- datos_2v2021_total %>% 
  filter(Candidato %in% c("GABRIEL BORIC FONT", "JOSE ANTONIO KAST RIST")) %>% 
  select(Candidato, votos = Chile) %>% 
  mutate(porcentaje = round((votos/sum(votos))*100, 1)) %>% 
  mutate(Candidato = case_when(
    str_detect(Candidato, "BORIC") ~ "Boric",
    TRUE ~ "Kast"
  )) %>% 
  rename(tipo = Candidato)

datos_2v2021_total <- datos_2v2021_total %>% 
  filter(Candidato %in% c("GABRIEL BORIC FONT", "JOSE ANTONIO KAST RIST")) %>% 
  select(Candidato, votos = Total) %>% 
  mutate(porcentaje = round((votos/sum(votos))*100, 1)) %>% 
  mutate(Candidato = case_when(
    str_detect(Candidato, "BORIC") ~ "Boric",
    TRUE ~ "Kast"
  )) %>% 
  rename(tipo = Candidato)

datos_2v2021_reg <- datos_2v2021 %>% 
  filter(Candidato %in% c("GABRIEL BORIC FONT", "JOSE ANTONIO KAST RIST")) %>% 
  group_by(region = `Región`, tipo = Candidato) %>% 
  summarise(votos = sum(`Votos TRICEL`, na.rm = TRUE)) %>% 
  mutate(porcentaje = (round(votos/sum(votos)*100, 1))) %>% 
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
  select(-orden_reg) %>% 
  mutate(tipo = case_when(
    str_detect(tipo, "BORIC") ~ "Boric",
    TRUE ~ "Kast"
  )) 
