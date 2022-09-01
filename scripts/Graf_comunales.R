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
  rowwise() %>% 
  mutate(votos = sample(1:10, 1)) %>% 
  group_by(region, comuna) %>% 
  mutate(porcentaje = round(votos/sum(votos)*100, 1))

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

graf_comunal <- a %>% 
  ggplot(aes(x = Boric, y = Apruebo)) +
  geom_abline(intercept = 0, slope = 1, color = "blue",
              size = 1.5, 
              #linetype = 2,
              alpha= 0.5
              ) +
  geom_point(size = 2, alpha = 0.8, col = "light grey") +
  geom_point(data = filter(a, destacar == "1"), size = 2, color = "red") +
  ylim(c(5, 95)) +
  xlim(c(5, 95)) +
  mi_tema +
  labs(title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('En rojo se destacan las 10 comunas con mayor población en Chile. Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       x = "Votación (%) Boric en 2da Vuelta",
       y = "Votación (%) Apruebo 04/05",
       caption = "Elaborado por Pablo Aguirre Hörmann (@PAguirreH - https://github.com/pjaguirreh)") +
  annotate("text", x = 19, y = 95, 
                     label = c("Comunas a este lado de la curva están votando en mayor medida\npor el Apruebo al comparar con su votación por Boric"),
                     size = 3) +
  annotate("text", x = 81, y = 5, 
                     label = c("Comunas a este lado de la curva están votando en menor medida\npor el Apruebo al comparar con su votación por Boric"),
                     size = 3) +
  theme(legend.position = "none",
        plot.caption = element_text(color = "grey70", size = 13))

