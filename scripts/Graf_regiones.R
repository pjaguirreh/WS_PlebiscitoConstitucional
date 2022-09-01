graf_regional <- resultados_region %>% 
  select(-votos_validos, -votos_totales, -cod_reg, -nulo, -blanco) %>% 
  pivot_longer(2:3, names_to = "tipo", values_to = "votos") %>% 
  mutate(tipo = case_when(
    tipo == "apruebo" ~ "Apruebo",
    tipo == "rechazo" ~ "Rechazo"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo"))) 

graf_regional$votos <- rep(c(1500, 1500), 16)

graf_regional <- graf_regional %>% 
  group_by(region) %>% 
  mutate(porcentaje = (votos/sum(votos)*100)) %>% 
  bind_rows(datos_2v2021_reg) %>% 
  mutate(cat = case_when(
    tipo %in% c("Apruebo", "Rechazo") ~ "1",
    TRUE ~ "0"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo",
                                    "Boric", "Kast")))  %>% 
  ggplot(aes(x = tipo, y = porcentaje, label = paste0(porcentaje, "% \n(",
                                                      prettyNum(votos, big.mark = "."),")"),
             fill = cat)) +
  geom_col() +
  geom_label(size = 4, nudge_y = 18, fill = NA) +
  labs(x = NULL, y = NULL,
       title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       caption = "Elaborado por Pablo Aguirre Hormann (@PAguirreH - https://github.com/pjaguirreh)") +
  mi_tema +
  theme(axis.text.y.left = element_blank()) +
  facet_wrap(vars(region), scales = "free_x") +
  theme(axis.text.y.left = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        plot.caption = element_text(color = "grey70", size = 18)) +
  scale_fill_manual(values = c("1" = "#619CFF", 
                               "0" = "light grey")) +
  ylim(c(0,100))
