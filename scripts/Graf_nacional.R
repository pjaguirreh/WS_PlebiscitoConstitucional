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

resultados_nacional[1, 2] <- 1500
resultados_nacional[2, 2] <- 1500

graf_nacional <- resultados_nacional %>% 
  mutate(porcentaje = round((votos/sum(votos)*100), 1)) %>% 
  bind_rows(datos_2v2021_nac) %>% 
  mutate(cat = case_when(
    tipo %in% c("Apruebo", "Rechazo") ~ "1",
    TRUE ~ "0"
  )) %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("Apruebo", "Rechazo",
                                    "Boric", "Kast"))) %>% 
  ggplot(aes(x = tipo, y = porcentaje, 
             label = paste0(porcentaje, "% \n(", 
                            prettyNum(votos, big.mark = "."),")"),
             fill = cat)) +
  geom_col() +
  geom_label(nudge_y = 10, fill = NA) +
  labs(x = NULL, y = NULL,
       title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       caption = "Elaborado por Pablo Aguirre Hörmann (@PAguirreH - https://github.com/pjaguirreh)") +
  mi_tema +
  theme(axis.text.y.left = element_blank(),
        axis.text.x = element_text(size = 15),
        panel.grid.major = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("1" = "#619CFF", 
                               "0" = "light grey")) +
  ylim(c(0,100))

