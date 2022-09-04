computos <- read_csv("parciales/computos.csv")

ultimo_computo <- read_csv("parciales/ultimo.csv") %>% 
  summarise(
    apruebo = sum(apruebo, na.rm = TRUE),
    rechazo = sum(rechazo, na.rm = TRUE)
  ) %>% 
  mutate(fecha_hora = Sys.time(), .before = 1)

computos_act <- bind_rows(computos, ultimo_computo)

write_csv(computos_act, "parciales/computos.csv")

graf_evolucion <- computos_act %>% 
  rowwise() %>% 
  mutate(apruebo_por = apruebo/(apruebo+rechazo),
         rechazo_por = rechazo/(apruebo+rechazo)) %>% 
  ungroup() %>% 
  mutate(computo = row_number(),
         computo_nombre = glue("{hour(fecha_hora)}:{minute(fecha_hora)}")) %>% 
  select(-apruebo, -rechazo) %>% 
  pivot_longer(2:3, names_to = "opcion", values_to = "porcentaje") %>% 
  mutate(opcion = case_when(
    opcion == "apruebo_por"~ "Apruebo",
    opcion == "rechazo_por" ~ "Rechazo"
  ),
  opcion = as.factor(opcion)) %>% 
  ggplot(aes(x = reorder(computo_nombre, computo), y = porcentaje, 
             label = glue("{round(porcentaje*100, 1)}%"),
             group = opcion)) +
  geom_line(aes(linetype = opcion), size = 1, col = "#619CFF") +
  geom_label(size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ylim(c(0,1))
