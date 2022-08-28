library(ggplot2)
library(tidyr)
library(forcats)
library(lubridate)
library(glue)

mi_tema <- theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.7)),
        plot.subtitle = element_text(face = "plain", size = rel(1.3)),
        plot.caption = element_text(color = "grey70"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1)) +
  theme(strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA))

resultados_nacional <- datos_votaciones_plebiscito %>% 
  summarise(
    vot_apruebo = sum(vot_apruebo),
    vot_rechazo = sum(vot_rechazo),
    vot_nulo = sum(vot_nulo),
    vot_blanco = sum(vot_blanco),
    ) %>% 
  pivot_longer(1:4, names_to = "tipo", values_to = "votos") %>% 
  mutate(tipo = as.factor(tipo),
         tipo = fct_relevel(tipo, c("vot_apruebo", "vot_rechazo", "vot_nulo", "vot_blanco"))) %>% 
  mutate(tipo = case_when(
    tipo == "vot_apruebo" ~ "Apruebo",
    tipo == "vot_rechazo" ~ "Rechazo",
    tipo == "vot_nulo" ~ "Nulos",
    tipo == "vot_blanco" ~ "Blancos"
  ))

resultados_nacional %>% 
  ggplot(aes(x = tipo, y = votos, label = votos)) +
  geom_col() +
  geom_label() +
  labs(x = NULL, y = NULL,
       title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       caption = "Elaborado por Pablo Aguirre Hörmann (@PAguirreH - https://github.com/pjaguirreh)") +
  mi_tema +
  theme(axis.text.y.left = element_blank())
    

