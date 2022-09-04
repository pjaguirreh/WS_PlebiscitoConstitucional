graf_comunal <- a %>% 
  left_join(votos_validos_com) %>% 
  ggplot(aes(x = Boric, y = Apruebo, size = votos_validos)) +
  geom_abline(intercept = 0, slope = 1, color = "blue",
              size = 1.5, 
              #linetype = 2,
              alpha= 0.5
              ) +
  geom_point(alpha = 0.8, col = "light grey") +
  #geom_point(size = 2, alpha = 0.8, col = "light grey") +
  #geom_point(data = filter(a, destacar == "1"), size = 2, color = "red") +
  ylim(c(5, 95)) +
  xlim(c(5, 95)) +
  mi_tema1 +
  labs(title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('El tamaño de los puntos refleja el N° de votos válidamente emitidos. Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       x = "Votación (%) Boric en 2da Vuelta 2021",
       y = "Votación (%) Apruebo 04/05",
       caption = "Elaborado por Pablo Aguirre Hörmann (@PAguirreH - https://github.com/pjaguirreh)") +
  annotate("text", x = 25.5, y = 95, 
                     label = c("Comunas a este lado de la curva están votando en MAYOR medida\npor el Apruebo de lo que votaron por Boric en 2V"),
                     size = 4) +
  annotate("text", x = 74.5, y = 5, 
                     label = c("Comunas a este lado de la curva están votando en MENOR medida\npor el Apruebo de lo que votaron por Boric en 2v"),
                     size = 4) +
  theme(legend.position = "none",
        plot.caption = element_text(color = "grey70", size = 13))



graf_comunal2 <- a %>% 
  left_join(votos_validos_com) %>% 
  ggplot(aes(x = Boric, y = Apruebo, size = votos_validos)) +
  geom_vline(xintercept = 50, color = "blue",
              size = 1.5, 
              alpha= 0.5) +
  geom_hline(yintercept = 50, color = "blue",
             size = 1.5, 
             alpha= 0.5) +
  geom_point(alpha = 0.8, col = "light grey") +
  #geom_point(size = 2, alpha = 0.8, col = "light grey") +
  #geom_point(data = filter(a, destacar == "1"), size = 2, color = "red") +
  ylim(c(5, 95)) +
  xlim(c(5, 95)) +
  mi_tema1 +
  labs(title = "Resultados parciales plebiscito nacional propuesta constitucional",
       subtitle = glue('El tamaño de los puntos refleja el N° de votos válidamente emitidos. Actualizado a las {str_sub(Sys.time(), 12,16)}'),
       x = "Votación (%) Boric en 2da Vuelta 2021",
       y = "Votación (%) Apruebo 04/05",
       caption = "Elaborado por Pablo Aguirre Hörmann (@PAguirreH - https://github.com/pjaguirreh)") +
  annotate("text", x = 20, y = 95, 
           label = c("Comunas donde ganó Kast y va ganando Apruebo"),
           size = 4) +
  annotate("text", x = 80, y = 5, 
           label = c("Comunas donde ganó Boric y va ganando Rechazo"),
           size = 4) +
  annotate("text", x = 20, y = 5, 
           label = c("Comunas donde ganó Kast y va ganando Rechazo"),
           size = 4) +
  annotate("text", x = 80, y = 95, 
           label = c("Comunas donde ganó Boric y va ganando Apruebo"),
           size = 4)  +
  theme(legend.position = "none",
        plot.caption = element_text(color = "grey70", size = 13))
