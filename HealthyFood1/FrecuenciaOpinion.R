# TABLA DE FRECUENCIAS (orden Likert)
niveles_likert <- c("Totalmente de acuerdo", "De acuerdo", "Neutral",
                    "En desacuerdo", "Totalmente en desacuerdo")

tab_opinion <- df %>%
  mutate(Opinion_Afirmacion = factor(Opinion_Afirmacion, levels = niveles_likert)) %>%
  count(Opinion_Afirmacion, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  drop_na(Opinion_Afirmacion) %>%         # por si no existen todas las categorías
  arrange(Opinion_Afirmacion)

tab_opinion

# BARRAS HORIZONTALES (Likert ordenado)
ggplot(tab_opinion, aes(x = Opinion_Afirmacion, y = Frecuencia)) +
  geom_col() +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            hjust = -0.1, size = 3) +
  labs(title = "Opinión frente a la afirmación",
       x = "Respuesta (Likert)", y = "Frecuencia") +
  coord_flip() +
  theme_minimal()
