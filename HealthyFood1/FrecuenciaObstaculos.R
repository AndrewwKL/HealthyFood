# TABLA DE FRECUENCIAS
tab_obstaculos <- df %>%
  count(Obstaculos, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_obstaculos

# BARRAS
tab_obstaculos <- tab_obstaculos %>%
  mutate(Obstaculos = fct_reorder(Obstaculos, Frecuencia, .desc = TRUE))

ggplot(tab_obstaculos, aes(x = Obstaculos, y = Frecuencia)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            hjust = -0.1, size = 3) +
  labs(title = "Obstáculos para una Alimentación Saludable",
       x = "Obstáculo", y = "Frecuencia") +
  coord_flip() +
  theme_minimal()
