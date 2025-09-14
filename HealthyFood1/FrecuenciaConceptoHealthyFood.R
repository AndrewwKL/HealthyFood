# TABLA DE FRECUENCIAS
tab_concepto <- df %>%
  count(Concepto_Comida_Saludable, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_concepto

# BARRAS
tab_concepto <- tab_concepto %>%
  mutate(Concepto_Comida_Saludable = fct_reorder(Concepto_Comida_Saludable, Frecuencia, .desc = TRUE))

ggplot(tab_concepto, aes(x = Concepto_Comida_Saludable, y = Frecuencia)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            hjust = -0.1, size = 3) +
  labs(title = "Concepto de Comida Saludable",
       x = "Concepto", y = "Frecuencia") +
  coord_flip() +   # gira el gráfico para que se lean mejor las categorías
  theme_minimal()
