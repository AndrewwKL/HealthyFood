# TABLA DE FRECUENCIAS
tab_factores <- df %>%
  count(Factores_Influencia, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_factores

# BARRAS
tab_factores <- tab_factores %>%
  mutate(Factores_Influencia = fct_reorder(Factores_Influencia, Frecuencia, .desc = TRUE))

ggplot(tab_factores, aes(x = Factores_Influencia, y = Frecuencia)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            hjust = -0.1, size = 3) +
  labs(title = "Factores de Influencia en la Alimentación",
       x = "Factor de influencia", y = "Frecuencia") +
  coord_flip() +
  theme_minimal()
