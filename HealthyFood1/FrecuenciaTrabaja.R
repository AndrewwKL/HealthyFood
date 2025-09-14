# TABLA
tab_trabaja <- df %>%
  count(Trabaja, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_trabaja

# BARRAS
tab_trabaja <- tab_trabaja %>%
  mutate(Trabaja = fct_reorder(Trabaja, Frecuencia, .desc = TRUE))

ggplot(tab_trabaja, aes(x = Trabaja, y = Frecuencia)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            vjust = -0.4, size = 4) +
  labs(title = "Distribución de Trabaja",
       x = "¿Trabaja?", y = "Frecuencia") +
  theme_minimal()

# PASTEL
ggplot(tab_trabaja, aes(x = "", y = Frecuencia, fill = Trabaja)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "¿Trabaja? (Gráfico de pastel)", x = NULL, y = NULL, fill = "Trabaja") +
  theme_void()
