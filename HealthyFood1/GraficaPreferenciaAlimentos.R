# TABLA DE FRECUENCIAS
tab_pref <- df %>%
  count(Preferencias_Alimentos, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_pref

# BARRAS
tab_pref <- tab_pref %>%
  mutate(Preferencias_Alimentos = fct_reorder(Preferencias_Alimentos, Frecuencia, .desc = TRUE))

ggplot(tab_pref, aes(x = Preferencias_Alimentos, y = Frecuencia)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            hjust = -0.1, size = 3) +
  labs(title = "Preferencias de Alimentos",
       x = "Preferencia", y = "Frecuencia") +
  coord_flip() +
  theme_minimal()

# PASTEL
ggplot(tab_pref, aes(x = "", y = Frecuencia, fill = Preferencias_Alimentos)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Preferencias de Alimentos (Gráfico de pastel)",
       x = NULL, y = NULL, fill = "Preferencia") +
  theme_void()
