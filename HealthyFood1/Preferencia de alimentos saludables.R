ggplot(df, aes(x = Preferencias_Alimentos, fill = Trabaja)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_dodge(width = 0.9),
            hjust = -0.2,  # como usamos coord_flip, se ajusta horizontalmente
            size = 3.5) +
  coord_flip() +
  labs(title = "Preferencias de alimentos saludables: Trabaja vs No",
       x = "Preferencia", y = "Número de personas", fill = "¿Trabaja?") +
  theme_minimal()
