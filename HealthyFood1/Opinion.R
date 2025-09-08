ggplot(df, aes(x = Opinion_Afirmacion, fill = Sexo)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_dodge(width = 0.9),
            vjust = -0.3, 
            size = 3.5) +
  labs(title = "Opinión sobre la afirmación: Hombres vs Mujeres",
       x = "Nivel de acuerdo", y = "Número de personas", fill = "Sexo") +
  theme_minimal()
