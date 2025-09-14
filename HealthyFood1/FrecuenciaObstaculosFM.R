ggplot(df, aes(x = Obstaculos, fill = Sexo)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_dodge(width = 0.9),
            hjust = -0.2,   # ajusta la posición del texto (afuera de la barra)
            size = 3.5) +
  coord_flip() +
  labs(title = "Obstáculos para comer saludable: Hombres vs Mujeres",
       x = "Obstáculo", y = "Número de personas", fill = "Sexo") +
  theme_minimal()
