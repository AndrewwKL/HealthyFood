library(ggplot2)


ggplot(df, aes(x = factor(Disposicion_Pago_Num), fill = Sexo)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +
  labs(title = "Disposición a Pagar (%) por Sexo",
       x = "Porcentaje adicional",
       y = "Frecuencia",
       fill = "Sexo") +
  theme_minimal()
