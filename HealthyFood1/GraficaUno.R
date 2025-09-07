library(readxl)
library(tidyverse)
library(moments)

df <- read_excel("Opiniones_corregido_numeros_puros.xlsx")


# Revisar nombres de columna (porque R renombra columnas duplicadas)
names(df)

# Conversión de columnas a numéricas (ajustado para nombre con puntos)
df <- df %>%
  mutate(
    Edad = as.numeric(Edad),
    Frecuencia_Consumo = as.numeric(Frecuencia_Consumo),
    Horas_Contenido_Nutricion = as.numeric(Horas_Contenido_Nutricion),
    Disposicion_Pago_Num = as.numeric(Disposicion_Pago_Num)  # Usa esta columna
  )

ggplot(df, aes(x = factor(Disposicion_Pago_Num), fill = Sexo)) +
  geom_bar(position = "dodge") +
  labs(title = "Disposición a Pagar (%) por Sexo",
       x = "Porcentaje adicional",
       y = "Frecuencia",
       fill = "Sexo") +
  theme_minimal()