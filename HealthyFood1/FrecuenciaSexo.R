library(readxl)
library(tidyverse)   # incluye ggplot2

# Ruta relativa dentro del repo
df <- read_excel("Opiniones_corregido_numeros_puros.xlsx")

tab_sexo <- df %>%
  count(Sexo, name = "Frecuencia") %>%
  mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
  arrange(desc(Frecuencia))

tab_sexo

# Ordenar factor por frecuencia para que se vea más claro
tab_sexo <- tab_sexo %>%
  mutate(Sexo = fct_reorder(Sexo, Frecuencia, .desc = TRUE))

ggplot(tab_sexo, aes(x = Sexo, y = Frecuencia)) +
  geom_col() +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            vjust = -0.4, size = 4) +
  labs(title = "",
       x = "Sexo", y = "Frecuencia") +
  theme_minimal()

ggplot(tab_sexo, aes(x = "", y = Frecuencia, fill = Sexo)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Sexo (Gráfico de pastel)", x = NULL, y = NULL, fill = "Sexo") +
  theme_void()

