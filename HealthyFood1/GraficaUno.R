# Leer la base de datos
df <- Opiniones_sobre_la_comida_saludable_1_104_

# Cargar librerías
library(ggplot2)
library(e1071)  # Para calcular sesgo y curtosis

# Asegúrate de que tu columna de interés esté en formato numérico
df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)` <- as.numeric(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)

# --- Cálculos estadísticos ---
# Cálculo de cuartiles y otras métricas estadísticas
summary_stats <- summary(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)
Q1 <- summary_stats["1st Qu."]
Q3 <- summary_stats["3rd Qu."]
median_val <- summary_stats["Median"]
min_val <- summary_stats["Min."]
max_val <- summary_stats["Max."]
skewness_val <- skewness(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)
kurtosis_val <- kurtosis(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)

# Determinación de sesgo y curtosis
sesgo <- ifelse(skewness_val > 0, "Sesgo positivo", "Sesgo negativo")
curtosis <- ifelse(kurtosis_val > 3, "Leptocúrtica", "Platicúrtica")

# Mostrar las métricas estadísticas
cat("Cuartil 1 (Q1):", Q1, "\n")
cat("Cuartil 3 (Q3):", Q3, "\n")
cat("Mediana:", median_val, "\n")
cat("Sesgo:", sesgo, "\n")
cat("Curtosis:", curtosis, "\n")

# --- Generación de boxplot y densidad ---
windows(width=12, height=6)

p <- ggplot(df, aes(x = `¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)) +
  # Boxplot (con cuartiles)
  geom_boxplot(aes(fill = as.factor(`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)), 
               alpha = 0.3, outlier.colour = "red", outlier.size = 2) +
  
  # Curvas de densidad sobre el boxplot
  geom_density(aes(color = as.factor(`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)), 
               linewidth = 1) +
  
  # Agregar cuartiles, mediana y otros valores
  geom_vline(xintercept = Q1, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = median_val, color = "green", linetype = "dashed") +
  geom_vline(xintercept = Q3, color = "blue", linetype = "dashed") +
  
  scale_fill_manual(values=c("blue", "green", "orange", "red", "purple", "yellow", "pink", "brown")) +
  scale_color_manual(values=c("blue", "green", "orange", "red", "purple", "yellow", "pink", "brown")) +
  
  labs(title="Distribución de consumo semanal de comida saludable",
       x="Veces a la semana", y="Densidad", color=NULL, fill=NULL) +
  theme_minimal(base_size=12) +
  theme(legend.position="top", panel.grid.minor=element_blank())

# Imprimir el gráfico
print(p)




# Cargar librerías
library(ggplot2)
library(gridExtra)

# Asegúrate de que las columnas de interés estén en formato numérico
df$`¿Cuantos años tienes?` <- as.numeric(df$`¿Cuantos años tienes?`)
df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)` <- as.numeric(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)
df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?` <- as.numeric(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`)

# Calcular los cuartiles y la mediana para cada columna numérica
stats_edad <- summary(df$`¿Cuantos años tienes?`)
stats_consumo <- summary(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)
stats_contenido <- summary(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`)

# Crear el boxplot para cada una de las columnas numéricas
windows(width = 12, height = 6)

# Boxplot para la columna "¿Cuantos años tienes?"
p1 <- ggplot(df, aes(x = as.factor(1), y = `¿Cuantos años tienes?`)) +
  geom_boxplot(fill = "skyblue", outlier.colour = "red", outlier.size = 2) +
  geom_text(aes(x = 1, y = stats_edad["1st Qu."], label = paste("Q1:", round(stats_edad["1st Qu."], 2))), vjust = -1) +
  geom_text(aes(x = 1, y = stats_edad["Median"], label = paste("Mediana:", round(stats_edad["Median"], 2))), vjust = -1.5) +
  geom_text(aes(x = 1, y = stats_edad["3rd Qu."], label = paste("Q3:", round(stats_edad["3rd Qu."], 2))), vjust = -2) +
  labs(title = "Distribución de edades", x = "", y = "Edad") +
  theme_minimal()

# Boxplot para la columna "¿Cuántas veces a la semana consumes comida saludable?"
p2 <- ggplot(df, aes(x = as.factor(1), y = `¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)) +
  geom_boxplot(fill = "lightgreen", outlier.colour = "red", outlier.size = 2) +
  geom_text(aes(x = 1, y = stats_consumo["1st Qu."], label = paste("Q1:", round(stats_consumo["1st Qu."], 2))), vjust = -1) +
  geom_text(aes(x = 1, y = stats_consumo["Median"], label = paste("Mediana:", round(stats_consumo["Median"], 2))), vjust = -1.5) +
  geom_text(aes(x = 1, y = stats_consumo["3rd Qu."], label = paste("Q3:", round(stats_consumo["3rd Qu."], 2))), vjust = -2) +
  labs(title = "Distribución de consumo semanal", x = "", y ="") +
  theme_minimal()

# Boxplot para la columna "¿Cuántas horas a la semana consumes contenido sobre nutrición?"
p3 <- ggplot(df, aes(x = as.factor(1), y = `¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`)) +
  geom_boxplot(fill = "lightcoral", outlier.colour = "red", outlier.size = 2) +
  geom_text(aes(x = 1, y = stats_contenido["1st Qu."], label = paste("Q1:", round(stats_contenido["1st Qu."], 2))), vjust = -1) +
  geom_text(aes(x = 1, y = stats_contenido["Median"], label = paste("Mediana:", round(stats_contenido["Median"], 2))), vjust = -1.5) +
  geom_text(aes(x = 1, y = stats_contenido["3rd Qu."], label = paste("Q3:", round(stats_contenido["3rd Qu."], 2))), vjust = -2) +
  labs(title = "Distribución de horas de contenido", x = "", y = "") +
  theme_minimal()


# Mostrar los gráficos
grid.arrange(p1, p2, p3,ncol = 3)





# Cargar librerías
library(ggplot2)
library(e1071)  # Para calcular sesgo y curtosis
library(dplyr)  # Para calcular la moda

# Asegúrate de que la columna de interés esté en formato numérico
df$`¿Cuantos años tienes?` <- as.numeric(df$`¿Cuantos años tienes?`)

# Calcular el sesgo (skewness) y curtosis (kurtosis)
skew_edad <- skewness(df$`¿Cuantos años tienes?`, na.rm = TRUE)
kurt_edad <- kurtosis(df$`¿Cuantos años tienes?`, na.rm = TRUE)

# Calcular media, mediana y moda
mean_edad <- mean(df$`¿Cuantos años tienes?`, na.rm = TRUE)
median_edad <- median(df$`¿Cuantos años tienes?`, na.rm = TRUE)
mode_edad <- as.numeric(names(sort(table(df$`¿Cuantos años tienes?`), decreasing = TRUE))[1])

# Imprimir resultados
cat("Sesgo (Skewness) de las edades:", skew_edad, "\n")
cat("Curtosis (Kurtosis) de las edades:", round(kurt_edad, 2), "\n")

# Crear el histograma con la línea de densidad
windows(width = 12, height = 6)

ggplot(df, aes(x = `¿Cuantos años tienes?`)) +
  # Histograma
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Curva de densidad
  geom_density(color = "red", size = 1) +
  
  # Añadir el valor de la curtosis al gráfico
  annotate("text", x = 30, y = 0.05, label = paste("Curtosis:", round(kurt_edad, 2)), size = 5, color = "black") +
  
  # Añadir la media, mediana y moda en la parte superior derecha
  annotate("text", x = max(df$`¿Cuantos años tienes?`), 
           y = 0.10, label = paste("Mean:", round(mean_edad, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuantos años tienes?`), 
           y = 0.08, label = paste("Median:", round(median_edad, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuantos años tienes?`), 
           y = 0.06, label = paste("Mode:", mode_edad), size = 5, color = "black", hjust = 1) +
  
  # Etiquetas y título
  labs(title = "Histograma y Densidad de las edades",
       x = "Edad", y = "Densidad") +
  theme_minimal()




# Cargar librerías
library(ggplot2)
library(e1071)  # Para calcular sesgo y curtosis
library(dplyr)  # Para calcular la moda

# Asegúrate de que la columna de interés esté en formato numérico
df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)` <- as.numeric(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)

# Calcular el sesgo (skewness) y curtosis (kurtosis)
skew <- skewness(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`, na.rm = TRUE)
kurt <- kurtosis(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`, na.rm = TRUE)

# Calcular media, mediana y moda
mean_nutrition <- mean(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`, na.rm = TRUE)
median_nutrition <- median(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`, na.rm = TRUE)
mode_nutrition <- as.numeric(names(sort(table(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`), decreasing = TRUE))[1])

# Imprimir resultados
cat("Sesgo (Skewness):", skew, "\n")
cat("Curtosis (Kurtosis):", round(kurt, 2), "\n")

# Crear el histograma con la línea de densidad
windows(width = 12, height = 6)

ggplot(df, aes(x = `¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)) +
  # Histograma con un número específico de bins
  geom_histogram(aes(y = ..density..), bins = 8, fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Curva de densidad
  geom_density(color = "red", size = 1) +
  
  # Añadir el valor de la curtosis al gráfico
  annotate("text", x = 3, y = 0.3, label = paste("Platykurtic:", round(kurt, 2)), size = 5, color = "black") +
  
  # Añadir la media, mediana y moda en la parte superior derecha
  annotate("text", x = max(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`), 
           y = 0.35, label = paste("Mean:", round(mean_nutrition, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`), 
           y = 0.32, label = paste("Median:", round(median_nutrition, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`), 
           y = 0.29, label = paste("Mode:", mode_nutrition), size = 5, color = "black", hjust = 1) +
  
  # Etiquetas y título
  labs(title = "Histograma y Densidad de consumo semanal de comida saludable",
       x = "Veces a la semana", y = "Densidad") +
  theme_minimal()










# Load necessary library
library(ggplot2)
library(e1071)  # Para calcular sesgo y curtosis

# Assuming df is already loaded
# Convert the column to numeric if it's not already
df$healthy_eating_frequency <- as.numeric(df$`¿Cuántas veces a la semana consumes comida saludable?( 0 nunca : 7 todos los dias)`)

# Create the bar chart
windows()  # Open a new window for the plot
ggplot(df, aes(x = factor(healthy_eating_frequency))) +
  geom_bar(fill = "skyblue", color = "black") +
  
  # Add text labels on top of the bars
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  
  labs(
    title = "Frecuencia de Consumo de Comida Saludable Durante la Semana",
    x = "Frecuencia de Consumo (Días a la Semana)",
    y = "Cantidad de Personas"
  ) +
  theme_minimal()








# Cargar librerías
library(ggplot2)
library(e1071)  # Para calcular sesgo y curtosis
library(dplyr)  # Para calcular la moda

# Asegúrate de que la columna de interés esté en formato numérico
df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?` <- as.numeric(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`)

# Calcular el sesgo (skewness) y curtosis (kurtosis)
skew_nutrition <- skewness(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`, na.rm = TRUE)
kurt_nutrition <- kurtosis(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`, na.rm = TRUE)

# Calcular media, mediana y moda
mean_nutrition <- mean(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`, na.rm = TRUE)
median_nutrition <- median(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`, na.rm = TRUE)
mode_nutrition <- as.numeric(names(sort(table(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`), decreasing = TRUE))[1])

# Imprimir resultados
cat("Sesgo (Skewness) de horas de consumo de contenido sobre nutrición:", skew_nutrition, "\n")
cat("Curtosis (Kurtosis) de horas de consumo de contenido sobre nutrición:", round(kurt_nutrition, 2), "\n")

# Crear el histograma con la línea de densidad
windows(width = 12, height = 6)

ggplot(df, aes(x = `¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`)) +
  # Histograma con un número específico de bins
  geom_histogram(aes(y = ..density..), bins = 8, fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Curva de densidad
  geom_density(color = "red", size = 1) +
  
  # Añadir el valor de la curtosis al gráfico
  annotate("text", x = 3, y = 0.3, label = paste("Platykurtic:", round(kurt_nutrition, 2)), size = 5, color = "black") +
  
  # Añadir la media, mediana y moda en la parte superior derecha
  annotate("text", x = max(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`), 
           y = 0.35, label = paste("Mean:", round(mean_nutrition, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`), 
           y = 0.32, label = paste("Median:", round(median_nutrition, 2)), size = 5, color = "black", hjust = 1) +
  annotate("text", x = max(df$`¿Cuántas horas a la semana consumes contenido sobre nutrición y comida saludable?`), 
           y = 0.29, label = paste("Mode:", mode_nutrition), size = 5, color = "black", hjust = 1) +
  
  # Etiquetas y título
  labs(title = "Histograma y Densidad de consumo semanal de contenido sobre nutrición",
       x = "Horas a la semana", y = "Densidad") +
  theme_minimal()





















# Ajustar el tamaño de la ventana gráfica
windows(width = 10, height = 7)

# Calcular los cuartiles y la media
summary_stats <- df %>%
  group_by(Factores_Influencia) %>%
  summarise(
    Q1 = quantile(Edad, 0.25, na.rm = TRUE),
    Q2 = median(Edad, na.rm = TRUE),
    Q3 = quantile(Edad, 0.75, na.rm = TRUE),
    mean = mean(Edad, na.rm = TRUE)
  )

# Gráfico de Boxplot con media y cuartiles
p_boxplot <- ggplot(df, aes(x = .data[[ "Factores_Influencia" ]], y = .data[[ "Edad" ]])) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3, shape = 18) +  # Añadir la media
  theme_minimal() +
  labs(title = "¿Qué valor de la media y la mediana refleja las diferencias en las percepciones de los factores de influencia sobre la comida saludable?",
       x = "Factores de Influencia", y = "Edad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Ajustar la rotación de las etiquetas del eje X
  geom_text(data = summary_stats, aes(x = Factores_Influencia, y = Q1, label = paste("Q1:", round(Q1, 1))), 
            color = "blue", vjust = -1.5, nudge_y = 1) +  # Separar Q1
  geom_text(data = summary_stats, aes(x = Factores_Influencia, y = Q2, label = paste("Mediana:", round(Q2, 1))), 
            color = "blue", vjust = -2, nudge_y = 10) +  # Separar Mediana más arriba
  geom_text(data = summary_stats, aes(x = Factores_Influencia, y = Q3, label = paste("Q3:", round(Q3, 1))), 
            color = "blue", vjust = -1.5, nudge_y = 1) +  # Separar Q3
  geom_text(data = summary_stats, aes(x = Factores_Influencia, y = mean, label = paste("Media:", round(mean, 1))), 
            color = "red", vjust = -3, nudge_y = 5)  # Separar Media más arriba

# Mostrar el gráfico
print(p_boxplot)





