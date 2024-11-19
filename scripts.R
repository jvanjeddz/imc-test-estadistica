remove(list = ls())
graphics.off()
library(ggplot2)
library(dplyr)

data <- read.csv("datos.csv")
data$IMC <- data$peso / (data$estatura**2)

data <- data %>%
  mutate(categoria_imc = case_when(
    IMC <= 18.49 ~ "1. Bajo",
    IMC <= 24.99 ~ "2. Normal",
    IMC <= 29.99 ~ "3. Sobrepeso",
    T ~ "4. Obesidad"
  ))

data <- data %>%
  mutate(categoria_horas = case_when(
    horas == 0 ~ "1. Nula",
    horas <= 2 ~ "2. Baja",
    horas <= 5 ~ "3. Moderada",
    T ~ "4. Intensa"
  ))


n <- length(data$peso)
k <- round(1 + log2(n))

rango_peso <- range(data$peso)
print(rango_peso)
breaks_peso <- seq(rango_peso[1], rango_peso[2], length.out = k + 1)
print(breaks_peso)
tabla_freq_peso <- table(cut(data$peso, breaks = breaks_peso, include.lowest = T, right = F))
print(tabla_freq_peso)
barplot(tabla_freq_peso,
        main = "Tabla de frecuencia del peso",
        xlab = "Peso (kg)",
        ylab = "Frecuencia",
        col = "blue")

rango_estatura <- range(data$estatura)
print(rango_estatura)
breaks_estatura <- seq(rango_estatura[1], rango_estatura[2], length.out = k + 1)
print(breaks_estatura)
tabla_freq_estatura <- table(cut(data$estatura, breaks = breaks_estatura, include.lowest = T, right = F))
print(tabla_freq_estatura)
barplot(tabla_freq_estatura,
        main = "Tabla de frecuencia de la estatura",
        xlab = "Estatura (metros)",
        ylab = "Frecuencia",
        col = "red")

rango_horas <- range(data$horas)
print(rango_horas)
breaks_horas <- seq(rango_horas[1], rango_horas[2], length.out = k + 1)
print(breaks_horas)
tabla_freq_horas <- table(cut(data$horas, breaks = breaks_horas, include.lowest = T, right = F))
print(tabla_freq_horas)
barplot(tabla_freq_horas,
        main = "Tabla de frecuencia de horas de actividad física por semana",
        xlab = "Horas",
        ylab = "Frecuencia",
        col = "green")

rango_imc <- range(data$IMC)
print(rango_imc)
breaks_imc <- seq(rango_imc[1], rango_imc[2], length.out = k + 1)
print(breaks_imc)
tabla_freq_imc <- table(cut(data$IMC, breaks = breaks_imc, include.lowest = T, right = F))
print(tabla_freq_imc)
barplot(tabla_freq_imc,
        main = "Tabla de frecuencia del Índice de Masa Corporal",
        xlab = "IMC",
        ylab = "Frecuencia",
        col = "yellow")

tabla_contingencia <- table(data$categoria_horas, data$categoria_imc)
print(tabla_contingencia)

chi_cuadrada <- chisq.test(tabla_contingencia)
print(chi_cuadrada)

df_tabla_con <- as.data.frame(tabla_contingencia)

ggplot(df_tabla_con, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Horas de actividad física por semana", y = NULL, fill = "Índice de masa corporal") +
  theme_minimal()

write.csv(data, "archivo.csv")