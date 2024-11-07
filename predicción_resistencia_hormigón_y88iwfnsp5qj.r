# Instalar y cargar los paquetes necesarios
install.packages("psych")
install.packages("GGally")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("lmtest")
library(lmtest)
library(ggplot2)
library(gridExtra)
library(psych)
library(GGally)

# Cargar los datos
library(readr)
concrete_data <- read_csv("C:/Users/ALVARO/Downloads/concrete_data.csv")
View(concrete_data)
dim(insurance)
str(insurance)
names(insurance)
summary(insurance)

# Calcular la matriz de correlación y redondear a 3 decimales
round(cor(x = concrete_data, method = "pearson"), 3)

# Crear histogramas múltiples para todas las variables en concrete_data
multi.hist(x = concrete_data,dcol = c("blue", "darkred"),dlty = c("solid", "dotted"),main = "",ncol = 3,breaks = 20)

# Crear la matriz de gráficos de dispersión para el dataset concrete_data
ggpairs(concrete_data, 
        lower = list(continuous = "smooth"),  # Añadir líneas de suavizado en la parte inferior
        diag = list(continuous = "barDiag"),  # Histograma en la diagonal
        axisLabels = "none")                  # Ocultar etiquetas de los ejes

# Ajustar el modelo de regresión lineal múltiple para predecir Strength
modelo <- lm(Strength ~ Cement + `Blast Furnace Slag` + `Fly Ash` + Water + Superplasticizer + 
               `Coarse Aggregate` + `Fine Aggregate` + Age, data = concrete_data)

# Ver el resumen del modelo
summary(modelo)

# Aplicar selección de variables paso a paso (stepwise selection)
step(object = modelo, direction = "both", trace = 1)

# Modelo simplificado con los mejores predictores
modelo_simplificado <- lm(Strength ~ Cement + `Blast Furnace Slag` + 
                         `Superplasticizer` + `Coarse Aggregate` + `Fly Ash` 
                                            + Age, data = concrete_data)

# Resumen del modelo simplificado
summary(modelo_simplificado)

# Calcular intervalos de confianza para los coeficientes del modelo simplificado
confint(modelo_simplificado)

# Crear gráficos de residuos para cada uno de los mejores predictores en el modelo simplificado
plot1 <- ggplot(data = concrete_data, aes(x = Cement, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Cement", y = "Residuals")

plot2 <- ggplot(data = concrete_data, aes(x = `Blast Furnace Slag`, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Blast Furnace Slag", y = "Residuals")

plot3 <- ggplot(data = concrete_data, aes(x = Superplasticizer, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Superplasticizer", y = "Residuals")

plot4 <- ggplot(data = concrete_data, aes(x = `Coarse Aggregate`, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Coarse Aggregate", y = "Residuals")

plot5 <- ggplot(data = concrete_data, aes(x = `Fly Ash`, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Fly Ash", y = "Residuals")

plot6 <- ggplot(data = concrete_data, aes(x = Age, y = modelo_simplificado$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + labs(x = "Age", y = "Residuals")

# Organizar los gráficos en una cuadrícula de 3x2
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

#Distribución normal de los residuos
qqnorm(modelo_simplificado$residuals)
qqline(modelo_simplificado$residuals)

#Test de Shapiro
shapiro.test(modelo_simplificado$residuals)

