library(tidyverse)

APPL <- read_csv("../Data/AAPL_2010_2019.csv")

### 1. Análisis general de las acciones:

# Comportamiento de las acciones
library(ggthemes)

ggplot(APPL, aes(x = Date, y = APPL$`Adj Close`)) +
  geom_line(aes(color = "Apple")) +
  labs(color = "Legend") +
  ggtitle("Cierres de Apple") +
  theme_dark()

# Diferencia de un día a otro en las acciones
dif_acc <- diff(APPL$`Adj Close`)
plot(dif_acc, type = "l")

# Histograma:
hist(dif_acc, probability = T)


### 2. Análisis de la serie temporal - Modelado/Predicción:

# Serie temporal
APPL_ts <- ts(APPL$`Adj Close`, start = c(2010, 1), frequency = 12)
head(APPL_ts, 10)

plot(APPL_ts, col = "darkgoldenrod1")

# Modelo
appl_mod <- HoltWinters(APPL_ts)
head(appl_mod, 10)

# Gráfico del real vs modelo
autoplot(appl_mod, col = "orange1", fcol = "red3") +
  labs(x = "Tiempo",
       y = "Cierre Ajustado",
       title = "Real vs Modelo") +
  theme_dark()

# Error 
appl_mod$SSE

# Predicción
library(forecast)

appl_pred <- forecast(appl_mod, h = 24)

# Gráfico de la predicción
autoplot(appl_pred, col = "red3", fcol = "deepskyblue4") +
  labs(x = "Tiempo",
       y = "Cierre Ajustado",
       title = "Predicción") +
  theme_update()

# Componentes del intervalo de confianza
ic_super <- appl_pred$upper
ic_inf <- appl_pred$lower

# Comportamiento real de las acciones entre 2010 - 2020
appl_2020 <- read_csv("../Data/AAPL_2010_2020.csv")
appl_2020_ts <- ts(appl_2020$`Adj Close`, start = c(2010, 1), frequency = 12)

autoplot(appl_2020_ts, colour = "darkgoldenrod1") +
  labs(x = "Time",
       y = "Adj Close",
       title = "Serie de Tiempo Apple 2010-2020")+
  theme_update()

# Comparación Real vs Predicción
plot(appl_pred,
     xlab = "",
     ylab = "",
     axes = FALSE,
     col = "red3",
     fcol = "steelblue4")
par(new = TRUE)
plot(appl_2020_ts, col = "darkgoldenrod1")


### 3. Predicción con ARIMA:

# Modelo autorregresivo
appl_arima <- auto.arima(APPL_ts)
summary(appl_arima)

# Predicción
ap_ped_arima <- forecast(appl_arima, h = 12)

# Gráfico de la predicción
autoplot(ap_ped_arima, col = "firebrick3", fcol = "deepskyblue3") +
  labs(x = "Time",
       y = "Adj Close",
       title = "Predicción con ARIMA")+
  theme_update()

