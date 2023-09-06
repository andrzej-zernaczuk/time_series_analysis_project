library(forecast)
library(ggplot2)
library(tseries)

theme_update(plot.title = element_text(hjust = 0.5))

# import csv
energy <- read.csv(file="./data/Norway_energy.csv", header=TRUE)
energy$Date <- as.Date(energy$Date, format = '%Y-%m')

# create time series
energy_ts <- ts(energy$Energy, start=c(2008, 1), frequency=12)

# typical plot
plot(energy_ts, main="Ilość energii", xlab="Lata", ylab="Wartość w kWh")

# seasonal plots
monthplot(energy_ts, main="Dekompozycja miesięczna", ylab="Ilość w kWh", xlab="Miesiące")
seasonplot(energy_ts, year.labels=TRUE, col=rainbow(5), main="Dekompozycja sezonowa", ylab="Ilość w kWh", xlab="Miesiące")

# Dickey-Fuller and KPS on entry data
adf.test(energy_ts)
kpss.test(energy_ts)

energy_diff <- diff(diff(energy_ts, 12))
plot(energy_diff, main="Zróżnicowany szereg", xlab="Lata", ylab="")
tsdisplay(energy_diff)
acf(energy_diff, lag.max = 200, main="Korelacja zróżnicowanego szeregu")
pacf(energy_diff, lag.max = 200, main="Cząstkowa zróżnicowanego korelacja szeregu")

# Dickey-Fuller and KPS on differenced data
adf.test(energy_diff)
kpss.test(energy_diff)

# Box-Ljung test
Box.test(energy_diff, type="Ljung-Box", lag=12)
tsdisplay(energy_diff)
acf(energy_ts, lag.max = 200, main="Korelacja szeregu")
pacf(energy_ts, lag.max = 200, main="Cząstkowa korelacja szeregu")

# train/test set
energy_train <- window(energy_ts, end=c(2022, 7))
energy_test <- window(energy_ts, start=c(2022, 8))

# horizon for predictions and indicators
horizon <- length(energy_test)
indicators <- c("RMSE", "MAE", "MAPE", "MASE")


SARIMA_auto = auto.arima(energy_train, d=2, D=1)
summary(SARIMA)

SARIMA = Arima(energy_train, order=c(1, 2, 0), seasonal=c(0, 1, 0))
summary(SARIMA)

SARIMA1 = Arima(energy_train, order=c(2, 2, 0), seasonal=c(0, 1, 0))
summary(SARIMA1)

SARIMA2 = Arima(energy_train, order=c(2, 2, 1), seasonal=c(0, 1, 0))
summary(SARIMA2)


SARIMA_forecast <- forecast(SARIMA2, h=horizon)
accuracy(SARIMA_forecast, energy_test)[, indicators]
plot(SARIMA_forecast, main="Predykcja ARIMA")

SARIMA_output <- data.frame(
  "Point Forecast" = SARIMA_forecast$mean,
  "Lo 80" = SARIMA_forecast$lower[, 1],
  "Hi 80" = SARIMA_forecast$upper[, 1],
  "Lo 95" = SARIMA_forecast$lower[, 2],
  "Hi 95" = SARIMA_forecast$upper[, 2]
)
SARIMA_output


HOLT_add = hw(energy_train, h=12, seasonal="additive")
summary(HOLT_add)

HOLT_forecast_add <- forecast(HOLT_add, h=horizon)
accuracy(HOLT_forecast_add, energy_test)[, indicators]
plot(HOLT_forecast_add, main="Predykcja HOLT")

HOLT_multi = hw(energy_train, h=12, seasonal="multiplicative")
summary(HOLT_multi)

HOLT_forecast_multi <- forecast(HOLT_multi, h=horizon)
accuracy(HOLT_forecast_multi, energy_test)[, indicators]
plot(HOLT_forecast_multi, main="Predykcja HOLT")


plot(window(energy_ts, start=c(2022, 8)), type="l", col="green", ylab="Ilość energii", xaxt="n", xlab="", lwd=2, main="Wyniki predykcji", 
     ylim=range(c(window(energy_ts, start=c(2022, 8)), SARIMA_forecast$mean, HOLT_forecast_multi$mean)))
# Add the forecasts to the plot
lines(SARIMA_forecast$mean, col="blue", lwd=2)
lines(HOLT_forecast_multi$mean, col="red", lwd=2)

# Add a legend
legend("topright", legend=c("Wartości realne", "SARIMA", "Holt-Winters"), col=c("green", "blue", "red"), lwd=2)
