library(forecast)
library(ggplot2)
library(tseries)

theme_update(plot.title = element_text(hjust = 0.5))

# import csv
ibm <- read.csv(file="./data/IBM.csv", header=TRUE)
ibm$Date <- as.Date(ibm$Date)

# create time series
IBM_ts <- ts(ibm$Close, start=c(1962, 2), frequency=12)

# typical plot
plot(IBM_ts, main="Wycena akcji IBM", xlab="Lata", ylab="Wartość w $")

# seasonal plots
par(mfrow=c(2, 1))
monthplot(IBM_ts, main="Dekompozycja miesięczna", ylab="Cena akcji", xlab="Miesiące")
seasonplot(IBM_ts, year.labels=TRUE, col=rainbow(5), main="Dekompozycja sezonowa", ylab="Cena akcji", xlab="Miesiące")

# Dickey-Fuller and KPS on entry data
adf.test(IBM_ts)
kpss.test(IBM_ts)

IBM_diff = diff(diff(IBM_ts))

# Differencing
par(mfrow=c(1, 1))
plot(IBM_diff, main="Zróżnicowany szereg", xlab="Lata", ylab="")
acf(IBM_diff, lag.max = 100, main="Korelacja zróżnicowanego szeregu")
pacf(IBM_diff, lag.max = 100, main="Cząstkowa korelacja zróżnicowanego szeregu")


# Dickey-Fuller and KPS on differenced data
adf.test(IBM_diff)
kpss.test(IBM_diff)

# Box-Ljung test
Box.test(IBM_diff, type="Ljung-Box", lag=12)

# train/test set
IBM_train <- window(IBM_diff, end=c(2023, 5))
IBM_test <- window(IBM_diff, start=c(2023, 6))

# horizon for predictions and indicators
horizon <- length(IBM_test)
indicators <- c("RMSE", "MAE", "MAPE", "MASE")


ARIMA = arima(IBM_train, order=c(1,2,1), include.mean=FALSE)
summary(ARIMA)
BIC(ARIMA)

ARIMA_forecast <- forecast(ARIMA, h=horizon)
accuracy(ARIMA_forecast, IBM_test)[, indicators]
plot(ARIMA_forecast, main="Predykcja ARIMA")


HOLT = holt(IBM_train)
summary(HOLT)
BIC(HOLT)

HOLT_forecast <- forecast(HOLT, h=horizon)
accuracy(HOLT_forecast, IBM_test)[, indicators]
plot(HOLT_forecast, main="Predykcja HOLT")


IBM_forecast = cbind(IBM_test, ARIMA_forecast$mean, HOLT_forecast$mean )
plot.ts(IBM_forecast, plot.type="single", col = c("green", "blue", "red"),
        ylab = "Cena",
        xlab = "Czas",
        main = "Porównanie wartości realnych z predykcjami ARIMA i Holt",
        lwd = 1.5)
legend("topright", legend=c("Wartości realne", "ARIMA", "Holt"), col = c("green", "blue", "red"), lwd=2)

plot(window(IBM_ts, start=c(2023, 6)), type="l", col="green", lwd=2, xaxt="n", main="Wyniki predykcji", ylab="", xlab="", ylim=range(c(window(IBM_ts, start=c(2023, 6)), ARIMA_forecast_orig$mean, HOLT_forecast_orig$mean)))
# Add the forecasts to the plot
lines(ARIMA_forecast_orig$mean, col="blue", lwd=2)
lines(HOLT_forecast_orig$mean, col="red", lwd=2)

# Add a legend
legend("topright", legend=c("Wartości realne", "ARIMA", "Holt"), col=c("green", "blue", "red"), lwd=2)


ARIMA_output <- data.frame(
  "Point Forecast" = ARIMA_forecast_orig$mean,
  "Lo 80" = ARIMA_forecast_orig$lower[, 1],
  "Hi 80" = ARIMA_forecast_orig$upper[, 1],
  "Lo 95" = ARIMA_forecast_orig$lower[, 2],
  "Hi 95" = ARIMA_forecast_orig$upper[, 2]
)
ARIMA_output

HOLT_output <- data.frame(
  "Point Forecast" = HOLT_forecast_orig$mean,
  "Lo 80" = HOLT_forecast_orig$lower[, 1],
  "Hi 80" = HOLT_forecast_orig$upper[, 1],
  "Lo 95" = HOLT_forecast_orig$lower[, 2],
  "Hi 95" = HOLT_forecast_orig$upper[, 2]
)
HOLT_output
