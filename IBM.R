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
tsdisplay(IBM_diff)
acf(IBM_diff, lag.max = 200, main="Korelacja zróżnicowanego szeregu")
pacf(IBM_diff, lag.max = 200, main="Cząstkowa zróżnicowanego korelacja szeregu")

# Dickey-Fuller and KPS on differenced data
adf.test(IBM_diff)
kpss.test(IBM_diff)

# Box-Ljung test
Box.test(IBM_diff, type="Ljung-Box", lag=12)

tsdisplay(IBM_diff)
acf(IBM_ts, lag.max = 200, main="Korelacja szeregu")
pacf(IBM_ts, lag.max = 200, main="Cząstkowa korelacja szeregu")

# train/test set
IBM_train <- window(IBM_ts, end=c(2023, 5))
IBM_test <- window(IBM_ts, start=c(2023, 6))

# horizon for predictions and indicators
horizon <- length(IBM_test)
indicators <- c("RMSE", "MAE", "MAPE", "MASE")


ARIMA = Arima(IBM_train, order=c(0,2,0))
summary(ARIMA)

ARIMA_forecast <- forecast(ARIMA, h=horizon)
accuracy(ARIMA_forecast, IBM_test)[, indicators]
plot(ARIMA_forecast, main="Predykcja ARIMA")

ARIMA_output <- data.frame(
  "Point Forecast" = ARIMA_forecast$mean,
  "Lo 80" = ARIMA_forecast$lower[, 1],
  "Hi 80" = ARIMA_forecast$upper[, 1],
  "Lo 95" = ARIMA_forecast$lower[, 2],
  "Hi 95" = ARIMA_forecast$upper[, 2]
)
ARIMA_output


HOLT = holt(IBM_train, h=3)
summary(HOLT)

HOLT_forecast <- forecast(HOLT, h=horizon)
accuracy(HOLT_forecast, IBM_test)[, indicators]
plot(HOLT_forecast, main="Predykcja HOLT")


plot(window(IBM_ts, start=c(2023, 6)), type="l", col="green", lwd=2, xaxt="n", main="Wyniki predykcji", ylab="", xlab="", ylim=range(c(window(IBM_ts, start=c(2023, 6)), ARIMA_forecast$mean, HOLT_forecast$mean)))
# Add the forecasts to the plot
lines(ARIMA_forecast$mean, col="blue", lwd=2)
lines(HOLT_forecast$mean, col="red", lwd=2)

# Add a legend
legend("topright", legend=c("Wartości realne", "ARIMA", "Holt"), col=c("green", "blue", "red"), lwd=2)
