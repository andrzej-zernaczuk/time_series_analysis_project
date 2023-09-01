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

# Box-Ljung test
Box.test(energy_ts, type="Ljung-Box", lag=12)

acf(energy_ts, lag.max = 100, main="Korelacja szeregu")
pacf(energy_ts, lag.max = 100, main="Cząstkowa korelacja szeregu")

