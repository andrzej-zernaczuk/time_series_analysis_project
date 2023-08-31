library(forecast)
library(ggplot2)
library(tseries)

theme_update(plot.title = element_text(hjust = 0.5))

# import csv
ibm <- read.csv(file="./data/IBM.csv", header=TRUE)
ibm$Date <- as.Date(ibm$Date)

# create time series
IBM.ts <- ts(ibm$Close, start=c(1962, 2), frequency=12)

# typical plot
plot(IBM.ts, main="Wycena akcji IBM", xlab="Lata", ylab="Wartość w $")


# seasonal plots
par(mfrow=c(2, 1))
monthplot(IBM.ts, main="Dekompozycja miesięczna", ylab="Cena akcji", xlab="Miesiące")
seasonplot(IBM.ts, year.labels=TRUE, col=rainbow(5), main="Dekompozycja sezonowa", ylab="Cena akcji", xlab="Miesiące")

# Dickey-Fuller and KPS on entry data
adf.test(IBM.ts)
kpss.test(IBM.ts)

# Differencing
IBM_diff <- diff(IBM.ts)

par(mfrow=c(1, 1))
plot(IBM_diff, main="Zróżnicowany szereg", xlab="Lata", ylab="")

# Dickey-Fuller and KPS on differenced data
adf.test(IBM_diff)
kpss.test(IBM_diff)

# Box-Ljung test
Box.test(IBM_diff, type="Ljung-Box", lag=12)
