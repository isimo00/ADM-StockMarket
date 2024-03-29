rm(list = ls())
library(lmtest)
library(quadprog)
library(tseries)
library(tsDyn)
library(aTSA)
library(vars)
library(tidyverse)
library(mFilter)
library(astsa)
library(fUnitRoots)
library(FitAR)

#Load data
data <- read.csv2("BAJAJFINSV.csv", header=TRUE, sep=",")
new_data <- data.frame(Date = as.Date(data[,1]), 
                       Weekday_name = wday(as.Date(data[,1]), label=TRUE), 
                       VWAP = as.numeric(data$VWAP))
rm(data)
data <- new_data[new_data$Weekday_name == "dl",]
rm(new_data)
VWAP <- ts(data$VWAP, start = c(2008,5,26), end= c(2019, 11, 16), frequency = 51)
components.ts = decompose(VWAP)
pdf(file="figures/ARIMA-vwap-components.pdf", 5, 5)
plot(components.ts)
dev.off()

# Remove non-stationary
urkpssTest(VWAP, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(VWAP, differences=1)
plot(tsstationary, ylab = "VWAP stationary")
acf(VWAP,lag.max=34) 
timeseriesseasonallyadjusted <- VWAP- components.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

## FITTING THE MODEL
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

fitARIMA <- arima(VWAP, order=c(1,1,0),seasonal = list(order = c(1,0,0), period = 51),method="CSS")
coeftest(fitARIMA) 
confint(fitARIMA)

acf(fitARIMA$residuals)
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)
forecast::auto.arima(VWAP, trace=TRUE) 

## Forecasting
predict(fitARIMA,n.ahead = 5)
futurVal <- forecast(fitARIMA, lead = 50)
plot(futurVal, type="l")
