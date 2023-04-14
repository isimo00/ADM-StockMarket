# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/

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
VWAP <- ts(data$VWAP, start = c(2008,5,26), frequency = 51)
components.ts = decompose(VWAP)
plot(components.ts)


urkpssTest(VWAP, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(VWAP, differences=1)
plot(tsstationary)
acf(VWAP,lag.max=34) 
timeseriesseasonallyadjusted <- VWAP- components.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

fitARIMA <- arima(VWAP, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMA) 
confint(fitARIMA)

acf(fitARIMA$residuals)
boxresult-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)
auto.arima(tsData, trace=TRUE) 

## Forecasting
predict(fitARIMA,n.ahead = 5)
futurVal <- forecast(fitARIMA,h=10, level=c(99.5))
plot(futurVal)
