# https://towardsdatascience.com/a-deep-dive-on-vector-autoregression-in-r-58767ebb3f06

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

#Load data
data <- read.csv2("BAJAJFINSV.csv", header=TRUE, sep=",")
new_data <- data.frame(Date = as.Date(data[,1]), 
                       Weekday_name = wday(as.Date(data[,1]), label=TRUE), 
                       VWAP = as.numeric(data$VWAP),
                       Open = as.numeric(data$Open))
rm(data)
data <- new_data[new_data$Weekday_name == "dl",]
rm(new_data)

VWAP <- ts(data$VWAP, start = c(2008,5,26), frequency = 51)
Open <- ts(data$Open, start = c(2008,5,26), frequency = 51)
tsplot(VWAP)

v1 <- cbind(VWAP, Open)
colnames(v1) <- cbind("VWAP", "Open")
lagselect <- VARselect(v1, lag.max = 15, type = "const")
lagselect$selection
Model1 <- VAR(v1, p = 15, type = "const", season = NULL, exog = NULL) 
summary(Model1)
Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1
Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

RRPirf <- irf(Model1, impulse = "VWAP", response = "VWAP", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "VWAP", main = "VWAP's shock to VWAP")

RRPirf <- irf(Model1, impulse = "Open", response = "Open", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "Open", main = "Open's shock to Open")

forecast <- predict(Model1, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "VWAP", main = "Fanchart for VWAP", xlab = "Horizon", ylab = "VWAP")
fanchart(forecast, names = "Open", main = "Fanchart for Open", xlab = "Horizon", ylab = "Open")