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

# plot VWAP (Volume Weighted Average Price)
pdf(file="figures/vwap-time.pdf", 5, 3.57)
if (length(date) == length(vwap)){
  plot(date, vwap, xlab = "Date", ylab = "VWAP [Rs]", "l")
}
dev.off()

# Augmented Dickey-Fuller Test
adf.test(vwap)
# Data differentiation
vwap_diff1 <- diff(vwap, differences = 1)
open_diff1 <- diff(open, differences = 1)
adf.test(vwap_diff1)
adf.test(open_diff1)

date_truncated <- date[2594:2838]
vwap_truncated <- vwap_diff1[2594:2838]
open_truncated <- open_diff1[2595:2838]

### ARIMA ########
# Autocorrelation and Partial Autocorrelation tests
Mlag <- as.numeric(difftime(max(date),min(date), units = "days"))
acf(vwap, lag.max = Mlag)
pacf(vwap, lag.max = Mlag)
rm(Mlag)

# https://towardsdatascience.com/time-series-analysis-with-auto-arima-in-r-2b220b20e8ab
stl()

Lambda<- BoxCox.lambda(trainUS)
fit_complex1 <- auto.arima(trainUS, xreg = trainREG_TS,D=1, approximation = FALSE, lambda = Lambda, drift=1)
forecast_complex1<-forecast(fit_complex1,xreg = testREG_TS)
checkresiduals(fit_complex1)
accuracy(f = forecast_complex1,x = testUS)

### VAR ########
# Granger causality test
pdf(file="figures/open-wap-time.pdf", 5, 3.57)
plot(date, open, xlab = "Date", ylab = "Open price, VWAP [Rs]", "l", col="red")
lines(date, vwap, col="blue")
legend(x="topleft", legend=c("Open price", "VWAP"),
       col=c("red", "blue"), lty=1, cex=0.8)
dev.off()
grangertest(vwap, open, 1)

# Split into test and train
open_var <- ts(open_truncated, start= date_truncated[1], end = date_truncated[244], frequency=0.5)
vwap_var <- ts(vwap_truncated, start= date_truncated[1], end = date_truncated[244], frequency=0.5)
variables_used <- cbind(vwap_var, open_var)
colnames(variables_used) <- cbind("VWAP","Open")
lagselect <- VARselect(variables_used, lag.max = 4, type = "const")
lagselect$selection
MODEL <- VAR(variables_used, p = 3, season = NULL, exog = NULL)
summary(MODEL)

# Model diagnostics
Serial1 <- serial.test(MODEL, lags.pt = 5, type = "PT.asymptotic")
Serial1
Arch1 <- arch.test(MODEL, lags.multi = 15, multivariate.only = TRUE)
Arch1
Norm1 <- normality.test(MODEL, multivariate.only = TRUE)
Norm1
Stability1 <- stability(MODEL, type = "OLS-CUSUM")
pdf(file="figures/stability-test-VAR.pdf", 5, 3.57)
plot(Stability1)
dev.off()

# Forecasting and simulations
# Policy
VWAPirf <- irf(MODEL, impulse = "VWAP", response = "VWAP", n.ahead = 20, boot = TRUE)
plot(VWAPirf, ylab = "VWAP", main = "VWAP's shock to VWAP") # not plotted

forecast <- predict(MODEL, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "VWAP", main = "Fanchart for VWAP", xlab = "Horizon", ylab = "VWAP")
fanchart(forecast, names = "Open", main = "Fanchart for Open", xlab = "Horizon", ylab = "Open price")