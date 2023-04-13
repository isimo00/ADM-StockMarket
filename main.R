rm(list = ls())
library(lmtest)
library(quadprog)
library(tseries)
library(tsDyn)
library(aTSA)
library(vars)

#Load data
data <- read.csv2("BAJAJFINSV.csv", header=TRUE, sep=",")
date <- as.Date(data[1:2838,1])
for(x in 4:12){
  data[,x] <- as.numeric(data[,x])
}
rm(x)
open <- data[1:2838, 5]
vwap <- data[1:2838, 10]

# plot VWAP (Volume Weighted Average Price)
pdf(file="figures/vwap-time.pdf", 5, 3.57)
if (length(date) == length(vwap)){
  plot(date, vwap, xlab = "Date", ylab = "VWAP [Rs]", "l")
}
dev.off()

# Augmented Dickey-Fuller Test
adf.test(vwap, k=0)
# Data differentiation
vwap_diff1 <- diff(vwap, differences = 1)
open_diff1 <- diff(open, differences = 1)
adf.test(vwap_diff1, k=0)
adf.test(open_diff1, k=0)

### ARIMA ########
# Autocorrelation and Partial Autocorrelation tests
Mlag <- as.numeric(difftime(max(date),min(date), units = "days"))
acf(vwap, lag.max = Mlag)
pacf(vwap, lag.max = Mlag)
rm(Mlag)

### VAR ########
# Granger causality test
pdf(file="figures/open-wap-time.pdf", 5, 3.57)
plot(date, open, xlab = "Date", ylab = "Open price, VWAP [Rs]", "l", col="red")
lines(date, vwap, col="blue")
legend(x="topleft", legend=c("Open price", "VWAP"),
       col=c("red", "blue"), lty=1, cex=0.8)
dev.off()
grangertest(vwap, open, 1)

date_truncated <- date[2595:2838]
vwap_truncated <- vwap_diff1[2595:2838]
open_truncated <- open_diff1[2595:2838]

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
plot(VWAPirf, ylab = "VWAP", main = "VWAP's shock to VWAP")

forecast <- predict(MODEL, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "VWAP", main = "Fanchart for VWAP", xlab = "Horizon", ylab = "VWAP")
fanchart(forecast, names = "Open", main = "Fanchart for Open", xlab = "Horizon", ylab = "Open price")