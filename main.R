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
lagselect <- VARselect(variables_used, lag.max = 4, type = "const", season = 4)
lagselect$selection
var.model_lev <- VAR(variables_used, p = 3, season = NULL, exog = NULL)
summary(var.model_lev)
forecast <- predict(var.model_lev, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "vwap", main = "Fanchart for vwap", xlab = "Horizon", ylab = "RRP")
fanchart(forecast, names = "open", main = "Fanchart for vwap", xlab = "Horizon", ylab = "RRP")