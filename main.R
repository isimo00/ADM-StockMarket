rm(list = ls())
library(quadprog)
library(tseries)
library(tsDyn)

#Load data
data <- read.csv2("BAJAJFINSV.csv", header=TRUE, sep=",")
date <- as.Date(data[,1])
for(x in 4:12){
  data[,x] <- as.numeric(data[,x])
}
rm(x)
vwap <- data[, 10]

# plot VWAP (Volume Weighted Average Price)
pdf(file="figures/vwap-time.pdf", 5, 3.57)
if (length(date) == length(vwap)){
  plot(date, vwap, xlab = "Date", ylab = "VWAP", "l")
}
dev.off()

# Augmented Dickey-Fuller Test
adf.test(vwap, k=0)
# Data differentiation
vwap_diff1 <- diff(vwap, differences = 1)
adf.test(vwap_diff1, k=0)

# Autocorrelation and Partial Autocorrelation tests
Mlag <- as.numeric(difftime(max(date),min(date), units = "days"))
acf(vwap, lag.max = Mlag)
pacf(vwap, lag.max = Mlag)

# Granger causality test
grangertest(vwap, y, 4) 
