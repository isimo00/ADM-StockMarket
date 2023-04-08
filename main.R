rm(list = ls())

#Load data
data <- read.csv2("BAJAJFINSV.csv", header=TRUE, sep=",")
date <- as.Date(data[,1])
for(x in 4:12){
  data[,x] <- as.numeric(data[,x])
}
rm(x)
vwap <- data[, 10]

# plot VWAP (Volume Weighted Average Price)
if (length(date) == length(vwap)){
  plot(date, vwap, xlab = "Date", ylab = "VWAP")
}

# exploring the data
Mlag <- as.numeric(difftime(max(date),min(date), units = "days"))
acf(vwap, lag.max = Mlag)
pacf(vwap, lag.max = Mlag)
