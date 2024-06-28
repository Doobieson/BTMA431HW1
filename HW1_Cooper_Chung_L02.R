# Cooper Chung (REDACTED STUDENT ID #)
# Fall 2022 BTMA 431 HW1

load("HW1_data2022.rData")
library(lubridate)
library(dplyr)
library(ggplot2)

# 1a
# Set variables and master list
start.date <- as.Date("2017-12-01")
end.date <- as.Date("2022-09-01")
all.data <- list(BTC.charts, DASH.charts, ETH.charts, LTC.charts, PPC.charts, XLM.charts, XRP.charts)

# For loop to iterate over all.data
for(i in all.data){
  start.Value <- i[i$date == start.date,]
  end.Value <- i[i$date == end.date,]
  
  # Long Run ROI math
  long.run.roi <- (end.Value$close - start.Value$close) / start.Value$close
  print(long.run.roi)
}
print("Ethereum had the highest Long-Run ROI")

# 1b
# Filter out all data to required time frame (2017-12-01 and later)
BTC.scope.data <- filter(BTC.charts, date >= start.date)
DASH.scope.data <- filter(DASH.charts, date >= start.date)
ETH.scope.data <- filter(ETH.charts, date >= start.date)
LTC.scope.data <- filter(LTC.charts, date >= start.date)
PPC.scope.data <- filter(PPC.charts, date >= start.date)
XLM.scope.data <- filter(XLM.charts, date >= start.date)
XRP.scope.data <- filter(XRP.charts, date >= start.date)

# Math
n.BTC <- nrow(BTC.scope.data)
today.BTC <- BTC.scope.data$close[2:n.BTC]
yesterday.BTC <- BTC.scope.data$close[1:n.BTC - 1]
BTC.scope.data$daily.return <- c(NA, (today.BTC - yesterday.BTC) / yesterday.BTC)
mean(BTC.scope.data$daily.return, na.rm = TRUE)

n.DASH <- nrow(DASH.scope.data)
today.DASH <- DASH.scope.data$close[2:n.DASH]
yesterday.DASH <- DASH.scope.data$close[1:n.DASH - 1]
DASH.scope.data$daily.return <- c(NA, (today.DASH - yesterday.DASH) / yesterday.DASH)
mean(DASH.scope.data$daily.return, na.rm = TRUE)

n.ETH <- nrow(ETH.scope.data)
today.ETH <- ETH.scope.data$close[2:n.ETH]
yesterday.ETH <- ETH.scope.data$close[1:n.ETH - 1]
ETH.scope.data$daily.return <- c(NA, (today.ETH - yesterday.ETH) / yesterday.ETH)
mean(ETH.scope.data$daily.return, na.rm = TRUE)

n.LTC <- nrow(LTC.scope.data)
today.LTC <- LTC.scope.data$close[2:n.LTC]
yesterday.LTC <- LTC.scope.data$close[1:n.LTC - 1]
LTC.scope.data$daily.return <- c(NA, (today.LTC - yesterday.LTC) / yesterday.LTC)
mean(LTC.scope.data$daily.return, na.rm = TRUE)

n.PPC <- nrow(PPC.scope.data)
today.PPC <- PPC.scope.data$close[2:n.PPC]
yesterday.PPC <- PPC.scope.data$close[1:n.PPC - 1]
PPC.scope.data$daily.return <- c(NA, (today.PPC - yesterday.PPC) / yesterday.PPC)
mean(PPC.scope.data$daily.return, na.rm = TRUE)

n.XLM <- nrow(XLM.scope.data)
today.XLM <- XLM.scope.data$close[2:n.XLM]
yesterday.XLM <- XLM.scope.data$close[1:n.XLM - 1]
XLM.scope.data$daily.return <- c(NA, (today.XLM - yesterday.XLM) / yesterday.XLM)
mean(XLM.scope.data$daily.return, na.rm = TRUE)

n.XRP <- nrow(XRP.scope.data)
today.XRP <- XRP.scope.data$close[2:n.XRP]
yesterday.XRP <- XRP.scope.data$close[1:n.XRP - 1]
XRP.scope.data$daily.return <- c(NA, (today.XRP - yesterday.XRP) / yesterday.XRP)
mean(XRP.scope.data$daily.return, na.rm = TRUE)

print("Ripple had the highest mean daily return")

# 1c
# Math was completed above, only need to calculate for Standard Deviation
sd(BTC.scope.data$daily.return, na.rm = TRUE)
sd(DASH.scope.data$daily.return, na.rm = TRUE)
sd(ETH.scope.data$daily.return, na.rm = TRUE)
sd(LTC.scope.data$daily.return, na.rm = TRUE)
sd(PPC.scope.data$daily.return, na.rm = TRUE)
sd(XLM.scope.data$daily.return, na.rm = TRUE)
sd(XRP.scope.data$daily.return, na.rm = TRUE)

print("Bitcoin had the lowest standard deviation of its daily return")

# 2a
# Calculate number of shares bought for each cryptocurrency
BTC.shares <- 10000 / BTC.scope.data[1, "close"]
DASH.shares <- 2500 / DASH.scope.data[1, "close"]
ETH.shares <- 2500 / ETH.scope.data[1, "close"]
LTC.shares <- 5000 / LTC.scope.data[1, "close"]
PPC.shares <- 2500 / PPC.scope.data[1, "close"]
XLM.shares <- 2500 / XLM.scope.data[1, "close"]
XRP.shares <- 5000 / XRP.scope.data[1, "close"]

# Calculate portfolio values using shares bought
portfolio.one.value <- BTC.shares * BTC.scope.data[n.BTC, "close"]
print(paste(portfolio.one.value, "is the value of portfolio one on September 1, 2022"))

portfolio.two.value <- (XRP.shares * XRP.scope.data[n.XRP, "close"]) + (LTC.shares * LTC.scope.data[n.LTC, "close"])
print(paste(portfolio.two.value, "is the value of portfolio two on September 1, 2022"))


portfolio.three.value <- (ETH.shares * ETH.scope.data[n.ETH, "close"]) + 
  (DASH.shares * DASH.scope.data[n.DASH, "close"]) + 
  (PPC.shares * PPC.scope.data[n.PPC, "close"]) + 
  (XLM.shares * XLM.scope.data[n.XLM, "close"])
print(paste(portfolio.three.value, "is the value of portfolio three on September 1, 2022"))

# 2b
# Calculate portfolio values over time
portfolio.one.value.over.time <- BTC.shares * BTC.scope.data[1:n.BTC, "close"]
max(portfolio.one.value.over.time)
print(paste(max(portfolio.one.value.over.time), "is the highest value achieved by portfolio one"))

portfolio.two.value.over.time <- (XRP.shares * XRP.scope.data[1:n.XRP, "close"]) + (LTC.shares * LTC.scope.data[1:n.LTC, "close"])
max(portfolio.two.value.over.time)
print(paste(max(portfolio.two.value.over.time), "is the highest value achieved by portfolio two"))

portfolio.three.value.over.time <- (ETH.shares * ETH.scope.data[1:n.ETH, "close"]) + 
  (DASH.shares * DASH.scope.data[1:n.DASH, "close"]) + 
  (PPC.shares * PPC.scope.data[1:n.PPC, "close"]) + 
  (XLM.shares * XLM.scope.data[1:n.XLM, "close"])
max(portfolio.three.value.over.time)
print(paste(max(portfolio.three.value.over.time), "is the highest value achieved by portfolio three"))

print("Among the three portfolios, portfolio two achieved the highest value")

# Plot portfolio values over time
qplot(x = BTC.scope.data$date, y = portfolio.one.value.over.time,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 1 Value Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = XRP.scope.data$date, y = portfolio.two.value.over.time,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 2 Value Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = ETH.scope.data$date, y = portfolio.three.value.over.time,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 3 Value Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

# 2c
# Daily returns for all portfolios
portfolio.one.value.df <- as.data.frame(portfolio.one.value.over.time)
n.portfolio.one <- nrow(portfolio.one.value.df)
today.portfolio.one <- portfolio.one.value.df$portfolio.one.value.over.time[2:n.portfolio.one]
yesterday.portfolio.one <- portfolio.one.value.df$portfolio.one.value.over.time[1:n.portfolio.one - 1]
portfolio.one.value.df$daily.return <- c(NA, (today.portfolio.one - yesterday.portfolio.one) / yesterday.portfolio.one)

portfolio.two.value.df <- as.data.frame(portfolio.two.value.over.time)
n.portfolio.two <- nrow(portfolio.two.value.df)
today.portfolio.two <- portfolio.two.value.df$portfolio.two.value.over.time[2:n.portfolio.two]
yesterday.portfolio.two <- portfolio.two.value.df$portfolio.two.value.over.time[1:n.portfolio.two - 1]
portfolio.two.value.df$daily.return <- c(NA, (today.portfolio.two - yesterday.portfolio.two) / yesterday.portfolio.two)

portfolio.three.value.df <- as.data.frame(portfolio.three.value.over.time)
n.portfolio.three <- nrow(portfolio.three.value.df)
today.portfolio.three <- portfolio.three.value.df$portfolio.three.value.over.time[2:n.portfolio.three]
yesterday.portfolio.three <- portfolio.three.value.df$portfolio.three.value.over.time[1:n.portfolio.three - 1]
portfolio.three.value.df$daily.return <- c(NA, (today.portfolio.three - yesterday.portfolio.three) / yesterday.portfolio.three)

# Build scatterplots
qplot(x = BTC.scope.data$daily.return, y = portfolio.two.value.df$daily.return,
      geom = "point", xlab = "BTC Daily Return", ylab = "Portfolio Two Daily Return") +
  ggtitle("BTC Daily Return vs Portfolio Two Daily Return") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = BTC.scope.data$daily.return, y = portfolio.three.value.df$daily.return,
      geom = "point", xlab = "BTC Daily Return", ylab = "Portfolio Three Daily Return") +
  ggtitle("BTC Daily Return vs Portfolio Three Daily Return") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation calculations
cor(na.omit(portfolio.two.value.df$daily.return), na.omit(BTC.scope.data$daily.return))
print(paste(cor(na.omit(portfolio.two.value.df$daily.return), na.omit(BTC.scope.data$daily.return)), 
            "is the correlation between portfolio two's daily return and the daily return of Bitcoin"))

cor(na.omit(portfolio.three.value.df$daily.return), na.omit(BTC.scope.data$daily.return))
print(paste(cor(na.omit(portfolio.three.value.df$daily.return), na.omit(BTC.scope.data$daily.return)), 
            "is the correlation between portfolio three's daily return and the daily return of Bitcoin"))

# 2d
end.precovid.date <- as.Date("2019-12-31")

BTC.precovid.data <- filter(BTC.charts, date >= start.date & date <= end.precovid.date)
DASH.precovid.data <- filter(DASH.charts, date >= start.date & date <= end.precovid.date)
ETH.precovid.data <- filter(ETH.charts, date >= start.date & date <= end.precovid.date)
LTC.precovid.data <- filter(LTC.charts, date >= start.date & date <= end.precovid.date)
PPC.precovid.data <- filter(PPC.charts, date >= start.date & date <= end.precovid.date)
XLM.precovid.data <- filter(XLM.charts, date >= start.date & date <= end.precovid.date)
XRP.precovid.data <- filter(XRP.charts, date >= start.date & date <= end.precovid.date)

n.BTC.precovid <- nrow(BTC.precovid.data)
n.DASH.precovid <- nrow(DASH.precovid.data)
n.ETH.precovid <- nrow(ETH.precovid.data)
n.LTC.precovid <- nrow(LTC.precovid.data)
n.PPC.precovid <- nrow(PPC.precovid.data)
n.XLM.precovid <- nrow(XLM.precovid.data)
n.XRP.precovid <- nrow(XRP.precovid.data)

portfolio.one.value.precovid <- BTC.shares * BTC.precovid.data[1:n.BTC.precovid, "close"]

portfolio.two.value.precovid <- (XRP.shares * XRP.precovid.data[1:n.XRP.precovid, "close"]) + 
  (LTC.shares * LTC.precovid.data[1:n.LTC.precovid, "close"])

portfolio.three.value.precovid <- (ETH.shares * ETH.precovid.data[1:n.ETH.precovid, "close"]) + 
  (DASH.shares * DASH.precovid.data[1:n.DASH.precovid, "close"]) + 
  (PPC.shares * PPC.precovid.data[1:n.PPC.precovid, "close"]) + 
  (XLM.shares * XLM.precovid.data[1:n.XLM.precovid, "close"])

portfolio.one.precovid.df <- as.data.frame(portfolio.one.value.precovid)
n.portfolio.one.precovid <- nrow(portfolio.one.precovid.df)
today.portfolio.one.precovid <- portfolio.one.precovid.df$portfolio.one.value.precovid[2:n.portfolio.one.precovid]
yesterday.portfolio.one.precovid <- portfolio.one.precovid.df$portfolio.one.value.precovid[1:n.portfolio.one.precovid - 1]
portfolio.one.precovid.df$daily.return <- c(NA, (today.portfolio.one.precovid - yesterday.portfolio.one.precovid) / yesterday.portfolio.one.precovid)

portfolio.two.precovid.df <- as.data.frame(portfolio.two.value.precovid)
n.portfolio.two.precovid <- nrow(portfolio.two.precovid.df)
today.portfolio.two.precovid <- portfolio.two.precovid.df$portfolio.two.value.precovid[2:n.portfolio.two.precovid]
yesterday.portfolio.two.precovid <- portfolio.two.precovid.df$portfolio.two.value.precovid[1:n.portfolio.two.precovid - 1]
portfolio.two.precovid.df$daily.return <- c(NA, (today.portfolio.two.precovid - yesterday.portfolio.two.precovid) / yesterday.portfolio.two.precovid)

portfolio.three.precovid.df <- as.data.frame(portfolio.three.value.precovid)
n.portfolio.three.precovid <- nrow(portfolio.three.precovid.df)
today.portfolio.three.precovid <- portfolio.three.precovid.df$portfolio.three.value.precovid[2:n.portfolio.three.precovid]
yesterday.portfolio.three.precovid <- portfolio.three.precovid.df$portfolio.three.value.precovid[1:n.portfolio.three.precovid - 1]
portfolio.three.precovid.df$daily.return <- c(NA, (today.portfolio.three.precovid - yesterday.portfolio.three.precovid) / yesterday.portfolio.three.precovid)

sd(na.omit(portfolio.one.precovid.df$daily.return))
sd(na.omit(portfolio.two.precovid.df$daily.return))
sd(na.omit(portfolio.three.precovid.df$daily.return))

print(paste(sd(na.omit(portfolio.two.precovid.df$daily.return)), 
            "is the volatility of portfolio two between December 1, 2017, and December 31, 2019. It is also the highest."))

start.postcovid.date <- as.Date("2020-01-01")

BTC.postcovid.data <- filter(BTC.charts, date >= start.postcovid.date & date <= end.date)
DASH.postcovid.data <- filter(DASH.charts, date >= start.postcovid.date & date <= end.date)
ETH.postcovid.data <- filter(ETH.charts, date >= start.postcovid.date & date <= end.date)
LTC.postcovid.data <- filter(LTC.charts, date >= start.postcovid.date & date <= end.date)
PPC.postcovid.data <- filter(PPC.charts, date >= start.postcovid.date & date <= end.date)
XLM.postcovid.data <- filter(XLM.charts, date >= start.postcovid.date & date <= end.date)
XRP.postcovid.data <- filter(XRP.charts, date >= start.postcovid.date & date <= end.date)

n.BTC.postcovid <- nrow(BTC.postcovid.data)
n.DASH.postcovid <- nrow(DASH.postcovid.data)
n.ETH.postcovid <- nrow(ETH.postcovid.data)
n.LTC.postcovid <- nrow(LTC.postcovid.data)
n.PPC.postcovid <- nrow(PPC.postcovid.data)
n.XLM.postcovid <- nrow(XLM.postcovid.data)
n.XRP.postcovid <- nrow(XRP.postcovid.data)

portfolio.one.value.postcovid <- BTC.shares * BTC.postcovid.data[1:n.BTC.postcovid, "close"]

portfolio.two.value.postcovid <- (XRP.shares * XRP.postcovid.data[1:n.XRP.postcovid, "close"]) + 
  (LTC.shares * LTC.postcovid.data[1:n.LTC.postcovid, "close"])

portfolio.three.value.postcovid <- (ETH.shares * ETH.postcovid.data[1:n.ETH.postcovid, "close"]) + 
  (DASH.shares * DASH.postcovid.data[1:n.DASH.postcovid, "close"]) + 
  (PPC.shares * PPC.postcovid.data[1:n.PPC.postcovid, "close"]) + 
  (XLM.shares * XLM.postcovid.data[1:n.XLM.postcovid, "close"])

portfolio.one.postcovid.df <- as.data.frame(portfolio.one.value.postcovid)
n.portfolio.one.postcovid <- nrow(portfolio.one.postcovid.df)
today.portfolio.one.postcovid <- portfolio.one.postcovid.df$portfolio.one.value.postcovid[2:n.portfolio.one.postcovid]
yesterday.portfolio.one.postcovid <- portfolio.one.postcovid.df$portfolio.one.value.postcovid[1:n.portfolio.one.postcovid - 1]
portfolio.one.postcovid.df$daily.return <- c(NA, (today.portfolio.one.postcovid - yesterday.portfolio.one.postcovid) / yesterday.portfolio.one.postcovid)

portfolio.two.postcovid.df <- as.data.frame(portfolio.two.value.postcovid)
n.portfolio.two.postcovid <- nrow(portfolio.two.postcovid.df)
today.portfolio.two.postcovid <- portfolio.two.postcovid.df$portfolio.two.value.postcovid[2:n.portfolio.two.postcovid]
yesterday.portfolio.two.postcovid <- portfolio.two.postcovid.df$portfolio.two.value.postcovid[1:n.portfolio.two.postcovid - 1]
portfolio.two.postcovid.df$daily.return <- c(NA, (today.portfolio.two.postcovid - yesterday.portfolio.two.postcovid) / yesterday.portfolio.two.postcovid)

portfolio.three.postcovid.df <- as.data.frame(portfolio.three.value.postcovid)
n.portfolio.three.postcovid <- nrow(portfolio.three.postcovid.df)
today.portfolio.three.postcovid <- portfolio.three.postcovid.df$portfolio.three.value.postcovid[2:n.portfolio.three.postcovid]
yesterday.portfolio.three.postcovid <- portfolio.three.postcovid.df$portfolio.three.value.postcovid[1:n.portfolio.three.postcovid - 1]
portfolio.three.postcovid.df$daily.return <- c(NA, (today.portfolio.three.postcovid - yesterday.portfolio.three.postcovid) / yesterday.portfolio.three.postcovid)

sd(na.omit(portfolio.one.postcovid.df$daily.return))
sd(na.omit(portfolio.two.postcovid.df$daily.return))
sd(na.omit(portfolio.three.postcovid.df$daily.return))

print(paste(sd(na.omit(portfolio.two.postcovid.df$daily.return)), 
            "is the volatility of portfolio two between January 1, 2020, and September 1, 2022. It remains the highest post-covid."))

# 2e
portfolio.two.value.df$btc.daily.return <- BTC.scope.data$daily.return
portfolio.two.value.btc.positive <- filter(portfolio.two.value.df, btc.daily.return > 0)
portfolio.two.value.btc.negative <- filter(portfolio.two.value.df, btc.daily.return < 0)

portfolio.three.value.df$btc.daily.return <- BTC.scope.data$daily.return
portfolio.three.value.btc.positive <- filter(portfolio.three.value.df, btc.daily.return > 0)
portfolio.three.value.btc.negative <- filter(portfolio.three.value.df, btc.daily.return < 0)

mean(portfolio.two.value.btc.positive$daily.return, na.rm = TRUE)
print(paste(mean(portfolio.two.value.btc.positive$daily.return, na.rm = TRUE), 
            "is the mean daily return of portfolio two when BTC daily return was positive."))

mean(portfolio.two.value.btc.negative$daily.return, na.rm = TRUE)
print(paste(mean(portfolio.two.value.btc.negative$daily.return, na.rm = TRUE), 
            "is the mean daily return of portfolio two when BTC daily return was negative."))

mean(portfolio.three.value.btc.positive$daily.return, na.rm = TRUE)
print(paste(mean(portfolio.three.value.btc.positive$daily.return, na.rm = TRUE), 
            "is the mean daily return of portfolio three when BTC daily return was positive."))

mean(portfolio.three.value.btc.negative$daily.return, na.rm = TRUE)
print(paste(mean(portfolio.three.value.btc.negative$daily.return, na.rm = TRUE), 
            "is the mean daily return of portfolio three when BTC daily return was negative."))

print("Overall, neither portfolio dominated the other, as two had lower negatives, and three had higher positives")

# 3
# Long-run ROI (1a)
for(i in all.data){
  start.Value <- i[i$date == start.date,]
  end.Value <- i[i$date == end.date,]
  
  long.run.roi.avg <- (((end.Value$high + end.Value$low) / 2) - ((start.Value$high + end.Value$low) / 2)) / ((start.Value$high + end.Value$close) / 2)
  print(long.run.roi.avg)
}
print("1a doesn't change. While the numbers are different, ETH still has the highest Long-Run ROI")

# Mean daily return (1b)
BTC.high <- BTC.scope.data$high[1:n.BTC]
BTC.low <- BTC.scope.data$low[1:n.BTC]
BTC.scope.data$avg.return <- c((BTC.high + BTC.low) / 2)
today.BTC.avg <- BTC.scope.data$avg.return[2:n.BTC]
yesterday.BTC.avg <- BTC.scope.data$avg.return[1:n.BTC - 1]
BTC.scope.data$daily.return.avg <- c(NA, (today.BTC.avg - yesterday.BTC.avg) / yesterday.BTC.avg)
mean(BTC.scope.data$daily.return.avg, na.rm = TRUE)

DASH.high <- DASH.scope.data$high[1:n.DASH]
DASH.low <- DASH.scope.data$low[1:n.DASH]
DASH.scope.data$avg.return <- c((DASH.high + DASH.low) / 2)
today.DASH.avg <- DASH.scope.data$avg.return[2:n.DASH]
yesterday.DASH.avg <- DASH.scope.data$avg.return[1:n.DASH - 1]
DASH.scope.data$daily.return.avg <- c(NA, (today.DASH.avg - yesterday.DASH.avg) / yesterday.DASH.avg)
mean(DASH.scope.data$daily.return.avg, na.rm = TRUE)

ETH.high <- ETH.scope.data$high[1:n.ETH]
ETH.low <- ETH.scope.data$low[1:n.ETH]
ETH.scope.data$avg.return <- c((ETH.high + ETH.low) / 2)
today.ETH.avg <- ETH.scope.data$avg.return[2:n.ETH]
yesterday.ETH.avg <- ETH.scope.data$avg.return[1:n.ETH - 1]
ETH.scope.data$daily.return.avg <- c(NA, (today.ETH.avg - yesterday.ETH.avg) / yesterday.ETH.avg)
mean(ETH.scope.data$daily.return.avg, na.rm = TRUE)

LTC.high <- LTC.scope.data$high[1:n.LTC]
LTC.low <- LTC.scope.data$low[1:n.LTC]
LTC.scope.data$avg.return <- c((LTC.high + LTC.low) / 2)
today.LTC.avg <- LTC.scope.data$avg.return[2:n.LTC]
yesterday.LTC.avg <- LTC.scope.data$avg.return[1:n.LTC - 1]
LTC.scope.data$daily.return.avg <- c(NA, (today.LTC.avg - yesterday.LTC.avg) / yesterday.LTC.avg)
mean(LTC.scope.data$daily.return.avg, na.rm = TRUE)

PPC.high <- PPC.scope.data$high[1:n.PPC]
PPC.low <- PPC.scope.data$low[1:n.PPC]
PPC.scope.data$avg.return <- c((PPC.high + PPC.low) / 2)
today.PPC.avg <- PPC.scope.data$avg.return[2:n.PPC]
yesterday.PPC.avg <- PPC.scope.data$avg.return[1:n.PPC - 1]
PPC.scope.data$daily.return.avg <- c(NA, (today.PPC.avg - yesterday.PPC.avg) / yesterday.PPC.avg)
mean(PPC.scope.data$daily.return.avg, na.rm = TRUE)

XLM.high <- XLM.scope.data$high[1:n.XLM]
XLM.low <- XLM.scope.data$low[1:n.XLM]
XLM.scope.data$avg.return <- c((XLM.high + XLM.low) / 2)
today.XLM.avg <- XLM.scope.data$avg.return[2:n.XLM]
yesterday.XLM.avg <- XLM.scope.data$avg.return[1:n.XLM - 1]
XLM.scope.data$daily.return.avg <- c(NA, (today.XLM.avg - yesterday.XLM.avg) / yesterday.XLM.avg)
mean(XLM.scope.data$daily.return.avg, na.rm = TRUE)

XRP.high <- XRP.scope.data$high[1:n.XRP]
XRP.low <- XRP.scope.data$low[1:n.XRP]
XRP.scope.data$avg.return <- c((XRP.high + XRP.low) / 2)
today.XRP.avg <- XRP.scope.data$avg.return[2:n.XRP]
yesterday.XRP.avg <- XRP.scope.data$avg.return[1:n.XRP - 1]
XRP.scope.data$daily.return.avg <- c(NA, (today.XRP.avg - yesterday.XRP.avg) / yesterday.XRP.avg)
mean(XRP.scope.data$daily.return.avg, na.rm = TRUE)

print("1b changes when using average, as Peercoin now has the highest mean daily return")

# Again, Math was completed above, only need to calculate for Standard Deviation (1c)
sd(BTC.scope.data$daily.return.avg, na.rm = TRUE)
sd(DASH.scope.data$daily.return.avg, na.rm = TRUE)
sd(ETH.scope.data$daily.return.avg, na.rm = TRUE)
sd(LTC.scope.data$daily.return.avg, na.rm = TRUE)
sd(PPC.scope.data$daily.return.avg, na.rm = TRUE)
sd(XLM.scope.data$daily.return.avg, na.rm = TRUE)
sd(XRP.scope.data$daily.return.avg, na.rm = TRUE)

print("1c doesn't change when using average, BTC still has the lowest standard deviation")

# Portfolio Value (2a)
BTC.shares.avg <- 10000 / BTC.scope.data[1, "avg.return"]
DASH.shares.avg <- 2500 / DASH.scope.data[1, "avg.return"]
ETH.shares.avg <- 2500 / ETH.scope.data[1, "avg.return"]
LTC.shares.avg <- 5000 / LTC.scope.data[1, "avg.return"]
PPC.shares.avg <- 2500 / PPC.scope.data[1, "avg.return"]
XLM.shares.avg <- 2500 / XLM.scope.data[1, "avg.return"]
XRP.shares.avg <- 5000 / XRP.scope.data[1, "avg.return"]

# Calculate portfolio values using shares bought
portfolio.one.value.avg <- BTC.shares.avg * BTC.scope.data[n.BTC, "avg.return"]
print(paste(portfolio.one.value.avg, "is the value of portfolio one on September 1, 2022, using the average"))

portfolio.two.value.avg <- (XRP.shares.avg * XRP.scope.data[n.XRP, "avg.return"]) + (LTC.shares.avg * LTC.scope.data[n.LTC, "avg.return"])
print(paste(portfolio.two.value.avg, "is the value of portfolio two on September 1, 2022, using the average"))


portfolio.three.value.avg <- (ETH.shares.avg * ETH.scope.data[n.ETH, "avg.return"]) + 
  (DASH.shares.avg * DASH.scope.data[n.DASH, "avg.return"]) + 
  (PPC.shares.avg * PPC.scope.data[n.PPC, "avg.return"]) + 
  (XLM.shares.avg * XLM.scope.data[n.XLM, "avg.return"])
print(paste(portfolio.three.value.avg, "is the value of portfolio three on September 1, 2022, using the average"))

print("2a doesn't change when using average. While the values are different, BTC is still the highest value")

# Calculate portfolio values over time (2b)
portfolio.one.value.over.time.avg <- BTC.shares.avg * BTC.scope.data[1:n.BTC, "avg.return"]
max(portfolio.one.value.over.time.avg)
print(paste(max(portfolio.one.value.over.time.avg), "is the highest value achieved by portfolio one using the average"))

portfolio.two.value.over.time.avg <- (XRP.shares.avg * XRP.scope.data[1:n.XRP, "avg.return"]) + (LTC.shares.avg * LTC.scope.data[1:n.LTC, "avg.return"])
max(portfolio.two.value.over.time.avg)
print(paste(max(portfolio.two.value.over.time.avg), "is the highest value achieved by portfolio two using the average"))

portfolio.three.value.over.time.avg <- (ETH.shares.avg * ETH.scope.data[1:n.ETH, "avg.return"]) + 
  (DASH.shares.avg * DASH.scope.data[1:n.DASH, "avg.return"]) + 
  (PPC.shares.avg * PPC.scope.data[1:n.PPC, "avg.return"]) + 
  (XLM.shares.avg * XLM.scope.data[1:n.XLM, "avg.return"])
max(portfolio.three.value.over.time.avg)
print(paste(max(portfolio.three.value.over.time.avg), "is the highest value achieved by portfolio three using the average"))

print("Among the three portfolios, portfolio two achieved the highest value using the average")

# Plot portfolio values over time
qplot(x = BTC.scope.data$date, y = portfolio.one.value.over.time.avg,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 1 Value Over Time (Average)") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = XRP.scope.data$date, y = portfolio.two.value.over.time.avg,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 2 Value Over Time (Average)") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = ETH.scope.data$date, y = portfolio.three.value.over.time.avg,
      geom = "line", xlab = "Date", ylab = "Closing Price") +
  ggtitle("Portfolio 3 Value Over Time (Average)") +
  theme(plot.title = element_text(hjust = 0.5))

print("2b doesn't change when using average. While the actual value increases, portfolio two remains as the highest value")

# Daily returns for all portfolios (2c)
portfolio.one.value.avg.df <- as.data.frame(portfolio.one.value.over.time.avg)
n.portfolio.one.avg <- nrow(portfolio.one.value.avg.df)
today.portfolio.one.avg <- portfolio.one.value.avg.df$portfolio.one.value.over.time.avg[2:n.portfolio.one.avg]
yesterday.portfolio.one.avg <- portfolio.one.value.avg.df$portfolio.one.value.over.time.avg[1:n.portfolio.one.avg - 1]
portfolio.one.value.avg.df$daily.return.avg <- c(NA, (today.portfolio.one.avg - yesterday.portfolio.one.avg) / yesterday.portfolio.one.avg)

portfolio.two.value.avg.df <- as.data.frame(portfolio.two.value.over.time.avg)
n.portfolio.two.avg <- nrow(portfolio.two.value.avg.df)
today.portfolio.two.avg <- portfolio.two.value.avg.df$portfolio.two.value.over.time.avg[2:n.portfolio.two.avg]
yesterday.portfolio.two.avg <- portfolio.two.value.avg.df$portfolio.two.value.over.time.avg[1:n.portfolio.two.avg - 1]
portfolio.two.value.avg.df$daily.return.avg <- c(NA, (today.portfolio.two.avg - yesterday.portfolio.two.avg) / yesterday.portfolio.two.avg)

portfolio.three.value.avg.df <- as.data.frame(portfolio.three.value.over.time.avg)
n.portfolio.three.avg <- nrow(portfolio.three.value.avg.df)
today.portfolio.three.avg <- portfolio.three.value.avg.df$portfolio.three.value.over.time.avg[2:n.portfolio.three.avg]
yesterday.portfolio.three.avg <- portfolio.three.value.avg.df$portfolio.three.value.over.time.avg[1:n.portfolio.three.avg - 1]
portfolio.three.value.avg.df$daily.return.avg <- c(NA, (today.portfolio.three.avg - yesterday.portfolio.three.avg) / yesterday.portfolio.three.avg)

# Build scatterplots
qplot(x = BTC.scope.data$daily.return.avg, y = portfolio.two.value.avg.df$daily.return.avg,
      geom = "point", xlab = "BTC Daily Return (Average)", ylab = "Portfolio Two Daily Return (Average)") +
  ggtitle("BTC Daily Return (Average) vs Portfolio Two Daily Return (Average") +
  theme(plot.title = element_text(hjust = 0.5))

qplot(x = BTC.scope.data$daily.return.avg, y = portfolio.three.value.avg.df$daily.return.avg,
      geom = "point", xlab = "BTC Daily Return (Average)", ylab = "Portfolio Three Daily Return (Average)") +
  ggtitle("BTC Daily Return (Average) vs Portfolio Three Daily Return(Average)") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation calculations
cor(na.omit(portfolio.two.value.avg.df$daily.return.avg), na.omit(BTC.scope.data$daily.return.avg))
print(paste(cor(na.omit(portfolio.two.value.avg.df$daily.return.avg), na.omit(BTC.scope.data$daily.return.avg)), 
            "is the correlation between portfolio two's daily return and the daily return of Bitcoin, using the average"))

cor(na.omit(portfolio.three.value.avg.df$daily.return.avg), na.omit(BTC.scope.data$daily.return.avg))
print(paste(cor(na.omit(portfolio.three.value.avg.df$daily.return.avg), na.omit(BTC.scope.data$daily.return.avg)), 
            "is the correlation between portfolio three's daily return and the daily return of Bitcoin, using the average"))

print("2c doesn't change when using the average. While the values are different, the correlation between portfolio three and BTC remains the strongest")

# Volatility (2d)
BTC.charts.avg <- BTC.charts
n.BTC.charts.avg <- nrow(BTC.charts.avg)
BTC.high.avg <- BTC.charts.avg$high[1:n.BTC.charts.avg]
BTC.low.avg <- BTC.charts.avg$low[1:n.BTC.charts.avg]
BTC.charts.avg$avg.return <- c((BTC.high.avg + BTC.low.avg) / 2)

DASH.charts.avg <- DASH.charts
n.DASH.charts.avg <- nrow(DASH.charts.avg)
DASH.high.avg <- DASH.charts.avg$high[1:n.DASH.charts.avg]
DASH.low.avg <- DASH.charts.avg$low[1:n.DASH.charts.avg]
DASH.charts.avg$avg.return <- c((DASH.high.avg + DASH.low.avg) / 2)

ETH.charts.avg <- ETH.charts
n.ETH.charts.avg <- nrow(ETH.charts.avg)
ETH.high.avg <- ETH.charts.avg$high[1:n.ETH.charts.avg]
ETH.low.avg <- ETH.charts.avg$low[1:n.ETH.charts.avg]
ETH.charts.avg$avg.return <- c((ETH.high.avg + ETH.low.avg) / 2)

LTC.charts.avg <- LTC.charts
n.LTC.charts.avg <- nrow(LTC.charts.avg)
LTC.high.avg <- LTC.charts.avg$high[1:n.LTC.charts.avg]
LTC.low.avg <- LTC.charts.avg$low[1:n.LTC.charts.avg]
LTC.charts.avg$avg.return <- c((LTC.high.avg + LTC.low.avg) / 2)

PPC.charts.avg <- PPC.charts
n.PPC.charts.avg <- nrow(PPC.charts.avg)
PPC.high.avg <- PPC.charts.avg$high[1:n.PPC.charts.avg]
PPC.low.avg <- PPC.charts.avg$low[1:n.PPC.charts.avg]
PPC.charts.avg$avg.return <- c((PPC.high.avg + PPC.low.avg) / 2)

XLM.charts.avg <- XLM.charts
n.XLM.charts.avg <- nrow(XLM.charts.avg)
XLM.high.avg <- XLM.charts.avg$high[1:n.XLM.charts.avg]
XLM.low.avg <- XLM.charts.avg$low[1:n.XLM.charts.avg]
XLM.charts.avg$avg.return <- c((XLM.high.avg + XLM.low.avg) / 2)

XRP.charts.avg <- XRP.charts
n.XRP.charts.avg <- nrow(XRP.charts.avg)
XRP.high.avg <- XRP.charts.avg$high[1:n.XRP.charts.avg]
XRP.low.avg <- XRP.charts.avg$low[1:n.XRP.charts.avg]
XRP.charts.avg$avg.return <- c((XRP.high.avg + XRP.low.avg) / 2)

BTC.precovid.data.avg <- filter(BTC.charts.avg, date >= start.date & date <= end.precovid.date)
DASH.precovid.data.avg <- filter(DASH.charts.avg, date >= start.date & date <= end.precovid.date)
ETH.precovid.data.avg <- filter(ETH.charts.avg, date >= start.date & date <= end.precovid.date)
LTC.precovid.data.avg <- filter(LTC.charts.avg, date >= start.date & date <= end.precovid.date)
PPC.precovid.data.avg <- filter(PPC.charts.avg, date >= start.date & date <= end.precovid.date)
XLM.precovid.data.avg <- filter(XLM.charts.avg, date >= start.date & date <= end.precovid.date)
XRP.precovid.data.avg <- filter(XRP.charts.avg, date >= start.date & date <= end.precovid.date)

n.BTC.precovid.avg <- nrow(BTC.precovid.data.avg)
n.DASH.precovid.avg <- nrow(DASH.precovid.data.avg)
n.ETH.precovid.avg <- nrow(ETH.precovid.data.avg)
n.LTC.precovid.avg <- nrow(LTC.precovid.data.avg)
n.PPC.precovid.avg <- nrow(PPC.precovid.data.avg)
n.XLM.precovid.avg <- nrow(XLM.precovid.data.avg)
n.XRP.precovid.avg <- nrow(XRP.precovid.data.avg)

portfolio.one.value.precovid.avg <- BTC.shares.avg * BTC.precovid.data.avg[1:n.BTC.precovid.avg, "avg.return"]

portfolio.two.value.precovid.avg <- (XRP.shares.avg * XRP.precovid.data.avg[1:n.XRP.precovid.avg, "avg.return"]) + 
  (LTC.shares.avg * LTC.precovid.data.avg[1:n.LTC.precovid.avg, "avg.return"])

portfolio.three.value.precovid.avg <- (ETH.shares.avg * ETH.precovid.data.avg[1:n.ETH.precovid.avg, "avg.return"]) + 
  (DASH.shares.avg * DASH.precovid.data.avg[1:n.DASH.precovid.avg, "avg.return"]) + 
  (PPC.shares.avg * PPC.precovid.data.avg[1:n.PPC.precovid.avg, "avg.return"]) + 
  (XLM.shares.avg * XLM.precovid.data.avg[1:n.XLM.precovid.avg, "avg.return"])

portfolio.one.precovid.avg.df <- as.data.frame(portfolio.one.value.precovid.avg)
n.portfolio.one.precovid.avg <- nrow(portfolio.one.precovid.avg.df)
today.portfolio.one.precovid.avg <- portfolio.one.precovid.avg.df$portfolio.one.value.precovid.avg[2:n.portfolio.one.precovid.avg]
yesterday.portfolio.one.precovid.avg <- portfolio.one.precovid.avg.df$portfolio.one.value.precovid.avg[1:n.portfolio.one.precovid.avg - 1]
portfolio.one.precovid.avg.df$daily.return <- c(NA, (today.portfolio.one.precovid.avg - yesterday.portfolio.one.precovid.avg) / yesterday.portfolio.one.precovid.avg)

portfolio.two.precovid.avg.df <- as.data.frame(portfolio.two.value.precovid.avg)
n.portfolio.two.precovid.avg <- nrow(portfolio.two.precovid.avg.df)
today.portfolio.two.precovid.avg <- portfolio.two.precovid.avg.df$portfolio.two.value.precovid.avg[2:n.portfolio.two.precovid.avg]
yesterday.portfolio.two.precovid.avg <- portfolio.two.precovid.avg.df$portfolio.two.value.precovid.avg[1:n.portfolio.two.precovid.avg - 1]
portfolio.two.precovid.avg.df$daily.return <- c(NA, (today.portfolio.two.precovid.avg - yesterday.portfolio.two.precovid.avg) / yesterday.portfolio.two.precovid.avg)

portfolio.three.precovid.avg.df <- as.data.frame(portfolio.three.value.precovid.avg)
n.portfolio.three.precovid.avg <- nrow(portfolio.three.precovid.avg.df)
today.portfolio.three.precovid.avg <- portfolio.three.precovid.avg.df$portfolio.three.value.precovid.avg[2:n.portfolio.three.precovid.avg]
yesterday.portfolio.three.precovid.avg <- portfolio.three.precovid.avg.df$portfolio.three.value.precovid.avg[1:n.portfolio.three.precovid.avg - 1]
portfolio.three.precovid.avg.df$daily.return <- c(NA, (today.portfolio.three.precovid.avg - yesterday.portfolio.three.precovid.avg) / yesterday.portfolio.three.precovid.avg)

sd(na.omit(portfolio.one.precovid.avg.df$daily.return))
sd(na.omit(portfolio.two.precovid.avg.df$daily.return))
sd(na.omit(portfolio.three.precovid.avg.df$daily.return))

print(paste(sd(na.omit(portfolio.two.precovid.avg.df$daily.return)), 
            "is the volatility of portfolio two between December 1, 2017, and December 31, 2019, using the average. It remains the highest."))

BTC.postcovid.data.avg <- filter(BTC.charts.avg, date >= start.postcovid.date & date <= end.date)
DASH.postcovid.data.avg <- filter(DASH.charts.avg, date >= start.postcovid.date & date <= end.date)
ETH.postcovid.data.avg <- filter(ETH.charts.avg, date >= start.postcovid.date & date <= end.date)
LTC.postcovid.data.avg <- filter(LTC.charts.avg, date >= start.postcovid.date & date <= end.date)
PPC.postcovid.data.avg <- filter(PPC.charts.avg, date >= start.postcovid.date & date <= end.date)
XLM.postcovid.data.avg <- filter(XLM.charts.avg, date >= start.postcovid.date & date <= end.date)
XRP.postcovid.data.avg <- filter(XRP.charts.avg, date >= start.postcovid.date & date <= end.date)

n.BTC.postcovid.avg <- nrow(BTC.postcovid.data.avg)
n.DASH.postcovid.avg <- nrow(DASH.postcovid.data.avg)
n.ETH.postcovid.avg <- nrow(ETH.postcovid.data.avg)
n.LTC.postcovid.avg <- nrow(LTC.postcovid.data.avg)
n.PPC.postcovid.avg <- nrow(PPC.postcovid.data.avg)
n.XLM.postcovid.avg <- nrow(XLM.postcovid.data.avg)
n.XRP.postcovid.avg <- nrow(XRP.postcovid.data.avg)

portfolio.one.value.postcovid.avg <- BTC.shares.avg * BTC.postcovid.data.avg[1:n.BTC.postcovid.avg, "avg.return"]

portfolio.two.value.postcovid.avg <- (XRP.shares.avg * XRP.postcovid.data.avg[1:n.XRP.postcovid.avg, "avg.return"]) + 
  (LTC.shares.avg * LTC.postcovid.data.avg[1:n.LTC.postcovid.avg, "avg.return"])

portfolio.three.value.postcovid.avg <- (ETH.shares.avg * ETH.postcovid.data.avg[1:n.ETH.postcovid.avg, "avg.return"]) + 
  (DASH.shares.avg * DASH.postcovid.data.avg[1:n.DASH.postcovid.avg, "avg.return"]) + 
  (PPC.shares.avg * PPC.postcovid.data.avg[1:n.PPC.postcovid.avg, "avg.return"]) + 
  (XLM.shares.avg * XLM.postcovid.data.avg[1:n.XLM.postcovid.avg, "avg.return"])

portfolio.one.postcovid.avg.df <- as.data.frame(portfolio.one.value.postcovid.avg)
n.portfolio.one.postcovid.avg <- nrow(portfolio.one.postcovid.avg.df)
today.portfolio.one.postcovid.avg <- portfolio.one.postcovid.avg.df$portfolio.one.value.postcovid.avg[2:n.portfolio.one.postcovid.avg]
yesterday.portfolio.one.postcovid.avg <- portfolio.one.postcovid.avg.df$portfolio.one.value.postcovid.avg[1:n.portfolio.one.postcovid.avg - 1]
portfolio.one.postcovid.avg.df$daily.return <- c(NA, (today.portfolio.one.postcovid.avg - yesterday.portfolio.one.postcovid.avg) / yesterday.portfolio.one.postcovid.avg)

portfolio.two.postcovid.avg.df <- as.data.frame(portfolio.two.value.postcovid.avg)
n.portfolio.two.postcovid.avg <- nrow(portfolio.two.postcovid.avg.df)
today.portfolio.two.postcovid.avg <- portfolio.two.postcovid.avg.df$portfolio.two.value.postcovid.avg[2:n.portfolio.two.postcovid.avg]
yesterday.portfolio.two.postcovid.avg <- portfolio.two.postcovid.avg.df$portfolio.two.value.postcovid.avg[1:n.portfolio.two.postcovid.avg - 1]
portfolio.two.postcovid.avg.df$daily.return <- c(NA, (today.portfolio.two.postcovid.avg - yesterday.portfolio.two.postcovid.avg) / yesterday.portfolio.two.postcovid.avg)

portfolio.three.postcovid.avg.df <- as.data.frame(portfolio.three.value.postcovid.avg)
n.portfolio.three.postcovid.avg <- nrow(portfolio.three.postcovid.avg.df)
today.portfolio.three.postcovid.avg <- portfolio.three.postcovid.avg.df$portfolio.three.value.postcovid.avg[2:n.portfolio.three.postcovid.avg]
yesterday.portfolio.three.postcovid.avg <- portfolio.three.postcovid.avg.df$portfolio.three.value.postcovid.avg[1:n.portfolio.three.postcovid.avg - 1]
portfolio.three.postcovid.avg.df$daily.return <- c(NA, (today.portfolio.three.postcovid.avg - yesterday.portfolio.three.postcovid.avg) / yesterday.portfolio.three.postcovid.avg)

sd(na.omit(portfolio.one.postcovid.avg.df$daily.return))
sd(na.omit(portfolio.two.postcovid.avg.df$daily.return))
sd(na.omit(portfolio.three.postcovid.avg.df$daily.return))

print(paste(sd(na.omit(portfolio.two.postcovid.avg.df$daily.return)), 
            "is the volatility of portfolio two between January 1, 2020, and September 1, 2022 using averages. It remains the highest post-covid."))

print("2d doesn't change when using average. While the actual value decreases, portfolio two remains as the highest value")

# Relative to BTC (2e)
portfolio.two.value.avg.df$btc.daily.return <- BTC.scope.data$daily.return.avg
portfolio.two.value.avg.btc.positive <- filter(portfolio.two.value.avg.df, btc.daily.return > 0)
portfolio.two.value.avg.btc.negative <- filter(portfolio.two.value.avg.df, btc.daily.return < 0)

portfolio.three.value.avg.df$btc.daily.return <- BTC.scope.data$daily.return.avg
portfolio.three.value.avg.btc.positive <- filter(portfolio.three.value.avg.df, btc.daily.return > 0)
portfolio.three.value.avg.btc.negative <- filter(portfolio.three.value.avg.df, btc.daily.return < 0)

mean(portfolio.two.value.avg.btc.positive$daily.return.avg, na.rm = TRUE)
print(paste(mean(portfolio.two.value.avg.btc.positive$daily.return.avg, na.rm = TRUE), 
            "is the mean daily return of portfolio two when BTC daily return was positive, and using averages."))

mean(portfolio.two.value.avg.btc.negative$daily.return.avg, na.rm = TRUE)
print(paste(mean(portfolio.two.value.avg.btc.negative$daily.return.avg, na.rm = TRUE), 
            "is the mean daily return of portfolio two when BTC daily return was negative, and using averages."))

mean(portfolio.three.value.avg.btc.positive$daily.return.avg, na.rm = TRUE)
print(paste(mean(portfolio.three.value.avg.btc.positive$daily.return.avg, na.rm = TRUE), 
            "is the mean daily return of portfolio three when BTC daily return was positive, and using averages."))

mean(portfolio.three.value.avg.btc.negative$daily.return.avg, na.rm = TRUE)
print(paste(mean(portfolio.three.value.avg.btc.negative$daily.return.avg, na.rm = TRUE), 
            "is the mean daily return of portfolio three when BTC daily return was negative, and using averages."))

print("Overall, neither portfolio dominated the other, as two still had lower negatives, and three still had higher positives using averages.")

print("2e doesn't change when using average. While the actual values are different, neither portfolio becomes dominant.")