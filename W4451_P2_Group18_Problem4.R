library(fGarch)
library(timeDate)
library(timeSeries)
library(fBasics)

Allianz = read.csv("AllianzStockPrices.csv")
n = dim(Allianz)[1]
day1 = Allianz$X2014.09.22[1:n]
day2 = Allianz$X2014.09.23[1:n]
day3 = Allianz$X2014.09.24[1:n]

#time variable 
time = seq(from=as.POSIXct("2014-09-22 09:00"),to=as.POSIXct("2014-09-24 17:30"), length.out = n)

#Task1

par(mfrow = c(3, 1))
plot(time, day1, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Stock 09-22-2014", col = "green")
plot(time, day2, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Stock 09-23-2014", col = "green")
plot(time, day3, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Stock 09-24-2014", col = "green")

#Task2 
#returns 
day1_returns = diff(log(day1))
day2_returns = diff(log(day2))
day3_returns = diff(log(day3))

time2 = time[2:n]

par(mfrow = c(3, 1))
plot(time2, day1_returns, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Returns 09-22-2014", col = "green")
plot(time2, day2_returns, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Returns 09-23-2014", col = "green")
plot(time2, day3_returns, type = "l", xlab = "Trading Daytime", ylab = "Price", 
     main = "Allianz Returns 09-24-2014", col = "green")

voladay1=var(day1_returns)
voladay2=var(day2_returns)
voladay3=var(day3_returns)

voladay1
voladay2
voladay3

