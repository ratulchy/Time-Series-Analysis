#Reading the data sets

amzn = read.csv("AMZN.csv")
head(amzn)

#2.1 Defining the time sequence 

n1 = dim(amzn)[1]
Open_amzn = amzn$Open[n1:1]
time_amzn = seq(from = 2019 + 1/12, to = 2020 + 12/12, length.out = n1)


#2.2 Calculating log returns 

# log transformation
ln_Open_amzn = log(Open_amzn)

# returns
rt_Open_amzn = diff(ln_Open_amzn)
time2_amzn = time_amzn[2:n1]

#Fitting a GARCH(1,1) model to both return series 

library(fGarch)

garch11_Open_amzn = garchFit(~garch(1, 1), rt_Open_amzn, trace = FALSE)
garch11_Open_amzn@fit$ics[2]


#Plot of conditional volatility along with returns for both companies

sigma_t_Open_amzn = garch11_Open_amzn@sigma.t
plot(time2_amzn, rt_Open_amzn, type = "l", xlab = "Year", ylab = "Returns", 
     main = "Returns for Open and Conditional volatility (AMZN)")

lines(time2_amzn, sigma_t_Open_amzn, col = "red", lwd = 2)




