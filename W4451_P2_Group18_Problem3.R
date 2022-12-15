
#3.1 Plot daily stock prices of 4 companies

alv = read.csv("ALV.csv")
dai = read.csv("DAI.csv")
dpw = read.csv("DPW.csv")
hei = read.csv("HEI.csv")

head(alv)
head(dai)
head(dpw)
head(hei)

tail(alv)
tail(dai)
tail(dpw)
tail(hei)

n = dim(alv)[1]

alv_close = alv$Close[1:n]
dai_Close = dai$Close[1:n]
dpw_Close = dpw$Close[1:n]
hei_Close = hei$Close[1:n]

alv_time = seq(from = "2019.5", to = "2021.5", length.out = n)
plot(alv_time, alv_close, type = "l", xlab = "Date", ylab = "Price",
     main = "Allianz SE Closing Prices in EUR")

dai_time = seq(from = "2019.5", to = "2021.5", length.out = n)
plot(dai_time, dai_Close, type = "l", xlab = "Date", ylab = "Price",
     main = "Daimler AG Closing Prices in EUR")

dpw_time = seq(from = "2019.5", to = "2021.5", length.out = n)
plot(dpw_time, dpw_Close, type = "l", xlab = "Date", ylab = "Price",
     main = "Deutsche Post AG Closing Prices in EUR")


hei_time = seq(from = "2019.5", to = "2021.5", length.out = n)
plot(hei_time, hei_Close, type = "l", xlab = "Date", ylab = "Price",
     main = "HeidelbergCement AG Closing Prices in EUR")


#3.2 Computing Returns and Reporting Garch and Aparch model

library(fGarch)

#Log transformations

rt_alv = diff(log(alv_close))
rt_dai = diff(log(dai_Close))
rt_dpw = diff(log(dpw_Close))
rt_hei = diff(log(hei_Close))

#Plot of the return series

par(mfrow = c(2, 2))

alv_time2 = alv_time[2:n]                   
plot(alv_time2, rt_alv, type = "l", xlab = "Year", ylab = "Returns", 
     main = "Returns of Closing prices for Allianz SE stocks", col = "red")

dai_time2 = dai_time[2:n]
plot(dai_time2, rt_dai, type = "l", xlab = "Year", ylab = "Returns", 
     main = "Returns of Closing prices for Daimler AG stocks", col = "red")

dpw_time2 = dpw_time[2:n]
plot(dpw_time2, rt_dpw, type = "l", xlab = "Year", ylab = "Returns", 
     main = "Returns of Closing prices for Deutsche Post AG stocks", col = "red")

hei_time2 = hei_time[2:n]
plot(hei_time2, rt_hei, type = "l", xlab = "Year", ylab = "Returns", 
     main = "Returns of Closing prices for HeidelbergCement AG", col = "red")

#Estimating the best GARCH model

garchFit(~garch(1, 2), rt_alv, trace = FALSE)@fit$ics[2]
garchFit(~garch(1, 2), rt_dai, trace = FALSE)@fit$ics[2]
garchFit(~garch(1, 2), rt_dpw, trace = FALSE)@fit$ics[2]
garchFit(~garch(1, 2), rt_hei, trace = FALSE)@fit$ics[2]

GARCH_alv = garchFit(~garch(1, 2), rt_alv, trace = FALSE)
GARCH_alv@fit$coef

#Estimating the best APARCH model

garchFit(~aparch(1, 2), rt_alv, trace = FALSE)@fit$ics[2]
garchFit(~aparch(1, 2), rt_dai, trace = FALSE)@fit$ics[2]
garchFit(~aparch(1, 2), rt_dpw, trace = FALSE)@fit$ics[2]
garchFit(~aparch(1, 2), rt_hei, trace = FALSE)@fit$ics[2]

APARCH_alv = garchFit(~aparch(1, 2), rt_alv, trace = FALSE)
APARCH_alv@fit$coef

#3.3 Estimating 0.95 Var and 0.95 ES for the GARCH and APARCH model of all companies

# Loss/Risk variable for GARCH and APARCH models of all companies

L_alv = -rt_alv
L_dai = -rt_dai
L_dpw = -rt_dpw
L_hei = -rt_hei

# Assigning GARCH and APARCH variables for all remaining companies

GARCH_dai = garchFit(~garch(1, 2), rt_dai, trace = FALSE)
GARCH_dpw = garchFit(~garch(1, 2), rt_dpw, trace = FALSE)
GARCH_hei = garchFit(~garch(1, 2), rt_hei, trace = FALSE)

APARCH_dai = garchFit(~aparch(1, 2), rt_dai, trace = FALSE)
APARCH_dpw = garchFit(~aparch(1, 2), rt_dpw, trace = FALSE)
APARCH_hei = garchFit(~aparch(1, 2), rt_hei, trace = FALSE)

# Estimate Conditional Volatility for all companies

sigma_n_garch_alv = GARCH_alv@sigma.t
sigma_n_aparch_alv = APARCH_alv@sigma.t

sigma_n_garch_dai = GARCH_dai@sigma.t
sigma_n_aparch_dai = APARCH_dai@sigma.t

sigma_n_garch_dpw = GARCH_dpw@sigma.t
sigma_n_aparch_dpw = APARCH_dpw@sigma.t

sigma_n_garch_hei = GARCH_hei@sigma.t
sigma_n_aparch_hei = APARCH_hei@sigma.t

# calculate VaR and ES for all companies

#0.95 VAR and ES for Allianz SE
VaR_n95_garch_alv = qnorm(0.95) * sigma_n_garch_alv
ES_n95_garch_alv = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_garch_alv

VaR_n95_aparch_alv = qnorm(0.95) * sigma_n_aparch_alv
ES_n95_aparch_alv = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_aparch_alv

#0.95 VAR and ES for Daimler AG
VaR_n95_garch_dai = qnorm(0.95) * sigma_n_garch_dai
ES_n95_garch_dai = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_garch_dai

VaR_n95_aparch_dai = qnorm(0.95) * sigma_n_aparch_dai
ES_n95_aparch_dai = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_aparch_dai

#0.95 VAR and ES for Deutsche Post AG
VaR_n95_garch_dpw = qnorm(0.95) * sigma_n_garch_dpw
ES_n95_garch_dpw = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_garch_dpw

VaR_n95_aparch_dpw = qnorm(0.95) * sigma_n_aparch_dpw
ES_n95_aparch_dpw = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_aparch_dpw

#0.95 VAR and ES for Heidelberg Cement AG
VaR_n95_garch_hei = qnorm(0.95) * sigma_n_garch_hei
ES_n95_garch_hei = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_garch_hei

VaR_n95_aparch_hei = qnorm(0.95) * sigma_n_aparch_hei
ES_n95_aparch_hei = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_n_aparch_hei

# plot the results of return series with VAR and ES for all companies

#Plot of 0.95 VAR and ES for Allianz SE return series

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)
plot(alv_time2, L_alv, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for GARCH model of Allianz SE")
lines(alv_time2, VaR_n95_garch_alv, col = "red")
lines(alv_time2, ES_n95_garch_alv, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

plot(alv_time2, L_alv, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for APARCH model of Allianz SE")
lines(alv_time2, VaR_n95_aparch_alv, col = "red")
lines(alv_time2, ES_n95_aparch_alv, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

#Plot of 0.95 VAR and ES for Daimler AG return series

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)
plot(dai_time2, L_dai, ylab = "Loss/Risk", type = "l", ylim = c(-0.30, 0.30))
title("95% Risk Measures under a Normal Distribution for GARCH model of Daimler AG")
lines(dai_time2, VaR_n95_garch_dai, col = "red")
lines(dai_time2, ES_n95_garch_dai, col = "blue")
legend(2021,0.30, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

plot(dai_time2, L_dai, ylab = "Loss/Risk", type = "l", ylim = c(-0.30, 0.30))
title("95% Risk Measures under a Normal Distribution for APARCH model of Daimler AG")
lines(dai_time2, VaR_n95_aparch_dai, col = "red")
lines(dai_time2, ES_n95_aparch_dai, col = "blue")
legend(2021,0.30, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

#Plot of 0.95 VAR and ES for Deutsche Post AG return series

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)
plot(dpw_time2, L_dpw, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for GARCH model of Deutsche Post AG")
lines(dpw_time2, VaR_n95_aparch_dpw, col = "red")
lines(dpw_time2, ES_n95_garch_dpw, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

plot(dpw_time2, L_dpw, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for APARCH model of Deutsche Post AG ")
lines(dpw_time2, VaR_n95_aparch_dpw, col = "red")
lines(dpw_time2, ES_n95_aparch_dpw, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

#Plot of 0.95 VAR and ES for Heidelberg Cement AG return series

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)
plot(hei_time2, L_hei, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for GARCH model of Heidelberg Cement AG")
lines(hei_time2, VaR_n95_garch_hei, col = "red")
lines(hei_time2, ES_n95_garch_hei, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

plot(hei_time2, L_hei, ylab = "Loss/Risk", type = "l", ylim = c(-0.15, 0.20))
title("95% Risk Measures under a Normal Distribution for APARCH model of Heidelberg Cement AG")
lines(hei_time2, VaR_n95_aparch_hei, col = "red")
lines(hei_time2, ES_n95_aparch_hei, col = "blue")
legend(2021,0.20, legend = c("VaR", "ES"), col = c("red", "blue"),
       lty = c(1, 1))

#3.4 Estimating the number of exceptions (point-over-threshold) for both models and all companies using the traffic light approach

#Counting POT for Allianz SE

POT_VaR_n95_garch_alv = sum(as.numeric(L_alv > VaR_n95_garch_alv))
pbinom(POT_VaR_n95_garch_alv, size = n, prob = 0.05)
#Assign Green

POT_VaR_n95_aparch_alv = sum(as.numeric(L_alv > VaR_n95_aparch_alv))
pbinom(POT_VaR_n95_aparch_alv, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_garch_alv = sum(as.numeric(L_alv > ES_n95_garch_alv))
pbinom(POT_ES_n95_garch_alv, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_aparch_alv = sum(as.numeric(L_alv > ES_n95_aparch_alv))
pbinom(POT_ES_n95_aparch_alv, size = n, prob = 0.05)
#Assign Green

#Counting POT for Daimler AG

POT_VaR_n95_garch_dai = sum(as.numeric(L_dai > VaR_n95_garch_dai))
pbinom(POT_VaR_n95_garch_dai, size = n, prob = 0.05)
#Assign Green

POT_VaR_n95_aparch_dai = sum(as.numeric(L_dai > VaR_n95_aparch_dai))
pbinom(POT_VaR_n95_aparch_dai, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_garch_dai = sum(as.numeric(L_dai > ES_n95_garch_dai))
pbinom(POT_ES_n95_garch_dai, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_aparch_dai = sum(as.numeric(L_dai > ES_n95_aparch_dai))
pbinom(POT_ES_n95_aparch_dai, size = n, prob = 0.05)
#Assign Green

#Counting POT for Deutsche post AG

POT_VaR_n95_garch_dpw = sum(as.numeric(L_dpw > VaR_n95_garch_dpw))
pbinom(POT_VaR_n95_garch_dpw, size = n, prob = 0.05)
#Assign Green

POT_VaR_n95_aparch_dpw = sum(as.numeric(L_dpw > VaR_n95_aparch_dpw))
pbinom(POT_VaR_n95_aparch_dpw, size = n, prob = 0.05)
#Assign Yellow

POT_ES_n95_garch_dpw = sum(as.numeric(L_dpw > ES_n95_garch_dpw))
pbinom(POT_ES_n95_garch_dpw, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_aparch_dpw = sum(as.numeric(L_dpw > ES_n95_aparch_dpw))
pbinom(POT_ES_n95_aparch_dpw, size = n, prob = 0.05)
#Assign Green

#Counting POT for Heidelberg Cement AG

POT_VaR_n95_garch_hei = sum(as.numeric(L_hei > VaR_n95_garch_hei))
pbinom(POT_VaR_n95_garch_hei, size = n, prob = 0.05)
#Assign Green

POT_VaR_n95_aparch_hei = sum(as.numeric(L_hei > VaR_n95_aparch_hei))
pbinom(POT_VaR_n95_aparch_hei, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_garch_hei = sum(as.numeric(L_hei > ES_n95_garch_hei))
pbinom(POT_ES_n95_garch_hei, size = n, prob = 0.05)
#Assign Green

POT_ES_n95_aparch_hei = sum(as.numeric(L_hei > ES_n95_aparch_hei))
pbinom(POT_ES_n95_aparch_hei, size = n, prob = 0.05)
#Assign Green




