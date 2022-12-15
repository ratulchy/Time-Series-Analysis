require(quantmod)
require(tidyquant) 
require(plotly) 
require(timetk) 
require(tidyverse)

ARsim = read.csv("W4451 - Project 2 - AR sim.csv")
n = length(ARsim)

# set up matrices for storage of BIC
BIC = matrix(0, nrow = 1, ncol = 11)


# loop
for (p in 0:10)
{
    model = arima(ARsim, order = c(p, 0, 0))
    BIC[1, p + 1] = -2 * model$loglik + log(n)*(p)
  
}
which(BIC == min(BIC), arr.ind = TRUE)
BIC
#optimal order is AR(10)


arima(ARsim, order = c(10, 0, 0))

