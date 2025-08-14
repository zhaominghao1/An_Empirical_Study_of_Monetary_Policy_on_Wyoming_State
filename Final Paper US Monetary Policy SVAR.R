# Econ 112 Macroeconomic Data Analysis

# Structural VARs: An Application to U.S. Monetary Policy

# written by Giacomo Rondina, last updated August 2024

library(tseries)
library(vars)

data.gdp=read.table("real_gdp_econ_112.csv",header=T,sep=",")

rgdp=ts(data.gdp[,2],start=1947,frequency=4)

data.ff=read.table("fed_funds_rate_econ_112.csv",header=T,sep=',')

ffrate=ts(data.ff[,2],frequency=12,start=c(1954, 7))

int=aggregate(ffrate,nfrequency=4,mean)


# compute quarterly real GDP growth rate

dlrgdp = 400*diff(log(rgdp))



# ensure data is of same length (also use the window command if you want to exclude the COVID period)

# check initial date and final date, for the date used here initial common date is quarter three in 1954
# hence c(1954,3)

y=window(dlrgdp,start= c(1954,3),end=c(2024,2))

int = window(int,start= c(1954,3),end=c(2024,2))


dev.new()
plot(y,col="black",lwd=2)
lines(int,col="blue",lwd=2)

#VAR analysis begins here

#bind data (pay attention to order)

data.y.int<-ts(cbind(y,int))

#compute information criteria and select order of VAR

VARselect(data.y.int, lag.max = 16, type = "const")

#run VAR estimation and report summary

var.y.int = VAR(data.y.int,p=6)

summary(var.y.int)

#diagnostic: fit and residuals

dev.new()
plot(var.y.int)

#Structural Impulse Response using Sims (1980) solution (Cholesky decomposition)

irf.y.int<-irf(var.y.int, ortho = TRUE, ci = 0.95, runs = 100, n.ahead = 16)
plot(irf.y.int,lwd=2)

#What do you conclude from Impulse Response about effect of U.S. Monetary Policy on Economic Activity?

#What theoretical assumptions are implemented by the Cholesky decomposition?


