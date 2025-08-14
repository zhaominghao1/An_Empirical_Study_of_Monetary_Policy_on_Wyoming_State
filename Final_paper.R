# ECON 112 Final Paper

rm(list=ls())

setwd("~/Desktop/ECON_112/Final paper")

library(readxl)
library(tseries)
library(vars)

# Open datasets
wyoming_urate <- read_xls("~/Desktop/ECON_112/Final paper/WYUR.xls")
urate <- ts(wyoming_urate$WYUR, start = 1976, frequency = 12)
urate_quart <- aggregate(urate, nfrequency = 4, FUN = "mean")
u <- window(urate_quart, start = c(1997,2), end = c(2022,4))

wyoming_realgdp <- read.table("Wyoming_RealGDP.csv", header = T, sep = ",")
gdp <- ts(wyoming_realgdp$RealGDP, start = 1997, frequency = 4)
growth <- 400*diff(log(gdp))

int <- read_xls("~/Desktop/ECON_112/Final paper/FEDFUNDS.xls")
ffr <- ts(int$FEDFUNDS, start = c(1954,7), frequency = 12)
ffr_quart <- aggregate(ffr, nfrequency = 4, FUN = "mean")
ffrate <- window(ffr_quart, start = c(1997,2), end = c(2022,4))

# plot graphs
plot(u, main = "Wyoming Unemployment rate", ylab = "unemployment rate (%)", col = 'red')
plot(growth, main = "Wyoming real GDP growth", ylab = "growth rate (%)", col = 'black')
plot(ffrate, main = "Federal Funds Rate", ylab = "FFR", col = 'blue')
adf.test(ffrate)
adf.test(u)
adf.test(growth)
acf(ffrate, main = "ACF of FFR")
acf(growth, main = "ACF of growth rate")
acf(u, main = "ACF of unemployment")
pacf(growth, main = "PACF of growth rate")
pacf(u, main = "PACF of unemployment")
pacf(ffrate, main = "PACF of FFR")

# Bind the data together
data.u.ffr <- cbind(ffrate,u)

# Do VAR and plot residuals
VARselect(data.u.ffr,lag.max=15,type="const")
var.u.ffr <- VAR(data.u.ffr, p = 2)
summary(var.u.ffr)
dev.new()
plot(var.u.ffr)

# Structural impulse response function
irf.u.ffr <- irf(var.u.ffr, ortho = TRUE,ci = 0.95, runs = 100, n.ahead = 16)
dev.new()
plot(irf.u.ffr,lwd=2)

# Repeat the same analysis with GDP growth rate
data.ffr.growth <- cbind(growth, ffrate)
VARselect(data.ffr.growth, lag.max = 15, type = "const")
var.growth.ffr <- VAR(data.ffr.growth, p = 2)
summary(var.growth.ffr)
plot(var.growth.ffr)
irf.growth.ffr <- irf(var.growth.ffr, ortho = TRUE, ci = 0.95, runs = 100, n.ahead = 16)
dev.new()
plot(irf.growth.ffr)
