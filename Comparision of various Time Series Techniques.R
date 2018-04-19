library(forecast)
library(stats)
library(data.table)
library(TTR)
library(tseries)
library(astsa)
library(xts)

# ASSIGNMENT PART 1

library(xlsx)
demanddata <- read.xlsx("ProductDemand.xlsx",sheetName = "Data",header=TRUE)

# STEP #1: Read the demand data, convert to time series object and plot
ts.demand <- ts(demanddata, frequency = 12, start = c(2013,1))
plot(ts.demand,xlab="Year", ylab = "Product Demand")

# STEP #2: Stationarity Check - Dicky-Fuller Test

# Null Hypothesis        H0: Data is not Stationary
# Alternative Hypothesis H1: Data is Stationary

adf.test(ts.demand,alternative="stationary")

# Since p-value is 0.2965 > 0.05, Null Hypothesis is rejected; Data is stationary
# Differencing is not required

# STEP #3: Decompose the time series

decompose.demand <- decompose(ts.demand, "multiplicative")
plot(decompose.demand)

# STEP #4: Identify the estimated seasonal index
Seasonal_Index <- decompose.demand$figure
Seasonal_Index

# STEP #4: Identify the deseasonalized demand
Deseasonalized_demand <- ts.demand / Seasonal_Index
round(Deseasonalized_demand,0)
plot(round(Deseasonalized_demand,0))

# STEP #5: ARMA Model

##### STEP #5a : Identifying the no. of AR and MA terms
# acf2 function shows the plots of both acf and pacf

acf(ts.demand,max.lag=35)

##### STEP #5b : Arima(1,0,0) model

AR1 <- arima(ts.demand,order=c(1,0,0))
accuracy(fitted(AR1),ts.demand)

##### STEP #5c : Check model fitment
qqnorm(AR1$residuals)
qqline(AR1$residuals)
acf(AR1$residuals,max.lag=35)
Box.test(AR1$residuals, lag=20, fitdf=1, type='Ljung-Box')

# its clearly evident that the residuals are normally distributed and it does not have any auto correlation.
#further Ljung - Box test proves that the model has good fit(from p- value, null hypothesis is proved)

##### STEP #5d : Automated Arima
AR2 <- auto.arima(ts.demand,stepwise=FALSE,approximation=FALSE)
arima.performance <- accuracy(fitted(AR2),ts.demand)
arima.mape <- arima.performance[5]
arima.theil <- arima.performance[7] 

# STEP #6: Simple Moving Average Model
sma.ts.demand <- round(SMA(ts.demand,n=2),0)
sma.ts.demand
plot(sma.ts.demand)
sma.performance <- accuracy(sma.ts.demand,ts.demand)
sma.mape <- sma.performance[5]
sma.theil <- sma.performance[7] 
sma.mape
sma.theil

# STEP #7: Weighted Moving Average Model
wma.ts.demand <- round(WMA(ts.demand,n=2),0)
wma.ts.demand
wma.performance <- accuracy(wma.ts.demand,ts.demand)
wma.mape <- wma.performance[5]
wma.theil <- wma.performance[7] 
wma.mape
wma.theil

# STEP #8: Exponential Moving Average Model
ema.ts.demand <- round(EMA(ts.demand,n=2),0)
ema.ts.demand
ema.performance <- accuracy(ema.ts.demand,ts.demand)
ema.mape <- ema.performance[5]
ema.theil <- ema.performance[7] 
ema.mape
ema.theil

# STEP #9: Simple Exponential Smoothing Model
ses.output <- ses(ts.demand)
ses.ts.demand <- round(ses.output$fitted,0)
ses.ts.demand
ses.performance <- accuracy(ses.ts.demand,ts.demand)
ses.mape <- ses.performance[5]
ses.theil <- ses.performance[7] 
ses.mape
ses.theil

# STEP #10: Double Exponential Smoothing Model
des.output <- holt(ts.demand)
des.ts.demand <- round(des.output$fitted,0)
des.ts.demand
des.performance <- accuracy(des.ts.demand,ts.demand)
des.mape <- des.performance[5]
des.theil <- des.performance[7] 
des.mape
des.theil

# STEP #11: Triple Exponential Smoothing Model
tes.output <- HoltWinters(ts.demand,seasonal=c("multiplicative"))
tes.output
tes.ts.demand <- round(tes.output$fitted,0)
tes.ts.demand
tes.performance <- accuracy(tes.ts.demand,ts.demand)
tes.mape <- tes.performance[5]
tes.theil <- tes.performance[7] 
tes.mape
tes.theil

# STEP #12: CONCLUSION

par(mfrow=c(1,1))
plot(ts.demand, type="l", col="black",main="Original Data Vs SMA Output")
lines(sma.ts.demand, col="red", lwd=2)

plot(ts.demand, type="l", col="black",main="Original Data Vs WMA Output")
lines(wma.ts.demand, col="blue",lwd=2)

plot(ts.demand, type="l", col="black",main="Original Data Vs EMA Output")
lines(ema.ts.demand, col="brown",lwd=2)

plot(ts.demand, type="l", col="black",main="Original Data Vs SES Output")
lines(ses.ts.demand, col="green", lwd=2)

plot(ts.demand, type="l", col="black",main="Original Data Vs DES Output")
lines(des.ts.demand, col="yellow",lwd=2)


methods <- c("ARMA","SMA","WMA","EMA","SES","DES","TES")
mape <- c(arima.mape,sma.mape,wma.mape,ema.mape,ses.mape,des.mape,tes.mape)
Theils <- c(arima.theil,sma.theil,wma.theil,ema.theil,ses.theil,des.theil,tes.theil)
summary <- data.frame(methods,mape,Theils)
summary

# ARMA and WMA seems to be a better model