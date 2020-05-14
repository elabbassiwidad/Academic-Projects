install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("astsa")
library(astsa)
getwd()
setwd("/Users/irene/Documents/MasterDS/Statistic/TimeSeries/")
ts=ts(data_g8[,2],start=c(1987,6),end=c(2013,9),frequency=4)
plot(ts)
plot(log(ts))

# Plot of descomposition of series
acf(ts)

#Using stl() function in R: very versatile and robust method for decomposing time series. STL stands for Seasonal and Trend decomposition using Loess (method for estimating nonlinear relationships).
stlDescomp <- stl(ts,s.window = "periodic")
plot(stlDescomp)
addictiveDecomp= decompose(ts,type="additive")
plot(addictiveDecomp)

#Multiplicative decomposition which is equivalent to the additive
#decomposition after Logarithmic transformation.
multiplicativeDecomp= decompose(ts,type="multiplicative")
plot(multiplicativeDecomp)

#forescast 
forecast(ts)
plot(forecast(ts))
forecast::Acf(ts,lag.max = 107)

#ACF and PACF of the remainder part
remainder.ts <- stlDescomp$time.series[,'remainder']
acf(remainder.ts)
pacf(remainder.ts)

#trying to remove seasonal component to observe the cyclic-trend of out data 
removeSeasonalComponet.ts <- ts-stlDescomp$time.series[,1]
plot(removeSeasonalComponet.ts)

# 3.a) Fit an Arima model
plot(log(ts))
plot(ts)
tsdisplay(ts)

# 3.b) Fit an Arima model
tsdisplay(ts, plot.type = "spectrum")
seasonplot(ts)
monthplot(ts)



#3. c)Decide on the values of d and D to make your series stationary.
lamda=BoxCox.lambda(ts)
acf2(diff(BoxCox(ts,lamda)),max.lag = 100)
#ACF and PACF of transformed and differenced data drops quickly with few spikes for big values of lags slightly beyond critical values. The data can be considered stationary. ARMA model can be applied.
ndiffs(ts) #function to estimate necessary number of differencies, confirm the result n=1

#using kpss to test stationarity
kpss.test(diff(ts))  #p-value > 0.05, means stationary

#The p-value is greater than 0.05. The null hypothesis of stationarity around a trend is not rejected.

# The optimal order of differencing is often the order of differencing at which the standard deviation is lowest. 
sd(ts)  #standard deviations
sd(diff(ts)) #this one has smallest value
sd(diff(diff(ts)))

#3-d

model.1=Arima(ts,order=c(1,1,1),include.constant=1)
summary(model.1)
#Diagnostic plot with jarque.bera.test
jarque.bera.test(model.1$residuals)
tsdiag(model.1) 

model.2=Arima(ts,order=c(1,1,2),include.constant=1)
summary(model.2)

model.5=Arima(ts,order=c(1,2,1),include.constant=1)
summary(model.5)  

model.3=Arima(ts,order=c(1,1,3),include.constant=1)
summary(model.3)

model.4=Arima(ts,order=c(2,1,2),include.constant=1)
summary(model.4)

model.5=Arima(ts,order=c(1,1,5),include.constant=1)
summary(model.5)          

#by introducing some more MA terms, reduces the AIC of the fitted model. After trying MA degrees of 1 through 5, it is found that an ARIMA(1,1,5) model has the lowest AIC.
cov2cor(model.1$var.coef)
#we check correlations between coefficients, we find something alarming, there is one high correlation 
# we discard this model and try increasing the order of the autoregressive part instead.
model.6=Arima(ts,order=c(2,1,1),include.constant = 1)
model.6$aicc

tsdiag(model.6) 
jarque.bera.test(model.6$residuals)

#3.f)
plot(forecast(model.1, h=6))

#3.g)

install.packages("vec2dtransf")
library(vec2dtransf)
install.packages("sp")
library(sp)

#getRMSE(model.6)  


#3trying auto.Arima results
auto=auto.arima(ts,max.p=3,max.q=3,max.P=3,max.Q=3)
auto
tsdisplay(auto$residuals, plot.type="spectrum")

