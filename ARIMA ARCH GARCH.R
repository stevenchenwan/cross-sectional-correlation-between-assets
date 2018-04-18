appl <-read.csv("2010.1.1 TO 2017.6.101 TEC IND.csv", header=T)

appl.close=appl$AAPL #read and store adj close price in original file

#Differencing the original series
plot(appl.close,type='l',main='EA Stock Price')
diff.appl=diff(appl.close)

#Plot differences of original series
plot(diff.appl,type='l',main='Difference EA')

# Take log of original series and plot the log price
log.appl=log(appl.close)
plot(log.appl,type='l',main='Log EA')

# Differencing log price and plot
difflog.appl=diff(log.appl)
#difflog.appl=diff(difflog.appl)
plot(difflog.appl,type='l',main='Difference Log EA')

#ACF and PACF of Log Apple
acf.appl=acf(log.appl,main='ACF EA',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(log.appl,main='PACF EA',lag.max=100,ylim=c(-0.5,1))

#ACF and PACF of Differenced log Apple
acf.appl=acf(difflog.appl,main='ACF Difference Log EA',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(difflog.appl,main='PACF Difference Log EA',lag.max=100,ylim=c(-0.5,1))

arima212=arima(log.appl,order=c(2,1,2))
summary(arima212)
library(tseries)
library(forecast)
auto.arima(log.appl)

arima(log.appl,order=c(0,1,0))
arima(log.appl,order=c(0,1,1))
arima(log.appl,order=c(0,1,2))
arima212=arima(log.appl,order=c(0,1,0))




#Codes to generate result for Squared Residuals:
res.arima212=arima212$res
squared.res.arima212=res.arima212^2
par(mfcol=c(3,1))
plot(squared.res.arima212,main='Squared Residuals')
acf.squared212=acf(squared.res.arima212,main='ACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.arima212,main='PACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))


#R codes to perform ARCH/GARCH model:
arch08=garch(res.arima212,order=c(0,1),trace=F)
AIC(arch08)
loglik08=logLik(arch08)
summary(arch08)


par(mfcol=c(1,1))

# Generate 1-step forecast, 100-step forecast, and plot of forecast:
forecast212step1=forecast(arima212,1,level=95)
forecast212=forecast(arima212,100,level=95) 
plot(forecast212)

# Compute ht, conditional variance:
ht.arch08=arch08$fit[,1]^2 #use 1st column of fit
plot(ht.arch08,main='Conditional variances')

# Generate plot of Log Price, 95% Upper and Lower limit
fit212=fitted.values(arima212)
low=fit212-1.96*sqrt(ht.arch08)
high=fit212+1.96*sqrt(ht.arch08)
plot(log.appl,type='l',main='Log Apple,Low,High')
lines(low,col='red')
lines(high,col='blue')





archres=res.arima212/sqrt(ht.arch08)
qqnorm(archres,main='ARIMA-ARCH Residuals')
qqline(archres)




qqnorm(res.arima212)

qqline(res.arima212)


########################################################################################################################



appl <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)

appl.close=appl$AAPL #read and store adj close price in original file

#Differencing the original series
plot(appl.close,type='l',main='EA Stock Price')
diff.appl=diff(appl.close)

#Plot differences of original series
plot(diff.appl,type='l',main='Difference EA')

# Take log of original series and plot the log price
log.appl=log(appl.close)
plot(log.appl,type='l',main='Log EA')

# Differencing log price and plot
difflog.appl=diff(log.appl)
difflog.appl=diff(difflog.appl)
plot(difflog.appl,type='l',main='Difference Log EA')

#ACF and PACF of Log Apple
acf.appl=acf(log.appl,main='ACF EA',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(log.appl,main='PACF EA',lag.max=100,ylim=c(-0.5,1))

#ACF and PACF of Differenced log Apple
acf.appl=acf(difflog.appl,main='ACF Difference Log EA',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(difflog.appl,main='PACF Difference Log EA',lag.max=100,ylim=c(-0.5,1))

arima212=arima(log.appl,order=c(2,1,2))
summary(arima212)
library(tseries)
library(forecast)
auto.arima(log.appl)

arima(log.appl,order=c(2,1,2))
arima212=arima(log.appl,order=c(0,1,1))




arima(log.appl,order=c(0,1,0))
arima(log.appl,order=c(0,1,1))
arima(log.appl,order=c(0,1,2))
arima(log.appl,order=c(0,1,3))
arima(log.appl,order=c(0,1,4))





















#Codes to generate result for Squared Residuals:
res.arima212=arima212$res
squared.res.arima212=res.arima212^2
par(mfcol=c(3,1))
plot(squared.res.arima212,main='Squared Residuals')
acf.squared212=acf(squared.res.arima212,main='ACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.arima212,main='PACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))


#R codes to perform ARCH/GARCH model:
arch08=garch(res.arima212,order=c(0,1),trace=F)
AIC(arch08)

arch08=garch(res.arima212,order=c(0,2),trace=F)
AIC(arch08)

arch08=garch(res.arima212,order=c(0,3),trace=F)
AIC(arch08)


arch08=garch(res.arima212,order=c(0,4),trace=F)
AIC(arch08)

arch08=garch(res.arima212,order=c(0,5),trace=F)
AIC(arch08)

arch08=garch(res.arima212,order=c(1,1),trace=F)
AIC(arch08)



loglik08=logLik(arch08)
summary(arch08)


par(mfcol=c(1,1))

# Generate 1-step forecast, 100-step forecast, and plot of forecast:
forecast212step1=forecast(arima212,1,level=95)
forecast212=forecast(arima212,100,level=95) 
plot(forecast212)

# Compute ht, conditional variance:
ht.arch08=arch08$fit[,1]^2 #use 1st column of fit
plot(ht.arch08,main='Conditional variances')

# Generate plot of Log Price, 95% Upper and Lower limit
fit212=fitted.values(arima212)
low=fit212-1.96*sqrt(ht.arch08)
high=fit212+1.96*sqrt(ht.arch08)
plot(log.appl,type='l',main='Log Apple,Low,High')
lines(low,col='red')
lines(high,col='blue')

ht.arch08=arch08$fit[,1]^2
ht.arch08
forecast(arima212,1,level=95)
fit212=fitted.values(arima212)
fit212-1.96*sqrt(ht.arch08) #low
fit212+1.96*sqrt(ht.arch08) #high


archres=res.arima212/sqrt(ht.arch08)
qqnorm(archres,main='ARIMA-ARCH Residuals')
qqline(archres)




qqnorm(res.arima212)

qqline(res.arima212)


