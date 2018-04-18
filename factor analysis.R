# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
mydata <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)
mydata1 <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)
#mydata <-read.csv("1994.1.1 TO 2007.1.1 and 2010.1.1 TO 2017.6.1.csv", header=T)
names(mydata)
mydata$Date <- NULL
mydata$SP500 <- NULL
appl<-mydata
mydata1$Date<- NULL
appl<-mydata1

############################################### ARIMA residuals #################################################################


#appl.close=appl$GE
#appl.close=appl$MMM
#appl.close=appl$GD
#appl.close=appl$HON
#appl.close=appl$ITW
#appl.close=appl$MAS
#appl.close=appl$BA
#appl.close=appl$TOL
#appl.close=appl$UTX
#appl.close=appl$VMC
appl.close=appl$SP500

diff.appl=diff(appl.close)
log.appl=log(appl.close)
difflog.appl=diff(log.appl)
library(tseries)
library(forecast)
abc<-auto.arima(log.appl)
arima010=arima(log.appl,order=c(0,1,0))
arima010$residuals




#GE<-arima010$residuals
#MMM<-arima010$residuals
#GD<-arima010$residuals
#HON<-arima010$residuals
#ITW<-arima010$residuals
#MAS<-arima010$residuals
#BA<-arima010$residuals
#TOL<-arima010$residuals
#UTX<-arima010$residuals
#VMC<-arima010$residuals
SP500<-arima010$residuals
M<-cbind(GE,MMM,GD,HON,ITW,MAS,BA,TOL,UTX,VMC,SP500)
mydata<-M


############################################### regression residuals #######################################################

abc<-cbind(appl$VMC,appl$SP500)
abc=abc[-1,]
abc<-as.data.frame(abc)
y=appl$VMC
x1=appl$SP500
a<-lm(y ~ x1, data=appl)
a$residuals






y=appl$GE
x1=appl$SP500
aa<-lm(y ~ x1, data=appl)
GE<-aa$residuals


y=appl$MMM
x1=appl$SP500
bb<-lm(y ~ x1, data=appl)
MMM<-bb$residuals


y=appl$GD
x1=appl$SP500
cc<-lm(y ~ x1, data=appl)
GD<-cc$residuals


y=appl$HON
x1=appl$SP500
dd<-lm(y ~ x1, data=appl)
HON<-dd$residuals


y=appl$ITW
x1=appl$SP500
ee<-lm(y ~ x1, data=appl)
ITW<-ee$residuals


y=appl$MAS
x1=appl$SP500
ff<-lm(y ~ x1, data=appl)
MAS<-ff$residuals


y=appl$BA
x1=appl$SP500
gg<-lm(y ~ x1, data=appl)
BA<-gg$residuals


y=appl$TOL
x1=appl$SP500
hh<-lm(y ~ x1, data=appl)
TOL<-hh$residuals


y=appl$UTX
x1=appl$SP500
ii<-lm(y ~ x1, data=appl)
UTX<-ii$residuals


y=appl$VMC
x1=appl$SP500
jj<-lm(y ~ x1, data=appl)
VMC<-jj$residuals


N<-cbind(GE,MMM,GD,HON,ITW,MAS,BA,TOL,UTX,VMC)
mydata<-N




##########################################################################################################################









mydata=regm

factanal(mydata, 1, rotation="varimax")
factanal(mydata, 2, rotation="varimax")
factanal(mydata, 3, rotation="varimax")
factanal(mydata, 4, rotation="varimax")
factanal(mydata, 5, rotation="varimax")
factanal(mydata, 6, rotation="varimax")
factanal(mydata, 7, rotation="varimax")
factanal(mydata, 8, rotation="varimax")
factanal(mydata, 9, rotation="varimax")
factanal(mydata, 10, rotation="varimax")
factanal(mydata, 11, rotation="varimax")
factanal(mydata, 12, rotation="varimax")
factanal(mydata, 13, rotation="varimax")
factanal(mydata, 14, rotation="varimax")



fit <- factanal(mydata, 11, rotation="varimax")
print(fit, digits=2, cutoff= NULL, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names


# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


par(mfcol=c(1,2))
# PCA Variable Factor Map 
library(FactoMineR)
result <- PCA(mydata) # graphs generated automatically








