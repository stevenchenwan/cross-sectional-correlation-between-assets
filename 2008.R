################################################  Loading data and packages  ############################################################


data<-read.csv("2000.1.1 TO 2007.1.1.csv", header=T)
data<-read.csv("2007.1.1 TO 2010.1.1.csv", header=T)
data<-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)

data<-read.csv("2000.1.1 TO 2007.1.1 all sections.csv", header=T)
#data<-read.csv("2007.1.1 TO 2010.1.1 all sections.csv", header=T)
#data<-read.csv("2010.1.1 TO 2017.5.1 all sections.csv", header=T)
data1<-data
library(tseries)
library(forecast)


#data1 <- data[1:156,] #1994.1.1 TO 2007.1.1
#data1 <- data[73:156,] #2000.1.1 TO 2007.1.1
#data1 <- data[157:192,] #2007.1.1 TO 2010.1.1
#data1 <- data[193:283,] #2010.1.1 TO 2017.7.1

data1$Date<- NULL


###########################################  regression residuals loop (between SP500 and others) ########################################

data2<-data1
data2$SP500 <- NULL

regm<-matrix(NA,nrow(data1), ncol(data1)-1)
colnames(regm) <- names(data2)

n <- ncol(data1)-1

for (i in 1:n) {
  y=data1[,i]
  x1=data1$SP500
  reg <-lm(y ~ x1, data1)
  regm[,i] <- reg$residuals
}
regm
M <- cor(regm)


# install.packages("corrplot")
library("corrplot")
#corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)

library("PerformanceAnalytics")
#chart.Correlation(regm, histogram=TRUE, pch=19)

#corrplot(corm, order="hclust", addrect=2)
#corrplot(M, method="circle")
corrplot.mixed(corm, upper="square")



cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(regm,0.95)
res2 <- cor.mtest(regm,0.99)
## specialized the insignificant value according to the significant level
corrplot.mixed(M, upper="square", p.mat = res1[[1]], sig.level=0.1, insig = "blank")
corrplot(M, order="hclust", addrect=2, p.mat = res1[[1]], sig.level=0.1, insig = "blank")
















########################################################################################################################

capmtest<-data

attach(capmtest)
names(capmtest)
capmtest$Date <- NULL
y<-apply(capmtest,2,rev); #reverse the time order
y<-apply(capmtest,2,rev)
y<-log(y);  #log prices
y<-apply(y,2,diff) #log-returns
lreturn<-y
y<-y*100 #covert to percentage
T1<-nrow(y); #number of time points
T1
my<-apply(y,2,mean)
y1<-y-rep(1,T1)%*%t(my);
n1<-ncol(y)-1; # the last column is S&P500
beta<-NULL; alpha<-NULL;sige2<-NULL; sigm2<-var(y1[,n1+1]);
sigi2<-NULL; corm<-matrix(0,n1,n1);
for(j in 1:n1)
{
  beta[j]<-as.vector(sum(y1[,j]*y1[,n1+1])/sum(y1[,n1+1]^2));
  alpha[j]<-my[j+1]-beta[j]*my[n1+1];
  sige2[j]<-(1/(T1-2))*sum((y[,j]-rep(1,T1)*alpha[j]-beta[j]*y[,n1+1])^2);
  sigi2[j]<-beta[j]^2*sigm2+sige2[j];
}
alpha;
beta;
sige2;
sigi2;
sigm2;
my;

capmtestt<-capmtest
capmtestt$SP500<-NULL

covm<-diag(sigi2);
colnames(covm) <- names(capmtestt)
rownames(covm) <- names(capmtestt)
corm<-matrix(1,n1,n1);
colnames(corm) <- names(capmtestt)
rownames(corm) <- names(capmtestt)
for(i in 1:n1)
  for(j in 1:n1)
  {
    if(i!=j){covm[i,j]<-beta[i]*beta[j]*sigm2;
    corm[i,j]<-beta[i]*beta[j]*sigm2/sqrt(sigi2[i]*sigi2[j]);}
  }
covm;
corm;





################################################  correlation plot  ########################################################

# install.packages("corrplot")
library("corrplot")
#corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)

library("PerformanceAnalytics")
#chart.Correlation(regm, histogram=TRUE, pch=19)

#corrplot(corm, order="hclust", addrect=2)
#corrplot(M, method="circle")
corrplot.mixed(corm, upper="square")

################################################  Factor analysis  ######################################################

factm<-matrix(NA,6, 1)
for (p in 1:6) {
  try({
    fit <- factanal(regm, p, rotation="varimax")
    if (fit$PVAL > 0.05) {
      factm[p,] <- fit$PVAL
    } 
  })
}
model<-which(factm > 0.05)
nfactors <- min(model)
bestfactanal <- factanal(regm, nfactors, rotation="varimax")
print(bestfactanal,cutoff= 0.3)

















