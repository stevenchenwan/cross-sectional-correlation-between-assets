capmtest <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)
attach(capmtest)
names(capmtest)
capmtest$Date <- NULL
y<-apply(capmtest,2,rev); #reverse the time order
y<-apply(capmtest,2,rev)
y<-log(y);  #log prices
y<-apply(y,2,diff) #log-returns
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

covm<-diag(sigi2);
corm<-matrix(1,n1,n1);
for(i in 1:n1)
  for(j in 1:n1)
  {
    if(i!=j){covm[i,j]<-beta[i]*beta[j]*sigm2;
    corm[i,j]<-beta[i]*beta[j]*sigm2/sqrt(sigi2[i]*sigi2[j]);}
  }
covm;
corm;


################################################################################################################


library(tseries)#monthly prices 1983-04 to 2011-04 #SP500,merck,ibm,jnj,ge,3m,sony
tb3m <-read.csv("FRED.csv", header=T) #monthly yields from 1983-04 to 2011-04
attach(tb3m)
names(tb3m)
T2<-length(tb3m$rate);
tb3m<-tb3m$rate[2:T2]; #1983-04 to 2011-04
tb3m<-tb3m/12 #convert to monthly returns
T2<-length(tb3m);
T2
# minimum variance portfolio
w1<- solve(covm)%*%rep(1,n1);
w1<-w1/sum(w1);
w1
crossprod(w1,my[1:n1]);
t(w1)%*%covm%*%w1;

# market portfolio
wm<-my[1:n1]-mean(tb3m);
wm<-solve(covm)%*%wm;
wm<-as.vector(wm/sum(wm));
wm
as.vector(crossprod(wm,my[1:n1]));
as.vector(t(wm)%*%covm%*%wm);

##################################################################################################################


bnt <-read.csv("all.csv", header=T)
AMD <- diff(log(bnt$AMD))
Apple <- diff(log(bnt$Apple))
IBM <- diff(log(bnt$IBM))
Intel <- diff(log(bnt$Intel))
NVIDA <- diff(log(bnt$NVIDA))
Microsoft <- diff(log(bnt$Microsoft))
sp500 <- diff(log(bnt$sp500))

AMD <- ts(AMD, start=c(2009,1),frequency = 12)
Apple <- ts(Apple, start=c(2009,1),frequency = 12)
IBM <- ts(IBM, start=c(2009,1),frequency = 12)
Intel <- ts(Intel, start=c(2009,1),frequency = 12)
NVIDA <- ts(NVIDA, start=c(2009,1),frequency = 12)
Microsoft <- ts(Microsoft, start=c(2009,1),frequency = 12)
sp500 <- ts(sp500, start=c(2009,1),frequency = 12)




plot_colors <- c("red","blue", "brown", "purple", "green", "orange","black" )

require(graphics)
ts.plot(cbind(AMD, Apple, IBM, Intel, NVIDA, Microsoft, sp500), gpars=list(xlab="Months", ylab="Log return",lty=c(1:1)),col= c("red","blue", "brown", "purple", "green", "orange","black" ), main="Log returns of the six stocks and market index (S&P500)")
legend(2008.7, 2, c("AMD", "Apple", "IBM", "Intel", "NVIDA", "Microsoft", "sp500"),cex=1, col=plot_colors, lty=1:1, x.intersp = 1, text.width = 0.3)
