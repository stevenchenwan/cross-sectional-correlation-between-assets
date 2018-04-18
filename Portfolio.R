capmtest <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)
#capmtest <-read.csv("2010.1.1 TO 2017.6.101 TEC IND.csv", header=T)



#capmtest1 <-read.csv("1994.1.1 TO 2007.1.1.csv", header=T)
#capmtest1$Date<-NULL
#capmtest2 <-read.csv("1994.1.1 TO 2007.1.1.csv", header=T)
#capmtest <-cbind(capmtest2, capmtest1)


#capmtest1 <-read.csv("2007.1.1 TO 2010.1.1.csv", header=T)
#capmtest1$Date<-NULL
#capmtest2 <-read.csv("2007.1.1 TO 2010.1.1.csv", header=T)
#capmtest <-cbind(capmtest1, capmtest2)

#capmtest <-data1 

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

library("psych")
b<-corr.p(corm,n=88,adjust="bonferroni",alpha=.05)
b$p

pvalue <- b$p
p<-as.data.frame.array(pvalue)
print(p,digits=3)
yy <- p>0.05
yy

#yy<-as.data.frame.array(y)
#yy$SP500<-NULL

#a<-corr.test(yy,adjust = "bonferroni")
#a$p

############################################## Multiple factor model ###################################################################


objmerkothers1<-lm(y[,1]~y[,2]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers2<-lm(y[,2]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers3<-lm(y[,3]~y[,1]+y[,2]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers4<-lm(y[,4]~y[,1]+y[,3]+y[,2]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers5<-lm(y[,5]~y[,1]+y[,3]+y[,4]+y[,2]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers6<-lm(y[,6]~y[,1]+y[,3]+y[,4]+y[,5]+y[,2]+y[,7]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers7<-lm(y[,7]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,2]+y[,8]+y[,9]+y[,10]+y[,11]);
objmerkothers8<-lm(y[,8]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,2]+y[,9]+y[,10]+y[,11]);
objmerkothers9<-lm(y[,9]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,2]+y[,10]+y[,11]);
objmerkothers10<-lm(y[,10]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,2]+y[,11]);
objmerkothers11<-lm(y[,11]~y[,1]+y[,3]+y[,4]+y[,5]+y[,6]+y[,7]+y[,8]+y[,9]+y[,10]+y[,2]);



summary(objmerkothers2)

anova(objmerkothers1)

#analysis of the residuals: white noises?

resid1<-residuals(objmerkothers1)
resid2<-residuals(objmerkothers2)
resid3<-residuals(objmerkothers3)
resid4<-residuals(objmerkothers4)
resid5<-residuals(objmerkothers5)
resid6<-residuals(objmerkothers6)
resid7<-residuals(objmerkothers7)
resid8<-residuals(objmerkothers8)
resid9<-residuals(objmerkothers9)
resid10<-residuals(objmerkothers10)
resid11<-residuals(objmerkothers11)


resid<-cbind.data.frame(resid1<-residuals(objmerkothers1)
      ,resid2<-residuals(objmerkothers2)
      ,resid3<-residuals(objmerkothers3)
      ,resid4<-residuals(objmerkothers4)
      ,resid5<-residuals(objmerkothers5)
      ,resid6<-residuals(objmerkothers6)
      ,resid7<-residuals(objmerkothers7)
      ,resid8<-residuals(objmerkothers8)
      ,resid9<-residuals(objmerkothers9)
      ,resid10<-residuals(objmerkothers10)
      ,resid11<-residuals(objmerkothers11))




Box.test(resid[,1],lag=1,type="Ljung-Box")

pbox<-NULL;
for(i in 1:10)
{
  pbox[i]<-Box.test(resid[,i],lag=1,type="Ljung-Box")$p.value;
}
1:10

pbox










#1
Box.test(resid[,1],lag=1,type="Ljung-Box")

pbox1<-NULL;
for(k in 10:20)
   {
     pbox1[k-9]<-Box.test(resid[,1],lag=k,type="Ljung-Box")$p.value;
    }
10:20

pbox1


#2
Box.test(resid[,2],lag=1,type="Ljung-Box")

pbox2<-NULL;
for(k in 10:20)
{
  pbox2[k-9]<-Box.test(resid[,2],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox2



#3
Box.test(resid[,3],lag=1,type="Ljung-Box")

pbox3<-NULL;
for(k in 10:20)
{
  pbox3[k-9]<-Box.test(resid[,3],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox3



#4
Box.test(resid[,4],lag=1,type="Ljung-Box")

pbox4<-NULL;
for(k in 10:20)
{
  pbox4[k-9]<-Box.test(resid[,4],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox4



#5
Box.test(resid[,5],lag=1,type="Ljung-Box")

pbox5<-NULL;
for(k in 10:20)
{
  pbox5[k-9]<-Box.test(resid[,5],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox5






#6
Box.test(resid[,6],lag=1,type="Ljung-Box")

pbox6<-NULL;
for(k in 10:20)
{
  pbox6[k-9]<-Box.test(resid[,6],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox6








#7
Box.test(resid[,7],lag=1,type="Ljung-Box")

pbox7<-NULL;
for(k in 10:20)
{
  pbox7[k-9]<-Box.test(resid[,7],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox7








#8
Box.test(resid[,8],lag=1,type="Ljung-Box")

pbox8<-NULL;
for(k in 10:20)
{
  pbox8[k-9]<-Box.test(resid[,8],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox8




#9
Box.test(resid[,9],lag=1,type="Ljung-Box")

pbox9<-NULL;
for(k in 10:20)
{
  pbox9[k-9]<-Box.test(resid[,9],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox9






#10
Box.test(resid[,10],lag=1,type="Ljung-Box")

pbox10<-NULL;
for(k in 10:20)
{
  pbox10[k-9]<-Box.test(resid[,10],lag=k,type="Ljung-Box")$p.value;
}
10:20

pbox10

aa<-cbind(pbox1,pbox2,pbox3,pbox4,pbox5,pbox6,pbox7,pbox8,pbox9,pbox10)
aa<-as.data.frame(aa)
colnames(aa) <- names(data2)
aa




library(nlme)
mod.gls<-gls(y[,1]~y[,2]+y[,3]+y[,4],correlation=corARMA(p=2), method='ML')
summary(mod.gls)






################################################### CAPM selection ###################################################################


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

############################################ CAPM testing #####################################################

alpha
beta

data1 <-read.csv("2010.1.1 TO 2017.6.1.csv", header=T)
data1$Date<-NULL
data2<-data1
data2$SP500 <- NULL



regm<-matrix(NA,nrow(data1), ncol(data1)-1)
colnames(regm) <- names(data2)

n <- ncol(data1)-1

for (i in 1:n) {
  z=data1[,i]
  x1=data1$SP500
  reg <-lm(z ~ x1, data1)
  regm[,i] <- reg$residuals
}
regm




regma<-matrix(NA,nrow(data1), ncol(data1)-1)
colnames(regm) <- names(data2)

n <- ncol(data1)-1

for (i in 1:n) {
  z=data1[,i]
  x1=data1$SP500
  reg <-lm(z ~ x1, data1)
  regma[,i] <- reg$fitted.values
}
regma



lm.fit = lm(y[,1] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,2] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,3] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,4] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,5] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,6] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,7] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,8] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,9] ~ y[, 11])
summary(lm.fit)

lm.fit = lm(y[,10] ~ y[, 11])
summary(lm.fit)



my


#joint test of alpha_i's
ridmatrix<-regm;
sigma2vep<-t(ridmatrix)%*%ridmatrix/T1;
grstest<- T1*(1+my[11]^2/((T1-1)*var(y[,11])))^{-1}*t(alpha)%*%solve(sigma2vep,alpha);
grstest<-as.vector(grstest);


pvalue<-1-pchisq(grstest,3);
pvalue

Box.test(alpha)

###############################################################################################################

bnt <-read.csv("2010.1.1 TO 2017.5.1.csv", header=T)
names(bnt)
MMM <- diff(log(bnt$MMM))
GD <- diff(log(bnt$GD))
GE <- diff(log(bnt$GE))
HON <- diff(log(bnt$HON))
ITW <- diff(log(bnt$ITW))
MAS <- diff(log(bnt$MAS))
BA <- diff(log(bnt$BA))
TOL <- diff(log(bnt$TOL))
UTX <- diff(log(bnt$UTX))
VMC <- diff(log(bnt$VMC))
sp500 <- diff(log(bnt$SP500))

MMM <- ts(MMM, start=c(2010,1),frequency = 12)
GD <- ts(GD, start=c(2010,1),frequency = 12)
GE <- ts(GE, start=c(2010,1),frequency = 12)
HON <- ts(HON, start=c(2010,1),frequency = 12)
ITW <- ts(ITW, start=c(2010,1),frequency = 12)
MAS <- ts(MAS, start=c(2010,1),frequency = 12)
BA <- ts(BA, start=c(2010,1),frequency = 12)
TOL <- ts(TOL, start=c(2010,1),frequency = 12)
UTX <- ts(UTX, start=c(2010,1),frequency = 12)
VMC <- ts(VMC, start=c(2010,1),frequency = 12)
sp500 <- ts(sp500, start=c(2010,1),frequency = 12)


#khaki

plot_colors <- c("green","cyan", "brown","yellow", "purple", "red", "orange","blue","pink","khaki","black")

require(graphics)
ts.plot(cbind(MMM,GD,GE,HON,ITW,MAS,BA,TOL,UTX,VMC, sp500), gpars=list(xlab="Months", ylab="Log return",lty=c(1:1)),col= c("green","cyan", "brown","yellow", "purple", "red", "orange","blue","pink","khaki","black"), main="Log returns of the Ten stocks and market index (S&P500)")
legend("topright", c("MMM",   "GD" ,   "GE"   , "HON" ,  "ITW"  , "MAS"  , "BA"   , "TOL"  , "UTX" ,  "VMC" ,  "SP500"),cex=0.5, col=plot_colors, lty=1:1, x.intersp = 1, text.width = 0.3)


print(corm, digits = 4)



# Correlations with significance levels
#library(Hmisc)
#rcorr(yy, type="pearson") # type can be pearson or spearman




#library("psych")

#a<-corr.test(covm,adjust = "bonferroni")
#a$p


#b<-corr.p(corm,n=10,adjust="bonferroni",alpha=.05)
#b$p


#library(psych)
#zs <- fisherz(corm)
#rs <- fisherz2r(zs)
#round(zs,2)
#n <- 100
#r <- corm
#rc <- matrix(r.con(r,n),ncol=2)
#t <- r*sqrt(n-2)/sqrt(1-r^2)
#p <- (1-pt(t,n-2))/2
#r.rc <- data.frame(r=r,z=fisherz(r),t=t,p=p)
#round(r.rc,2)
#p.adjust(p, "bonferroni")


