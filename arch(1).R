## Script to illustrate ARCH models using Intel

library(fGarch)
da=read.table("m-intc7303.txt",header=T)
head(da);tail(da)
intc=log(da$rtn+1)  # compute log returns

## plot
x11(10,7)
par(mfcol=c(2,2))
acf(intc); pacf(intc); acf(intc^2); pacf(intc^2)
acf(da$rtn); pacf(da$rtn); acf(da$rtn^2); pacf(da$rtn^2)
x11(10,7)
par(mfcol=c(2,1))
plot(da$rtn,type='l');abline(h=0);plot(da$rtn^2,type='l')

## some preliminary test
t.test(intc)
Box.test(intc,lag=10,type='Ljung')
Box.test(intc^2,lag=10,type='Ljung')

m1 = garchFit(~garch(3,0),data= intc) # lots of output
m1 = garchFit(~garch(3,0),data= intc,trace=F) # no output printed.
summary(m1) 

### refine the model
m2=garchFit(~garch(1,0),data= intc,trace=F)
summary(m2)
plot(m2)
predict(m2,6) # prediction

# use Student-t innovations
m3=garchFit(~garch(1,0),data= intc,trace=F,cond.dist=c("std"))
summary(m3)
plot(m3)

# use skewed Student-t innovations
m4=garchFit(~garch(1,0),data= intc,trace=F,cond.dist=c("sstd"))
summary(m4)
plot(m4)
