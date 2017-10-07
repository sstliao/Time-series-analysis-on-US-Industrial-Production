setwd("/Users/shutingliao/Documents/2017\ Winter/stats\ 531/data")
name_col=c("YR","MN","IP","MFG","MFGD","MFGN","MIN","UTIL","P","MAT")
M = read.table('USA industrial production.txt', col.names = name_col)
P = ts(M[,c(3:10)],start=c(1947, 1),frequency = 12)
head(P)
plot(P)
P1= diff(P,1)
plot(P1)
par(mfrow=c(2,2))
acf(P[,1], lag.max = 1000)
acf(P[,2], lag.max = 1000)
acf(P[,3], lag.max = 1000)
acf(P[,4], lag.max = 1000)
acf(P[,5], lag.max = 1000)
acf(P[,6], lag.max = 1000)
acf(P[,7], lag.max = 1000)
acf(P[,8], lag.max = 1000)
pacf(P[,1], lag.max = 1000)
pacf(P[,2], lag.max = 1000)
pacf(P[,3], lag.max = 1000)
pacf(P[,4], lag.max = 1000)
pacf(P[,5], lag.max = 1000)
pacf(P[,6], lag.max = 1000)
pacf(P[,7], lag.max = 1000)
pacf(P[,8], lag.max = 1000)

#Test
library(tseries)
library(urca)
library(forecast)
library(portes)
library(stats)
#stationary check
print(adf.test(P[,1], alt='stationary'))
print(kpss.test(P[,1], null='Level'))
print(kpss.test(P[,1], null='Trend'))

print(adf.test(P[,2], alt='stationary'))
print(kpss.test(P[,2], null='Level'))
print(kpss.test(P[,2], null='Trend'))

print(adf.test(P[,3], alt='stationary'))
print(kpss.test(P[,3], null='Level'))
print(kpss.test(P[,3], null='Trend'))

print(adf.test(P[,4], alt='stationary'))
print(kpss.test(P[,4], null='Level'))
print(kpss.test(P[,4], null='Trend'))

print(adf.test(P[,5], alt='stationary'))
print(kpss.test(P[,5], null='Level'))
print(kpss.test(P[,5], null='Trend'))

print(adf.test(P[,6], alt='stationary'))
print(kpss.test(P[,6], null='Level'))
print(kpss.test(P[,6], null='Trend'))

print(adf.test(P[,7], alt='stationary'))
print(kpss.test(P[,7], null='Level'))
print(kpss.test(P[,7], null='Trend'))

print(adf.test(P[,8], alt='stationary'))
print(kpss.test(P[,8], null='Level'))
print(kpss.test(P[,8], null='Trend'))

P1=diff(P,1)
#take difference
print(adf.test(P1[,1], alt='stationary'))
print(kpss.test(P1[,1], null='Level'))
print(kpss.test(P1[,1], null='Trend'))

print(adf.test(P1[,2], alt='stationary'))
print(kpss.test(P1[,2], null='Level'))
print(kpss.test(P1[,2], null='Trend'))

print(adf.test(P1[,3], alt='stationary'))
print(kpss.test(P1[,3], null='Level'))
print(kpss.test(P1[,3], null='Trend'))

print(adf.test(P1[,4], alt='stationary'))
print(kpss.test(P1[,4], null='Level'))
print(kpss.test(P1[,4], null='Trend'))

print(adf.test(P1[,5], alt='stationary'))
print(kpss.test(P1[,5], null='Level'))
print(kpss.test(P1[,5], null='Trend'))

print(adf.test(P1[,6], alt='stationary'))
print(kpss.test(P1[,6], null='Level'))
print(kpss.test(P1[,6], null='Trend'))

print(adf.test(P1[,7], alt='stationary'))
print(kpss.test(P1[,7], null='Level'))
print(kpss.test(P1[,7], null='Trend'))

print(adf.test(P1[,8], alt='stationary'))
print(kpss.test(P1[,8], null='Level'))
print(kpss.test(P1[,8], null='Trend'))



#2. Cointergration tests 
#Engle-Granger conintergration test 
z=list()
z1=list()
for(i in 1:8) z[[i]]= lm(P1[,i]~., P1[,-i])
for(i in 1:8) print(summary(z[[i]]))
for(i in 1:8) {z1[[i]]=ur.df(z[[i]]$res);
if (i ==1) print(z1[[i]]@cval);
print(z1[[i]]@teststat)}

#Pillips-Ouliaris conintergration test  
zz=ca.po(P1[,1:6], demean="constant", type = "Pu")
print(zz@cval)
print(zz@teststat)
zz1=ca.po(P1[,1:6], demean="constant", type = "Pz")
print(zz1@cval)
print(zz1@teststat)
#Johansen procedure
at=ca.jo(P, type = "trace", ecdet = "const")
#summary(at)
at@cval
at@teststat#r=6

#granger test
library(MSBVAR)
g=granger.test(P1, p=8)
round(g, 4)


#library(MTS)
library(vars)
(m1=VAR(P1, p=4, type = "both"))
m2 = VAR(P, p=4, type = "both")
stability(m1)
e1 = residuals(m1)

par(mfrow=c(2,2)) ##line 28
plot(gvtest(e1[,1],1:60)[,4], main="Generalized variance tests for IP (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,1],1:60)[,4], main="Ljung-Box tests for IP (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,1], main="Acf of residuals for IP (VAR)", lag.max = 60) ##line 36
plot(e1[,1],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,2],1:60)[,4], main="Generalized variance tests for MFG (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,2],1:60)[,4], main="Ljung-Box tests for MFG (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,2], main="Acf of residuals for MFG (VAR)", lag.max = 60) ##line 36
plot(e1[,2],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,3],1:60)[,4], main="Generalized variance tests for MFGD (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,3],1:60)[,4], main="Ljung-Box tests for MFGD (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,3], main="Acf of residuals for MFGD (VAR)", lag.max = 60) ##line 36
plot(e1[,3],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,4],1:60)[,4], main="Generalized variance tests for MFGN (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,4],1:60)[,4], main="Ljung-Box tests for MFGN (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,4], main="Acf of residuals for MFGN (VAR)", lag.max = 60) ##line 36
plot(e1[,4],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,5],1:60)[,4], main="Generalized variance tests for MIN (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,5],1:60)[,4], main="Ljung-Box tests for MIN (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,5], main="Acf of residuals for MIN (VAR)", lag.max = 60) ##line 36
plot(e1[,5],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,6],1:60)[,4], main="Generalized variance tests for UTIL (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,6],1:60)[,4], main="Ljung-Box tests for UTIL (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,6], main="Acf of residuals for UTIL (VAR)", lag.max = 60) ##line 36
plot(e1[,6],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,7],1:60)[,4], main="Generalized variance tests for P (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,7],1:60)[,4], main="Ljung-Box tests for P (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,7], main="Acf of residuals for P (VAR)", lag.max = 60) ##line 36
plot(e1[,7],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e1[,8],1:60)[,4], main="Generalized variance tests for MAT (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e1[,8],1:60)[,4], main="Ljung-Box tests for MAT (VAR)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e1[,8], main="Acf of residuals for MAT (VAR)", lag.max = 60) ##line 36
plot(e1[,8],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)
  #prediction
z_pred=predict(m1, n.ahead=72, ci=0.95)
par(mar= c(1,1,1,1))
plot(z_pred)
m2_pred = predict(m2, n.ahead = 72, ci = 0.95)
plot(m2_pred)

#VARX
library(dse)
CNA = TSdata(input = P1[,c(1:4)], output=P1[,5:8])
CNA = tframed(CNA, list(start=c(1947,1), frequency=12))
CNA.ls = estVARXls(CNA, max.lag = 2)
print(CNA.ls)
#detach(package:vars)
stability(CNA.ls)
 #residuals analysis
par(mar = c(1,1,1,1))
rr = checkResiduals(CNA.ls, plot=T)
plot(rr$residuals, type='p')
e2 = residuals(CNA.ls)
#par('mar')
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(gvtest(e2[,1],1:60)[,4], main="Generalized variance tests for MIN (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e2[,1],1:60)[,4], main="Ljung-Box tests for MIN (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e2[,1], main="Acf of residuals for MIN (VARX)", lag.max = 60) ##line 36
plot(e2[,1],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e2[,2],1:60)[,4], main="Generalized variance tests for UTIL (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e2[,2],1:60)[,4], main="Ljung-Box tests for UTIL (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e2[,2], main="Acf of residuals for UTIL (VARX)", lag.max = 60) ##line 36
plot(e2[,2],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e2[,3],1:60)[,4], main="Generalized variance tests for P (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e2[,3],1:60)[,4], main="Ljung-Box tests for P (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e2[,3], main="Acf of residuals for P (VARX)", lag.max = 60) ##line 36
plot(e2[,3],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e2[,4],1:60)[,4], main="Generalized variance tests for MAT (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e2[,4],1:60)[,4], main="Ljung-Box tests for MAT (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e2[,4], main="Acf of residuals for MAT (VARX)", lag.max = 60) ##line 36
plot(e2[,4],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)
  #prediction
detach(package:forecast)
CNA.ls2=estVARXls(window(CNA, end = c(1985,12)),max.lag = 2)
S.p=forecast(CNA.ls2, conditioning.inputs=CNA$input)
S.p$forecast
par(mar=c(1,1,1,1))
tfplot(S.p)

#two outputs(MIN UTIL)
CNA_2 = TSdata(input = P1[,c(1:4, 7,8)], output=P1[,5:6])
CNA_2 = tframed(CNA_2, list(start=c(1947,1), frequency=12))
CNA.ls_2 = estVARXls(CNA_2, max.lag = 2)
print(CNA.ls_2)
#detach(package:vars)
stability(CNA.ls_2)
#residuals analysis
#par(mar = c(1,1,1,1))
rr = checkResiduals(CNA.ls_2, plot=T)
plot(rr$residuals, type='p')
e3 = residuals(CNA.ls_2)
#par('mar')
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(gvtest(e3[,1],1:60)[,4], main="Generalized variance tests for MIN (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e3[,1],1:60)[,4], main="Ljung-Box tests for MIN (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e3[,1], main="Acf of residuals for MIN (VARX)", lag.max = 60) ##line 36
plot(e3[,1],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)

plot(gvtest(e3[,2],1:60)[,4], main="Generalized variance tests for UTIL (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 31
abline(h=0.05,lty=2)##line 32
plot(LjungBox(e3[,2],1:60)[,4], main="Ljung-Box tests for UTIL (VARX)",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1)) ##line 34
abline(h=0.05, lty=2) ##line 35
Acf(e3[,2], main="Acf of residuals for UTIL (VARX)", lag.max = 60) ##line 36
plot(e3[,2],ylab="Residuals") ##line 37
title("Residuals series") ##line 38
abline(h=0,lty=2)


#prediction
detach(package:forecast)
CNA.ls2_2=estVARXls(window(CNA_2, end = c(1985,12)),max.lag = 2)
S.p=forecast(CNA.ls2_2, conditioning.inputs=CNA_2$input)
S.p$forecast
par(mar=c(1,1,1,1))
tfplot(S.p)

#AFIMA for UTIL 
library(fracdiff)
(x.fd= fracdiff(P1[,6], nar = 2, nma = 1, M=30))
par(mfrow=c(2,2))
e4=residuals(x.fd)
plot(gvtest(e4,1:60)[,4], main="Generalized variance tests for AFIMA",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1))
abline(h=0.05,lty=2)
plot(LjungBox(e4,1:60)[,4], main="Ljung-Box tests for AFIMA",
     ylab="p-value", xlab="lag", pch=16, ylim=c(0,1))
abline(h=0.05, lty=2) 
Acf(e4, main="Acf of residuals for AFIMA", lag.max = 60)
plot(e4,ylab="Residuals") 
title("Residuals series")
abline(h=0,lty=2) 

pred = predict(x.fd, n.ahead=144, ci=0.95)
par(mfrow=c(1,1))
plot(pred)
