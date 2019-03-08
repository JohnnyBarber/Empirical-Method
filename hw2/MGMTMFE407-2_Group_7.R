#1---------------------
set.seed(1)
b0 = 0
b1.1 = 1.1
b2.1 = -0.25
WN1 = rnorm(1000, mean = 0, sd = 1)
MySeq1 = rnorm(2)
for (i in 3:1000){
  MySeq1 = append(MySeq1, b0+b1.1*MySeq1[i-1]+b2.1*MySeq1[i-2]+WN1[i])
}

#a
plot(MySeq1, type = "l")
acf(MySeq1, lag.max = 20)

#b
1/polyroot(c(1,-b1.1,-b2.1))
#stationary
dm1 = b1.1^6+5*b1.1^4*b2.1+b2.1^3+6*b1.1^2*b2.1^2

#d
set.seed(3)
b0 = 0
b1.2 = 0.9
b2.2 = 0.8
WN2 = rnorm(100, mean = 0, sd = 1)
MySeq2 = rnorm(2)
for (i in 3:100){
  MySeq2 = append(MySeq2, b0+b1.2*MySeq2[i-1]+b2.2*MySeq2[i-2]+WN2[i])
}

par(mfrow=c(1,1))
plot(MySeq2, type = "l")
acf(MySeq2, lag.max = 20)

1/polyroot(c(1,-b1.2,-b2.2))
#stationary
dm2 = b1.2^6+5*b1.2^4*b2.2+b2.2^3+6*b1.2^2*b2.2^2

print(c(dm1,dm2))

#2---------------------
#1
PPI = read.csv("PPIFGS.csv")
valPPI = PPI$VALUE
delPPI = diff(valPPI)
logPPI = log(valPPI)
dellogPPI = diff(log(valPPI))
par(mfrow = c(2,2))
plot(valPPI, type = "l", ylab = "value")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
plot(logPPI, type = "l", ylab = "log value")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
plot(delPPI, type = "l", ylab = "change in value")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
plot(dellogPPI, type = "l", ylab = "change in log value")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

#3----------------
par(mfrow = c(1,1))
acf(dellogPPI, lag.max = 12)

#4-------------------
par(mfrow = c(1,1))
pacf(dellogPPI, lag.max = 12)

#5a
#manual----------------
n = length(dellogPPI)
yt = dellogPPI

yt.lag1 = yt[1:(n-3)]
yt.lag2 = yt[2:(n-2)]
yt.lag3 = yt[3:(n-1)]
Y3 = yt[4:n]

ar3 = lm(Y3~yt.lag1+yt.lag2+yt.lag3)

for (i in 1:11){
  assign(paste("yt.lag", i, sep = ""),
         yt[i:(n-(12-i))])
}
Y11 = yt[12:n]

ar11 = lm(Y11~yt.lag1+yt.lag2+yt.lag3+
            yt.lag4+yt.lag5+yt.lag6+
            yt.lag7+yt.lag8+yt.lag9+
            yt.lag10+yt.lag11)

ar3.coef = summary(ar3)
ar11.coef = summary(ar11)

1/polyroot(c(1,-ar3.coef$coefficients[2,1],
           -ar3.coef$coefficients[3,1],
           -ar3.coef$coefficients[4,1]))

1/polyroot(c(11,-ar11.coef$coefficients[2,1],
           -ar11.coef$coefficients[3,1],
           -ar11.coef$coefficients[4,1],
           -ar11.coef$coefficients[5,1],
           -ar11.coef$coefficients[6,1],
           -ar11.coef$coefficients[7,1],
           -ar11.coef$coefficients[8,1],
           -ar11.coef$coefficients[9,1],
           -ar11.coef$coefficients[10,1],
           -ar11.coef$coefficients[11,1],
           -ar11.coef$coefficients[12,1]))

#function------------
# ar3.test = ar(dellogPPI, order.max = 3, aic = F)
# ar12.test = ar(dellogPPI, order.max = 12, aic = F)
# 
# polyroot(c(1,-ar3.test$ar[1],-ar3.test$ar[2],-ar3.test$ar[3]))
# polyroot(c(1,-ar12.test$ar[1],-ar12.test$ar[2],-ar12.test$ar[3],
#            -ar12.test$ar[4],-ar12.test$ar[5],-ar12.test$ar[6],
#            -ar12.test$ar[7],-ar12.test$ar[8],-ar12.test$ar[9],
#            -ar12.test$ar[10],-ar12.test$ar[11],-ar12.test$ar[12]))

#5b------
par(mfrow = c(2,1))
plot(ar3$residuals, type = "l")
plot(ar11$residuals, type = "l")

par(mfrow = c(2,1))
acf(ar3$residuals)
acf(ar11$residuals)


#5c
Box.test(ar3$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar3$residuals, lag = 12, type = "Ljung-Box")

Box.test(ar11$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar11$residuals, lag = 12, type = "Ljung-Box")

aic = c(AIC(ar3),AIC(ar11))
bic = c(BIC(ar3),BIC(ar11))
aic
bic

#6--------------
library(xts)
PPI = read.csv("PPIFGS.csv")
date = as.Date(as.vector(PPI$DATE))
PPI.xts = xts(PPI$VALUE, order.by = date)
PPI.sample = diff(log(as.vector(PPI.xts["/2005-10-01"])))
PPI.pred = diff(log(as.vector(PPI.xts["2005-10-01/"])))

ar3.test = ar(PPI.sample,order.max = 3, aic = F)
ar11.test = ar(PPI.sample,order.max = 11, aic = F)

n.sample = length(PPI.sample)
n.pred = length(PPI.pred)

pred3 = predict(ar3.test, n.ahead = 39)$pred
pred11 = predict(ar11.test, n.ahead = 39)$pred

v3 = sum((PPI.pred-pred3)^2)/n.pred
v11 = sum((PPI.pred-pred11)^2)/n.pred
c(v3,v11)

#rw---------------------
rw.sd = sqrt(var(PPI.sample))
monte = vector()
for (i in 1:1000)
{
  set.seed(i)
  rw.test = PPI.sample[n.sample]
  for (i in 1:n.pred){
    rw.test = append(rw.test, rw.test[i]+rnorm(1,0,rw.sd))
  }
  rw.v = sum((PPI.pred-rw.test[-1])^2)/n.pred
  monte = append(monte,rw.v)
}
mean(monte)
