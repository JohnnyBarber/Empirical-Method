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

yt.lag1 = yt[1:(n-1)]
Y1 = yt[2:n]

ar1 = lm(Y1~yt.lag1)

yt.lag1 = yt[1:(n-2)]
yt.lag2 = yt[2:(n-1)]
Y2 = yt[3:n]

ar2 = lm(Y2~yt.lag1+yt.lag2)

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

ar1.coef = summary(ar1)
ar2.coef = summary(ar2)
ar3.coef = summary(ar3)
ar11.coef = summary(ar11)

1/polyroot(c(1,-ar1.coef$coefficients[2,1]))

1/polyroot(c(1,-ar2.coef$coefficients[2,1], -ar2.coef$coefficients[3,1]))

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
par(mfrow = c(2,2))
plot(ar1$residuals, type = "l")
plot(ar2$residuals, type = "l")
plot(ar3$residuals, type = "l")
plot(ar11$residuals, type = "l")

par(mfrow = c(2,2))
acf(ar1$residuals)
acf(ar2$residuals)
acf(ar3$residuals)
acf(ar11$residuals)


#5c
Box.test(ar1$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar1$residuals, lag = 12, type = "Ljung-Box")

Box.test(ar2$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar2$residuals, lag = 12, type = "Ljung-Box")

Box.test(ar3$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar3$residuals, lag = 12, type = "Ljung-Box")

Box.test(ar11$residuals, lag = 8, type = "Ljung-Box")
Box.test(ar11$residuals, lag = 12, type = "Ljung-Box")

aic = c(AIC(ar1),AIC(ar2),AIC(ar3),AIC(ar11))
bic = c(BIC(ar1),BIC(ar2),BIC(ar3),BIC(ar11))

#6--------------
library(xts)
date = as.Date(as.vector(PPI$ï..DATE))
PPI.xts = xts(PPI$VALUE, order.by = date)
PPI.sample = diff(log(as.vector(PPI.xts["/2005-10-01"])))
PPI.pred = diff(log(as.vector(PPI.xts["2005-10-01/"])))

ar1.test = arima(PPI.sample,order = c(1,0,0))
ar2.test = arima(PPI.sample,order = c(2,0,0))
ar3.test = arima(PPI.sample,order = c(3,0,0))
ar11.test = arima(PPI.sample,order = c(11,0,0))

n.sample = length(PPI.sample)
n.pred = length(PPI.pred)

# ar1.pred = PPI.sample[n.sample]
# ar2.pred = PPI.sample[(n.sample-1):n.sample]
# ar3.pred = PPI.sample[(n.sample-2):n.sample]
# ar11.pred = PPI.sample[(n.sample-10):n.sample]
# for (i in 1:n.pred)
# {
#   ar1.pred = append(ar1.pred,ar1.test$coef[2]+ar1.test$coef[1]*ar1.pred[i])
#   ar2.pred = append(ar2.pred,ar2.test$coef[3]+ar2.test$coef[2]*ar2.pred[i]+ar2.test$coef[1]*ar2.pred[i+1])
#   ar3.pred = append(ar3.pred,ar3.test$coef[4]+
#                       ar3.test$coef[3]*ar3.pred[i]+
#                       ar3.test$coef[2]*ar3.pred[i+1]+
#                       ar3.test$coef[1]*ar3.pred[i+2])
#   ar11.pred = append(ar11.pred,ar11.test$coef[12]+
#                        ar11.test$coef[11]*ar11.pred[i]+
#                        ar11.test$coef[10]*ar11.pred[i+1]+
#                        ar11.test$coef[9]*ar11.pred[i+2]+
#                        ar11.test$coef[8]*ar11.pred[i+3]+
#                        ar11.test$coef[7]*ar11.pred[i+4]+
#                        ar11.test$coef[6]*ar11.pred[i+5]+
#                        ar11.test$coef[5]*ar11.pred[i+6]+
#                        ar11.test$coef[4]*ar11.pred[i+7]+
#                        ar11.test$coef[3]*ar11.pred[i+8]+
#                        ar11.test$coef[2]*ar11.pred[i+9]+
#                        ar11.test$coef[1]*ar11.pred[i+10])
# }
# 
# par(mfrow = c(2,2))
# plot(ar1.pred, type = "l")
# plot(ar2.pred, type = "l")
# plot(ar3.pred, type = "l")
# plot(ar11.pred, type = "l")

pred1 = PPI.sample
pred2 = PPI.sample
pred3 = PPI.sample
pred11 = PPI.sample
for (i in 1:n.pred)
{
  pred1 = append(pred1,predict(ar1.test, n.ahead = 1)$pred)
  pred2 = append(pred2,predict(ar2.test, n.ahead = 1)$pred)
  pred3 = append(pred3,predict(ar3.test, n.ahead = 1)$pred)
  pred11 = append(pred11,predict(ar11.test, n.ahead = 1)$pred)
  ar1.test = arima(pred1,order = c(1,0,0))
  ar2.test = arima(pred2,order = c(2,0,0))
  ar3.test = arima(pred3,order = c(3,0,0))
  ar11.test = arima(pred11,order = c(11,0,0))
  print(paste(round(i/n.pred*100,2),"%"))
}

par(mfrow = c(2,2))
plot(pred1[n.sample:n], type = "l")
plot(pred2[n.sample:n], type = "l")
plot(pred3[n.sample:n], type = "l")
plot(pred11[n.sample:n], type = "l")

v1 = sum((PPI.pred-pred1[(n.sample+1):n])^2)/n.pred
v2 = sum((PPI.pred-pred2[(n.sample+1):n])^2)/n.pred
v3 = sum((PPI.pred-pred3[(n.sample+1):n])^2)/n.pred
v11 = sum((PPI.pred-pred11[(n.sample+1):n])^2)/n.pred
print(c(v1,v2,v3,v11))

#rw---------------------
# monte = vector()
# for (i in 1:1000)
# {
#   set.seed(i)
#   rw.test = as.vector(PPI.xts[n.sample])
#   for (i in 1:n.pred){
#     rw.test = append(rw.test, rw.test[i]+rnorm(1,0,1))
#   }
#   rw.test = diff(log(rw.test))
#   rw.v = sum((PPI.pred-rw.test)^2)/n.pred
#   #plot(rw.test, type = "l")
#   #print(rw.v)
#   monte = append(monte,rw.v)
# }
# mean(monte)
rw.sd = sqrt(var(PPI.sample))
monte = vector()
for (i in 1:1000)
{
  set.seed(i)
  rw.test = PPI.sample[n.sample]
  for (i in 1:n.pred){
    rw.test = append(rw.test, rw.test[i]+rnorm(1,0,rw.sd))
  }
  rw.v = sum((PPI.pred-rw.test)^2)/n.pred
  #plot(rw.test, type = "l")
  #print(rw.v)
  monte = append(monte,rw.v)
}
mean(monte)
