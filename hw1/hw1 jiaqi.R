#1--------------------------------------------------------
sim = rnorm(600,0.008,0.063)
hist(sim)
plot(sim, type = "l")

mu = 0.012; sigma = 0.05; p = 0.15; mu_j = -0.03; sigma_j = 0.1
epsilon = rnorm(600,0,1)
delta = rnorm(600,0,1)
bernoulli = rbinom(600,1,p)
r = mu + sigma*epsilon + bernoulli*(mu_j+sigma_j*delta)
hist(r)
plot(r, type = "l")
mu_r = mean(r)
std = sd(r)
skew = mean((r-mu_r)^3/(std^3))
kurt = mean((r-mu_r)^4/(std^4))

#2-------------------------------------------------------
DBV = read.csv("DBV.csv")
GSPC = read.csv("GSPc.csv")

DBV_p = DBV$Adj.Close
DBV_p_lag = c(0,DBV_p[1:(length(DBV_p)-1)])
dvd = DBV_p/DBV_p_lag
r_DBV = log(dvd[2:length(dvd)])
plot(r_DBV, type = "l")
hist(r_DBV)

GSPC_p = GSPC$Adj.Close
GSPC_p_lag = c(0,GSPC_p[1:(length(GSPC_p)-1)])
dvd2 = GSPC_p/GSPC_p_lag
r_GSPC = log(dvd2[2:length(dvd2)])
plot(r_GSPC, type = "l")
hist(r_GSPC)


skew_DBV = mean((r_DBV-mean(r_DBV))^3/(sd(r_DBV)^3))
t_skew_DBV = skew_DBV/sqrt(6/length(r_DBV))
if (abs(t_skew_DBV) > 1.96){
  print(paste("|t| =", round(abs(t_skew_DBV),2), "> 1.96, reject null hypothesis and skewness of DBV is significantly different than 0"))
  }else{
  print("skewness of DBV is not significantly different than 0")}
  
skew_GSPC = mean((r_GSPC-mean(r_GSPC))^3/(sd(r_GSPC)^3))
t_skew_GSPC = skew_GSPC/sqrt(6/length(r_GSPC))
if (abs(t_skew_GSPC) > 1.96){
  print(paste("|t| =", round(abs(t_skew_GSPC),2), "> 1.96, reject null hypothesis and skewness of GSPC is significantly different than 0"))
}else{
  print("skewness of GSPC is not significantly different than 0")}

kurt_DBV = mean((r_DBV-mean(r_DBV))^4/(sd(r_DBV)^4))
t_kurt_DBV = (kurt_DBV-3)/sqrt(24/length(r_DBV))
if (abs(t_kurt_DBV) > 1.96){
  print(paste("|t| =", round(abs(t_kurt_DBV),2), "> 1.96, reject null hypothesis and kurtosis of DBV is significantly different than 0"))
}else{
  print("kurtosis of DBV is not significantly different than 0")}

kurt_GSPC = mean((r_GSPC-mean(r_GSPC))^4/(sd(r_GSPC)^4))
t_kurt_GSPC = (kurt_GSPC-3)/sqrt(24/length(r_GSPC))
if (abs(t_kurt_GSPC) > 1.96){
  print(paste("|t| =", round(abs(t_kurt_GSPC),2), "> 1.96, reject null hypothesis and kurtosis of GSPC is significantly different than 0"))
}else{
  print("kurtosis of GSPC is not significantly different than 0")}

JB_DBV = t_skew_DBV^2+t_kurt_DBV^2
JB_GSPC = t_skew_GSPC^2+t_kurt_GSPC^2
CHI = qchisq(0.95,2)

if (JB_DBV > CHI){
  print(paste("JB =", round(JB_DBV,2), ">", round(CHI,2), ", reject null hypothesis and DBV is not normally distributed"))
}else{
  print("DBV is normally distributed")}

if (JB_GSPC > CHI){
  print(paste("JB =", round(JB_GSPC,2), ">", round(CHI,2), ", reject null hypothesis and GSPC is not normally distributed"))
}else{
  print("GSPC is normally distributed")}

#3
table = matrix(c(skew_DBV, t_skew_DBV, kurt_DBV, t_kurt_DBV, skew_GSPC, t_skew_GSPC, kurt_GSPC, t_kurt_GSPC), nrow = 4, ncol = 2)
colnames(table) = c("DBV", "GSPC")
rownames(table) = c("skewness", "t.skewness", "kurtosis", "t.kurtosis")
print(table)

#5
library(sandwich)

lm.return = lm(r_DBV ~ r_GSPC)
summ_return = summary(lm.return)
std.coeff = data.frame(summ_return$coefficients[1:2,2])
colnames(std.coeff) = "standard error"
plot(r_DBV,r_GSPC)
abline(summ_return$coefficients[1],summ_return$coefficients[2], col= "red")
wstd = sqrt(diag(vcovHC(lm.return, type = "HC")))
wstd.coeff = data.frame(wstd)
colnames(wstd.coeff) = "white std"
print(std.coeff)
print(wstd.coeff)
