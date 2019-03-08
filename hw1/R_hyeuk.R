
library(magrittr)
library(xts)
library(data.table)
library(readxl)

library(purrr)
library(moments)

library(knitr)
library(kableExtra)

##### Problem 1 -----------------------------------

## Q3
# 1) r ~ N(mean = 0.008, sd = 0.063), number of observations: 600 (50 years)
r = 0.008; sigma = 0.063; t = 1:600
sim <- function(r, sigma, t) {
  y = r + rnorm(length(t), sd = sigma)
}
normalmodel = sim(r, sigma, t)

#par(mfrow=c(1,2))
plot(t, normalmodel, pch=20, col="blue", ylim = c(-.3, .3), type = 'l')
abline(h = mean(normalmodel) + sd(normalmodel)*4, col = "red") # r + sigma*4
abline(h = mean(normalmodel) - sd(normalmodel)*4, col = "red") # r - sigma*4
title(paste("Daily log returns ~ N(mean = ", r,", sigma = ", sigma, ")", sep=""))
legend("topright", legend = c("Log Returns", "mu + 4sd", "mu - 4sd"), lty = 1, col = c("blue", "red", "red"), cex = .5)


# 2) r = mean + sd*e_t + J_t 
#e_t ~ N(0, 1)
#J_t; mean = mean(J)*p, var = mean(J)^2*p*(1-p) + var(J)*p
mean = 0.012; sigma = 0.05; p = 0.15; mean_j = -0.03; sigma_j = 0.1
sim2 <- function(mean, sigma, p, mean_j, sigma_j, t) {
  y = mean + sigma*rnorm(length(t), sd = 1) + rbernoulli(length(t), p)*(mean_j + sigma_j*rnorm(length(t), sd = 1))
}
jumpmodel = sim2(mean, sigma, p, mean_j, sigma_j, t)

#par(mfrow=c(1,2))
plot(t, jumpmodel, pch=20, col="green", ylim = c(-.3, .3), type = 'l')
abline(h = mean(jumpmodel) + sd(jumpmodel)*4, col = "red") # mean + mean_j*p + sigma*4
abline(h = mean(jumpmodel) - sd(jumpmodel)*4, col = "red")
title(paste("mean = ", mean,", sigma = ", sigma, ", p = ", p, sep=""))
legend("topright", legend = c("Log Returns", "mu + 4sd", "mu - 4sd"), lty = 1, 
       col = c("green", "red", "red"), cex = .5)

hist(jumpmodel, breaks = 50, xlim = c(-.3, .3), ylim = c(0, 60), col = "skyblue1", main = "", xlab = "")
par(new=TRUE)
hist(normalmodel, breaks = 50, xlim = c(-.3, .3), ylim = c(0, 60), col = "lightpink", main = "", xlab = "")
legend("topright", legend = c("Jump Model", "Normal Model"), lty = 1, col = c("skyblue1", "lightpink"), cex = 1)

paste("mean = ", mean(jumpmodel))
paste("variance = ", var(jumpmodel))
mu = mean(jumpmodel)
sd = sd(jumpmodel)
n = length(jumpmodel)
skew = sum((jumpmodel-mu)^3/(sd^3))/(n-1)
kurt = sum((jumpmodel-mu)^4/(sd^4))/(n-1)
paste("skewness = ", skew)
paste("kurtosis = ", kurt)


##### Problem 2 -----------------------------------

# Load Data
dbv <- read_xlsx("DBV.xlsx", sheet = 1) %>% as.data.table; gc()
gspc <- read_xlsx("GSPC.xlsx", sheet = 1) %>% as.data.table; gc()
dbv <- dbv[, .(Date, `Adj Close`)]
gspc <- gspc[, .(Date, `Adj Close`)]
colnames(dbv)[2] <- "close"
colnames(gspc)[2] <- "close"

# daily log returns
dbv[, "close" := log(close / shift(close))]
dbv <- dbv[-1]
gspc[, "close" := log(close / shift(close))]
gspc <- gspc[-1]

## Q1. Visualizing the data

# (a) Time series plots of the daily log-returns for DBV and GSPC
with(dbv, plot(Date, close, type = 'l', col = "blue"))
with(gspc, plot(Date, close, type = 'l'))

# (b) Histograms of the daily log-returns for DBV and GSPC
hist(dbv$close, breaks = 50, xlim = c(-.1, .1))
hist(gspc$close, breaks = 50, xlim = c(-.1, .1))


## Q2. Shape of Return Distribution
# (a) H0: skewness = 0, a = 0.05
# DBV (Deutsche Bank)
mean_dbv = mean(dbv$close)
sd_dbv = sd(dbv$close)
n_dbv = length(dbv$close)
skew_dbv <- sum((dbv$close-mean_dbv)^3/(sd_dbv^3))/(n_dbv-1)
skew_dbv_t <- skew_dbv / sqrt(6/n_dbv)
if(abs(skew_dbv_t) > 1.96) {
  cat(paste("|t| =", round(abs(skew_dbv_t), 4), "> 1.96: reject the null hypothesis\n (Skewness of DBV is significantly different from zero.)\n"))
} else {
  paste("Fail to reject the null hypothesis.")
}

# GSPC (S&P 500)
mean_gspc = mean(gspc$close)
sd_gspc = sd(gspc$close)
n_gspc = length(gspc$close)
skew_gspc <- sum((gspc$close-mean_gspc)^3/(sd_gspc^3))/(n_gspc-1)
skew_gspc_t <- skew_gspc / sqrt(6/n_gspc)
if(abs(skew_gspc_t) > 1.96) {
  cat(paste("|t| =", round(abs(skew_gspc_t), 4), "> 1.96: reject null hypothesis\n (Skewness of GSPC is significantly different than zero.)\n"))
} else {
  paste("Fail to reject the null hypothesis.")
}

# (b) H0: excess kurtosis = 0, a = 0.05
# DBV (Deutsche Bank)
kurt_dbv <- sum((dbv$close-mean_dbv)^4/(sd_dbv^4))/(n_dbv-1) - 3 # Excess kurtosis
kurt_dbv_t <- kurt_dbv / sqrt(24/n_dbv)
if(abs(kurt_dbv_t) > 1.96) {
  cat(paste("|t| =", round(abs(kurt_dbv_t), 4), "> 1.96: reject null hypothesis\n (Kurtosis of DBV is significantly different than zero.)\n"))
} else {
  paste("Fail to reject the null hypothesis.")
}

# GSPC (S&P 500)
kurt_gspc <- sum((gspc$close-mean_gspc)^4/(sd_gspc^4))/(n_gspc-1) - 3
kurt_gspc_t <- kurt_gspc / sqrt(24/n_gspc)
if(abs(kurt_gspc_t) > 1.96) {
  cat(paste("|t| =", round(abs(kurt_gspc_t), 4), "> 1.96: reject null hypothesis\n (Kurtosis of GPSC is significantly different than zero.)\n"))
} else {
  paste("Fail to reject the null hypothesis.")
}

# (c) Jarque-Bera test (H0: skewwness & excess kurtosis = 0 (log return ~ Normal), a = 0.05)
#jb = ( skew_dbv^2 / (6/length(dbv$close)) ) + (kurt_dbv^2 / (24/length(dbv$close)))
# DBV (Deutsche Bank)
jb_dbv <- skew_dbv_t^2 + kurt_dbv_t^2
jb_dbv_chi <- qchisq(0.95, df = 2)
if(jb_dbv > jb_dbv_chi) {
  paste(round(jb_dbv, 4), ">", round(jb_dbv_chi, 4), ": reject the null (DBV is not normally distributed.)")
} else {
  paste("Fail to reject the null hypothesis.")
}

# GSPC (S&P 500)
jb_gspc <- skew_gspc_t^2 + kurt_gspc_t^2
jb_gspc_chi <- qchisq(0.95, df = 2)
if(jb_gspc > jb_gspc_chi) {
  paste(round(jb_gspc, 4), ">", round(jb_gspc_chi, 4), ": reject the null (GSPC is not normally distributed.)")
} else {
  paste("Fail to reject the null hypothesis.")
}


## Q3.

dt <- matrix(
  c(skew_dbv, skew_dbv_t, kurt_dbv, kurt_dbv_t, jb_dbv, pchisq(jb_dbv, df=2, lower.tail=FALSE), 
    skew_gspc, skew_gspc_t, kurt_gspc, kurt_gspc_t, jb_gspc, pchisq(jb_gspc, df=2, lower.tail=FALSE)), 
  ncol = 2, 
  dimnames = list(c("Skewness", "t-test", "Excess Kurtosis", "t-test", "JB-test", "p-value JB"), 
                  c("DBV (Deutsche Bank)", "GSPC (S&P 500)")))
kable(dt, "latex", booktabs = T) %>% kable_styling(latex_options = "striped") %>% add_indent(c(2, 4))


## Q4.
# 1. scale the data to hold annual return = 0.2, annual sd = 0.4 (sharpe ratio = E(r)/sd = 0.5)
daily_target_r <- 0.2/252
daily_target_sd <- 0.4/sqrt(252)

# DBV
mean_dbv_daily <- mean(dbv$close)
sd_dbv_daily <- sd(dbv$close)
dbv$ret <- ((dbv$close - mean_dbv_daily)/sd_dbv_daily)* daily_target_sd + daily_target_r

## Annual returns
dbv_annual <- apply.yearly(dbv[, .(Date, close)], function(x) prod(1 + x, na.rm = T) - 1)
#mean(dbv_annual) # -0.003420513
dbv_lever <- apply.yearly(dbv[, .(Date, ret)], function(x) prod(1 + x, na.rm = T) - 1)
#mean(dbv_lever) # 0.2006492

# GSPC
mean_gspc_daily <- mean(gspc$close)
sd_gspc_daily <- sd(gspc$close)
gspc$ret <- ((gspc$close - mean_gspc_daily)/sd_gspc_daily)* daily_target_sd + daily_target_r
## Annual returns
gspc_annual <- apply.yearly(gspc[, .(Date, close)], function(x) prod(1 + x, na.rm = T) - 1)
#mean(gspc_annual) # 0.04344666
gspc_lever <- apply.yearly(gspc[, .(Date, ret)], function(x) prod(1 + x, na.rm = T) - 1)
#mean(gspc_lever) # 0.1985398

lev_mu_dbv = mean(dbv$ret)
lev_sd_dbv = sd(dbv$ret)
n_dbv = length(dbv$ret)
lev_skew_dbv = sum((dbv$ret-lev_mu_dbv)^3/(lev_sd_dbv^3))/(n_dbv-1)
lev_kurt_dbv = sum((dbv$ret-lev_mu_dbv)^4/(lev_sd_dbv^4))/(n_dbv-1)

lev_mu_gspc = mean(gspc$ret)
lev_sd_gspc = sd(gspc$ret)
n_gspc = length(gspc$ret)
lev_skew_gspc = sum((gspc$ret-lev_mu_gspc)^3/(lev_sd_gspc^3))/(n_gspc-1)
lev_kurt_gspc = sum((gspc$ret-lev_mu_gspc)^4/(lev_sd_gspc^4))/(n_gspc-1)

data <- matrix(
  c(mean_dbv_daily, mean(dbv_annual), sd_dbv_daily, sd(dbv_annual), skew_dbv, kurt_dbv + 3,
    mean(dbv$ret), mean(dbv_lever), sd(dbv$ret), sd(dbv_lever), lev_skew_dbv, lev_kurt_dbv,
    mean_gspc_daily, mean(gspc_annual), sd_gspc_daily, sd(gspc_annual), skew_gspc, kurt_gspc + 3,
    mean(gspc$ret), mean(gspc_lever), sd(gspc$ret), sd(gspc_lever), lev_skew_gspc, lev_kurt_gspc), 
  ncol = 4, 
  dimnames = list(c("Daily Return", "Annual Return", "Standard Deviation", "SD (annual)", "Skewness", "Kurtosis"), c("Original DBV", "Leveraged DBV", "Original GSPC", "Leveraged GSPC")))
kable(data, "latex", booktabs = T) %>% kable_styling(latex_options = "striped") %>% 
  row_spec(c(2, 4), bold = F, color = "gray") %>% add_indent(c(2, 4))


## Q5.

# Classic OLS (under normality)
out <- lm(dbv$close ~ gspc$close)
ols <- summary(out)$coef[1:2, 1:2] # coefficient and standard errors

# Regression allowing non-normality and HAC
library(sandwich)
hac <- sqrt(diag(vcovHC(out, type = "HC")))

# Comparing standard errors of the slope and the intercept
dt <- matrix(
  c(ols[2,2], ols[1,2], hac[[2]], hac[[1]]), 
  ncol = 2, 
  dimnames = list(c("Slope", "Intercept"), c("Standard OLS", "Non-normality and HAC")))
kable(dt, "latex", booktabs = T) %>% kable_styling(latex_options = "striped")
