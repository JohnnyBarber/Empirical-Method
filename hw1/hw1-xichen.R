library("xts")
library("moments")
library("Rlab")
library("sandwich")

# Problem 1

# 3. Plot a simulated series of 600 observations
# (a)
logreturns=rnorm(600,m=0.008,sd=0.063)
plot(logreturns)

# (b)
m=0.012
s=0.05
mj=-0.03
sj=0.1
B=rbern(600,0.15)
jump=m+s*rnorm(600,sd=1,mean = 0)+B*(mj+sj*rnorm(600,sd=1,mean = 0))
plot(jump)

#  unconditional mean, standard deviation, skewness and kurtosis of log stock return
mean(jump)
var(jump)
skewness(jump)
kurtosis(jump)

# Problem 2

DBV = read.csv("Desktop/empirical/hw1/DBV.csv", header = TRUE)
GSPC = read.csv("Desktop/empirical/hw1/GSPC.csv", header = TRUE)

# change row name to dates
DBV$Date = as.Date(as.character(DBV$Date))
rownames(DBV) = DBV[,1]
DBV$Date = NULL

GSPC$Date = as.Date(as.character(DBV$Date))
rownames(GSPC) = GSPC[,1]
GSPC$Date = NULL

# change data frame into xts form
GSPC = as.xts(GSPC)
DBV = as.xts(DBV)


# 1. Visualizing the data
# calculate daily log returns
DBV_adj=DBV["Adj Close"]
GSPC_adj=GSPC["Adj Close"]
log_returns_DBV=diff(log(DBV_adj),lag=1)
log_returns_GSPC=diff(log(GSPC_adj),lag=1)

# a) Time series plots
ts_DBV=plot.ts(log_returns_DBV)
ts_GSPC=plot.ts(log_returns_GSPC)

# b) Histograms 
hist(log_returns_DBV)
hist(log_returns_GSPC)

# 2. Shape of Return Distribution
# a) Test the skewness of daily log returns is zero
skew_DBV=skewness(log_returns_DBV)
skew_GSPC=skewness(log_returns_GSPC)

t_skew_DBV = skew_DBV / sqrt(6/length(DBV_adj))
t_skew_GSPC = skew_GSPC / sqrt(6/length(GSPC_adj))                               


# b) Test the excess kurtosis of daily log returns is zero
e_kurtosis_DBV=kurtosis(log_returns_DBV)-3
e_kurtosis_GSPC=kurtosis(log_returns_GSPC)-3

t_kurt_DBV = e_kurtosis_DBV / sqrt(24/length(DBV_adj))
t_kurt_GSPC = e_kurtosis_GSPC / sqrt(24/length(GSPC_adj)) 

# c) Test the daily log returns are normally distributed 
#jarque.test(log_returns_DBV)
#jarque.test(log_returns_GSPC)

jb_DBV = t_skew_DBV^2 + t_kurt_DBV^2
# Compare with
qchisq(0.95, df = 2)

jb_GSPC = t_skew_GSPC^2 + t_kurt_GSPC^2
# Compare with
qchisq(0.95, df = 2)

# 3. table
# a)
# b)

# 4
# The higher moment of ???
# calculate the power share of ???

# 5. Regression
out=lm(log_returns_DBV~log_returns_GSPC)
coeftest(out, vcov = vcovHC(out, type="HC1"))

# also search for the White standard errors
# package: LM and use sanwich to get the error