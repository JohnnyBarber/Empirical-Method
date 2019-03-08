### title: "MGMTMFE407-2 HW4"
### author: "Cohort 2, Group 7 - Hyeuk Jung, Jiaqi Li, Xichen Luo, Huanyu Liu"
### date: "February 24, 2019"

library(data.table)
library(dplyr)
library(readxl)
library(rugarch)
library(fGarch)
library(sandwich)

##### Problem 1: Risk Management using Value-at-Risk and GARCH Models ----
raw <- read_xlsx("Currency_fund_prices.xlsx") %>% as.data.table; gc()
data <- raw[, -c(2:5)]
data$lag_adj <- shift(data$`Adj Close`, 1)
data$log_ret <- log(data$`Adj Close` / data$lag_adj)

### Q1. Specify and estimate a parsimonious model for the conditional volatility of the daily log returns
# -> GARCH / I-GARCH / EGARCH / GJR-GARCH (total 4) * normal / t (total 2) * order 1,1 / 2,2 (2)
# -> AIC / BIC criterion (chapter 8, slide 66)

# GARCH(1,1)
garch_z <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "GARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = T),
                         distribution.model = "norm")
garch_t <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "GARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = T),
                         distribution.model = "std")

# I-GARCH(1,1)
igarch_z <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = T),
                      distribution.model = "norm")
igarch_t <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = T),
                      distribution.model = "std")

# E-GARCH(1,1)
egarch_z <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = T),
                      distribution.model = "norm")
egarch_t <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = T),
                      distribution.model = "std")

# GJR-GARCH(1,1)
gjrgarch_z <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0), include.mean = T),
                       distribution.model = "norm")
gjrgarch_t <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0), include.mean = T),
                       distribution.model = "std")

# Fitting the date to the models
test_data <- data[-1, "log_ret"]
garchfit_z <- ugarchfit(garch_z, test_data)
garchfit_t <- ugarchfit(garch_t, test_data)
igarchfit_z <- ugarchfit(igarch_z, test_data)
igarchfit_t <- ugarchfit(igarch_t, test_data)
egarchfit_z <- ugarchfit(egarch_z, test_data)
egarchfit_t <- ugarchfit(egarch_t, test_data)
gjrgarchfit_z <- ugarchfit(gjrgarch_z, test_data)
gjrgarchfit_t <- ugarchfit(gjrgarch_t, test_data)


# Combine Information Criterion from each model
result <- do.call("rbind", list(infocriteria(garchfit_z)[1:2,], infocriteria(garchfit_t)[1:2,],
                                infocriteria(igarchfit_z)[1:2,], infocriteria(igarchfit_t)[1:2,],
                                infocriteria(egarchfit_z)[1:2,], infocriteria(egarchfit_t)[1:2,],
                                infocriteria(gjrgarchfit_z)[1:2,], infocriteria(gjrgarchfit_t)[1:2,])
                  ) #[1:2]

row.names(result) <- c("GARCH(1,1)-z", "GARCH(1,1)-t", 
                       "I-GARCH(1,1)-z", "I-GARCH(1,1)-t", 
                       "E-GARCH(1,1)-z", "E-GARCH(1,1)-t",
                       "GJR-GARCH(1,1)-z", "GJR-GARCH(1,1)-t")

#############################################################
#2,2 normal Garch--------------------------------------------
garch22n = ugarchspec(variance.model = list(model = "fGARCH",
                                            submodel = "GARCH",
                                            garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(0,0),
                                        include.mean = T),
                      distribution.model = "norm")
fit22n = ugarchfit(garch22n,test_data)

#2,2 std Garch--------------------------------------------
garch22t = ugarchspec(variance.model = list(model = "fGARCH",
                                            submodel = "GARCH",
                                            garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(0,0),
                                        include.mean = T),
                      distribution.model = "std")
fit22t = ugarchfit(garch22t,test_data)

#2,2 normal iGarch--------------------------------------------
garch22n_i = ugarchspec(variance.model = list(model = "iGARCH",
                                              submodel = "GARCH",
                                              garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,0),
                                          include.mean = T),
                        distribution.model = "norm")
fit22n_i = ugarchfit(garch22n_i,test_data)

#2,2 normal iGarch--------------------------------------------
garch22t_i = ugarchspec(variance.model = list(model = "iGARCH",
                                              submodel = "GARCH",
                                              garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,0),
                                          include.mean = T),
                        distribution.model = "std")
fit22t_i = ugarchfit(garch22t_i,test_data)

#2,2 normal eGarch--------------------------------------------
garch22n_e = ugarchspec(variance.model = list(model = "eGARCH",
                                              submodel = "GARCH",
                                              garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,0),
                                          include.mean = T),
                        distribution.model = "norm")
fit22n_e = ugarchfit(garch22n_e,test_data)

#2,2 normal eGarch--------------------------------------------
garch22t_e = ugarchspec(variance.model = list(model = "eGARCH",
                                              submodel = "GARCH",
                                              garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,0),
                                          include.mean = T),
                        distribution.model = "std")
fit22t_e = ugarchfit(garch22t_e,test_data)

#2,2 normal gjrGarch--------------------------------------------
garch22n_gjr = ugarchspec(variance.model = list(model = "gjrGARCH",
                                                submodel = "GARCH",
                                                garchOrder = c(2,2)),
                          mean.model = list(armaOrder = c(0,0),
                                            include.mean = T),
                          distribution.model = "norm")
fit22n_gjr = ugarchfit(garch22n_gjr,test_data)

#2,2 normal gjrGarch--------------------------------------------
garch22t_gjr = ugarchspec(variance.model = list(model = "gjrGARCH",
                                                submodel = "GARCH",
                                                garchOrder = c(2,2)),
                          mean.model = list(armaOrder = c(0,0),
                                            include.mean = T),
                          distribution.model = "std")
fit22t_gjr = ugarchfit(garch22t_gjr,test_data)


order22 = do.call("rbind", list(infocriteria(fit22n)[1:2,],
                                infocriteria(fit22t)[1:2,],
                                infocriteria(fit22n_i)[1:2,],
                                infocriteria(fit22t_i)[1:2,],
                                infocriteria(fit22n_e)[1:2,],
                                infocriteria(fit22t_e)[1:2,],
                                infocriteria(fit22n_gjr)[1:2,],
                                infocriteria(fit22t_gjr)[1:2,]))

row.names(order22) <- c("GARCH(2,2)-z", "GARCH(2,2)-t", 
                       "I-GARCH(2,2)-z", "I-GARCH(2,2)-t", 
                       "E-GARCH(2,2)-z", "E-GARCH(2,2)-t",
                       "GJR-GARCH(2,2)-z", "GJR-GARCH(2,2)-t")

#############################################################

All = rbind(result,order22)
which(min(result[,"Akaike"]) == result[,"Akaike"])
which(min(result[,"Bayes"]) == result[,"Bayes"])
which(min(order22[,"Akaike"]) == order22[,"Akaike"])
which(min(order22[,"Bayes"]) == order22[,"Bayes"])
All[c("E-GARCH(1,1)-t","E-GARCH(2,2)-t"),]

# E-GARCH(1,1)-t has the lowest AIC and BIC values


### Q2. Develop a forecast for the 20-trading-day log return volatility on Jan 11, 2016 (end of day)
# -> sum of each of the 20 individual days' expected variance -> sqrt
forecast <- ugarchforecast(egarchfit_t, n.ahead = 20)
vola = as.vector(sigma(forecast))
volatility_pred = matrix(vola, nrow = 1,
                         ncol = length(sigma(forecast)))
colnames(volatility_pred) = c("T+1","T+2","T+3","T+4","T+5",
                               "T+6","T+7","T+8","T+9","T+10",
                               "T+11","T+12","T+13","T+14","T+15",
                               "T+16","T+17","T+18","T+19","T+20")
print(sigma(forecast))
#plot(forecast) # have to make a plot selection (unconditional)
plot(vola,
     main = "20-Trading-Day Log Return Volatility",
     type = "l")

volatility <- sqrt(sum(vola^2))
print(volatility)

mu = mean(test_data$log_ret)
V=c()
for (i in 1:length(test_data$log_ret)){
  V = c(V,(mu - test_data$log_ret[i])^2)
}
plot(abs(test_data$log_ret-mu),
     col = "lightgray", type = "l", xlab = "time",
     ylab = "volatility",
     main = "Actual vs Fitted")
lines(egarchfit_t@fit$sigma, type = "l",col = "blue")
legend("topright",legend = c("estimated","actual"),
       lty = 1, col = c("blue","grey"))

##### Problem 2: The Single Factor (Market) Model ------------------------
industry_48 <- read.csv("48_Industry_Portfolios_vw.csv", header = T) %>% as.data.table; gc()
famafrench <- read.csv("F-F_Research_Data_Factors.csv", header = T) %>% as.data.table; gc()
#industry_48[, Date := as.Date(as.character(X), "%Y%m")] -> Does not work

# Subset the data: year 1960~2015 & without -99.99 and missing values
industry_48$year <- as.numeric(substr(industry_48$X, 1, 4)) # get year info of each row
industry <- industry_48[ (industry_48$year >= 1960) & (industry_48$year < 2016), ] # year condition
industry[industry == -99.99] <- NA # changing -99.99 values to NA
industry[industry == -999] <- NA
industry_filtered <- industry %>% select_if( ~ !any(is.na(.)) ) # removing columns with NAs

# merge industry data and Rf from Fama-French data
data2 <- merge(industry_filtered, famafrench, by = "X")

### Q1. Regress the industry excess return on the market excess return
regression <- function(x) {
  out <- lm( (x-data2$RF) ~ data2$Mkt.RF )
  summary <- summary(out)
  
  alpha <- summary$coefficients[1, 1] # intercept for (c)
  beta <- summary$coefficients[2, 1] # beta for (a)                     #out$coef[2]
  hac <- sqrt(vcovHC(out, type = "HC")[2, 2]) # standard error for (a)  #print(hac)
  rsquared <- summary$r.squared # R^2 for (b)
  
  return(c(alpha, beta, hac, rsquared)) # does not return column names
}

industry_coef <- sapply(data2[, 2:44], regression) #data2[, 2:44]
row.names(industry_coef) <- c("Intercept", "Beta", "HAC", "R-Squared")

# Q1 (a). Bar plot
plots <- barplot(height = industry_coef["Beta", ], ylim = c(0, 1.6), xaxt = "n",
                 main = "Industry Betas", ylab = "Beta", xlab = "Industry", border = "black",
                 legend.text = F)
axis(1, at = plots, labels = 1:ncol(industry_coef))
segments(plots, industry_coef["Beta", ] - 2*industry_coef["HAC", ], 
         plots, industry_coef["Beta", ] + 2*industry_coef["HAC", ], lwd = 1.5)
arrows(plots, industry_coef["Beta", ] - 2*industry_coef["HAC", ], 
       plots, industry_coef["Beta", ] + 2*industry_coef["HAC", ], lwd = 1, angle = 90,
       code = 3, length = 0.03)

# Q1 (b). Range of Beta and R-Squared
beta_range <- c(min = min(industry_coef["Beta", ]), 
                mean = mean(industry_coef["Beta", ]), 
                max = max(industry_coef["Beta", ]))
rsquared_range <- c(min = min(industry_coef["R-Squared", ]), 
                    mean = mean(industry_coef["R-Squared", ]), 
                    max = max(industry_coef["R-Squared", ]))
beta_range
rsquared_range

# Q1 (c). Plot Intercept vs. Beta
plot(industry_coef["Beta", ], industry_coef["Intercept", ])
#abline(lm(industry_coef["Beta", ] ~ industry_coef["Intercept", ]))


### Q2. Rolling regression of 5 years of data
# 11 betas for the each industry
years <- seq(from = 1960, to = 2015, by = 5)
industry_coef_5y <- data.frame()
for( i in 1:(length(years)-1) ) {
  # regression function to get the betas
  regression2 <- function(x) {
    out <- lm( (x-temp$RF) ~ temp$Mkt.RF )
    beta <- out$coef[2]            
    return(beta)
  }
  
  temp <- data2[ (data2$year >= years[i] & data2$year < years[i+1]), ]
  industry_coef_5y <- rbind(industry_coef_5y, sapply(temp[, 2:44], regression2))
}

# Result of betas
colnames(industry_coef_5y) <- colnames(industry_coef)
#industry_coef_5y

industry_corr <- data.frame()
# Calculate correlations of adjacent betas for each industry
correlation <- function(x) {
  len <- length(x) # 11
  return(cor( x = x[1:(len-1)], y = x[2:len] ))
}

# Result of correlations
industry_corr <- sapply(industry_coef_5y, correlation) 
plots2 <- barplot(industry_corr, xaxt = "n", main = "The Correlations of Adjacent Betas",
                  ylim = c(-0.4, 1), ylab = "Correlation", xlab = "Industry")
axis(1, at = plots2, labels = 1:length(industry_corr))
