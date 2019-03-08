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
# -> GARCH / I-GARCH / GARCH-M / EGARCH / GJR-GARCH (total 5) * normal / t (total 2) * order 1,1 / ...?
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

# GARCH-M not supported in rugarch package
# M stands for multivariate..?

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

# E-GARCH(1,1)-t has the lowest AIC and BIC values


### Q2. Develop a forecast for the 20-trading-day log return volatility on Jan 11, 2016 (end of day)
# -> sum of each of the 20 individual days' expected variance -> sqrt
forecast <- ugarchforecast(egarchfit_t, n.ahead = 20)
plot(forecast) # have make a plot selection (1 or 3 works) -> 3 shows the prediction results
plot(sigma(forecast), main = "20-Trading-Day Log Return Volatility")

volatility <- sqrt(sum(sigma(forecast)^2)) # 0.03474748
volatility

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
barplot(industry_coef["Beta", ])
# Q1 (b). Range of Beta and R-Squared
min(industry_coef["Beta", ])
mean(industry_coef["Beta", ])
max(industry_coef["Beta", ])
min(industry_coef["R-Squared", ])
mean(industry_coef["R-Squared", ])
max(industry_coef["R-Squared", ])
# Q1 (c). Plot Intercept vs. Beta
plot(industry_coef["Beta", ], industry_coef["Intercept", ])


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
for( i in 1:ncol(industry_coef_5y) ) {
  # calculate correlations in each industry
  correlation <- function(x) {
    out <- lm( (x-temp$RF) ~ temp$Mkt.RF )
    beta <- out$coef[2]            
    return(beta)
  }

  industry_coef_5y <- rbind(industry_coef_5y, sapply(temp[, 2:44], regression2))
}

# Calculate correlations of adjacent betas for each industry
correlation <- function(x) {
  len <- length(x) # 11
  return(cor( x = x[1:(len-1)], y = x[2:len] ))
}

# Result of correlations
industry_corr <- sapply(industry_coef_5y, correlation) 
barplot(industry_corr)


