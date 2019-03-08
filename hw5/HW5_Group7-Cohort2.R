### title: "MGMTMFE407-2 HW5"
### author: "Cohort 2, Group 7 - Hyeuk Jung, Jiaqi Li, Xichen Luo, Huanyu Liu"
### date: "March , 2019"

library(data.table)
library(dplyr)
library(sandwich)
library(ggplot2)

#http://www.sthda.com/english/wiki/print.php?id=206
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html


# SD: the same as sqrt(eigenvalues$values) (should be!!)
# Eigenvectors: loading, explains how the (original data ex. yields) loads on the PCs(components) 
#    --> as long as the direction is the same, negative or positive values do not matter
# ex. level -> horizontal, loads to the data with the same amount..? / slope / curvature


##### Problem 1: The Principal Component Analysis ------------------------
industry_48 <- read.csv("48_Industry_Portfolios_vw.csv", header = T) %>% as.data.table; gc()
famafrench <- read.csv("F-F_Research_Data_Factors.csv", header = T) %>% as.data.table; gc()

### Data Cleaning and Manipulation
# 1. Subset the data: year 1960~2015
industry_48$year <- as.numeric(substr(industry_48$X, 1, 4)) # get year info of each row
industry <- industry_48[ (industry_48$year >= 1960) & (industry_48$year < 2016), ] # year condition #
# 2. Remove columns if it holds -99.99 or missing values
industry[industry == -99.99] <- NA # changing -99.99 values to NA
industry[industry == -999] <- NA
industry_filtered <- industry %>% select_if( ~ !any(is.na(.)) ) # removing columns with NAs
# 3. Merge industry data and Rf from Fama-French data
data <- merge(industry_filtered, famafrench, by = "X")


### Q1. Eigenvalues for the sample var-cov matrix of the excess returns -----------------------------------------
# 1. Calculate the excess return of each industry
ex.ret <- (data[, 2:44] - data$RF) #/100 # change into percentile dim(ex.ret)
# 2. Get the var-cov matrix
varcov <- cov(ex.ret) 
# 3. Get the eigenvalues
eigenvalues <- eigen(varcov) #$values #%>% as.data.table; gc()
# 4. Plot the fraction of variance explained by each eigenvalue
var <- sum(eigenvalues$values)
var_fraction <- eigenvalues$values / var
var_fraction_plot <- barplot(var_fraction, main = "Fraction of Variance Explained by Eigenvalues", 
                             ylab = "Fraction of Variance", xlab = "Industry", ylim = c(0, 0.7))

### Q2. The 3 first principal components ------------------------------------------------------------------------
# 2.(a). Explanation power of these three components
sum(var_fraction[1:3]) #0.6987876

# 2.(b). Mean sample return, SD, and correlation of these three components
prcomp_result <- prcomp(ex.ret)
summ_prcomp_result <- summary(prcomp_result) #summ_prcomp_result$rotation
## Using prcomp results (did not find the PC1 ~ PC3 values from princomp result)
largest_pca <- as.data.frame(prcomp_result$rotation[, 1:3])
# Mean #apply(largest_pca, 2, mean)
factor_return <- as.matrix(ex.ret) %*% as.matrix(largest_pca) # monthly returns for each industry*loadings
facor_return_mean <- apply(factor_return, 2, mean)
facor_return_mean
# SD
prcomp_result$sdev[1:3] # 32.612279  9.679627  7.862954
# Corr
cor(factor_return)
# Factor loadings
ggplot(data = largest_pca) + 
  geom_line(aes(x = 1:length(largest_pca$PC1), y = PC1), color = "red") +
  geom_line(aes(x = 1:length(largest_pca$PC2), y = PC2), color = "blue") +
  geom_line(aes(x = 1:length(largest_pca$PC3), y = PC3), color = "black") +
  labs(title = "PC1~3", x = "Industry", y = "PC")

# 2.(c). Predict returns
Re_real_mean <- apply(ex.ret, 2, mean) # Realized avg

# TA's approach
y <- as.matrix(ex.ret)
X <- solve(prcomp_result$rotation) %*% t(y)
X1 <- as.matrix(X[1, ])
X2 <- as.matrix(X[2, ])
X3 <- as.matrix(X[3, ])
T <- length(y[, 1]) # 672 dim(X1%*%t(as.matrix(largest_pca[, 1])))
F3 <- X1%*%t(as.matrix(largest_pca[, 1])) + X2%*%t(as.matrix(largest_pca[, 2])) + X3%*%t(as.matrix(largest_pca[, 3]))
F3_mean <- apply(F3, 2, mean)

# Mine
betas <- t(largest_pca) # betas for each industry #dim(largest_pca) -> 43by3; dim(betas) -> 3by43
#factor_return <- as.matrix(ex.ret) %*% as.matrix(largest_pca) # dim(ex.ret) -> 672by43 x dim(largest_pca) -> 43by3
Re_predict <- factor_return %*% betas # dim(factor_return) -> 672by3 x dim(betas) -> 3by43
Re_predict_mean <- apply(Re_predict, 2, mean) #dim(betas) dim(factor_return) -> 672by3

plot(x = F3_mean, y = Re_real_mean, pch = 19, col = "red", ylim = c(0, 1), xlim = c(0, 1))
abline(0, 1)
par(new=TRUE)
plot(x = Re_predict_mean, y = Re_real_mean, pch = 20, col = "blue", ylim = c(0, 1), xlim = c(0, 1))
abline(0, 1)

final <- cbind(Re_real_mean, Re_predict_mean, F3_mean)

ggplot(data = as.data.frame(final)) + 
  geom_line(aes(x = 1:length(final[, 1]), y = Re_real_mean), color = "red") +
  geom_line(aes(x = 1:length(final[, 2]), y = Re_predict_mean), color = "blue") +
  geom_line(aes(x = 1:length(final[, 3]), y = F3_mean), color = "black") +
  labs(title = "Mean Returns", x = "Industry", y = "Returns")


# 2.(d). R-squared
rsquard_1 <- 1 - var(Re_real_mean - F3_mean) / var(Re_real_mean)
rsquard_2 <- 1 - var(Re_real_mean - Re_predict_mean) / var(Re_real_mean)

rsquard_1 # -0.595244...? 
rsquard_2


### Q3. 25 FF Portfolios ----------------------------------------------------
industry_25 <- read.csv("25_Portfolios_5x5_vw.csv", header = T) %>% as.data.table; gc()
### Data Cleaning and Manipulation
# 1. Subset the data: year 1960~2015
industry_25$year <- as.numeric(substr(industry_25$X, 1, 4)) # get year info of each row
industry_2 <- industry_25[ (industry_25$year >= 1960) & (industry_25$year < 2016), ] # year condition
# 2. Remove columns if it holds -99.99 or missing values
industry_2[industry_2 == -99.99] <- NA # changing -99.99 values to NA
industry_2[industry_2 == -999] <- NA
industry_2_filtered <- industry_2 %>% select_if( ~ !any(is.na(.)) ) # removing columns with NAs
# 3. Merge industry data and Rf from Fama-French data
data_2 <- merge(industry_2_filtered, famafrench, by = "X")


# 3.(a). Eigenvalues for the sample var-cov matrix of the excess returns
# 1. Calculate the excess return of each industry
ex.ret_2 <- (data_2[, 2:26] - data$RF) #/100 # change into percentile
dim(ex.ret_2)
# 2. Get the var-cov matrix
varcov_2 <- var(ex.ret_2) 
# 3. Get the eigenvalues
eigenvalues_2 <- eigen(varcov_2) #$values #%>% as.data.table; gc()
# 4. Plot the fraction of variance explained by each eigenvalue
var_2 <- sum(eigenvalues_2$values)
var_fraction_2 <- eigenvalues_2$values / var_2
var_fraction_plot_2 <- barplot(var_fraction_2, main = "Fraction of Variance Explained by Eigenvalues", 
                             ylab = "Fraction of Variance", xlab = "Industry",
                             ylim = c(0, 1))

# 3.(b). How many factors?
summary(prcomp(ex.ret_2)) # PC1 ~ PC3 or PC4 seems reasonable; ~PC3 covers 93% and ~PC4 covers 95%

