# OLS

# Ordinary Least Squares

# Ordinary least squares (OLS) is the workhorse of statistics. 
# It gives a way of taking complicated outcomes and explaining behavior (such as trends) using linearity. 
# The simplest application of OLS is fitting a line.

# Load data 'galton'
library(UsingR)
data(galton)
str(galton)
summary(galton)



# General least squares for linear equations
# https://en.wikipedia.org/wiki/Ordinary_least_squares




# Revisiting Galton's data

# GLS 
# Let's double check our calculations using R

y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

#       (Intercept) x
# [1,]  23.94153    0.6462906
# [2,]  23.94153    0.6462906

# We can see that the result of lm is identical to hard coding the fit ourselves. 
# Let's Reversing the outcome/predictor relationship.

beta1r <- cor(y, x) *  sd(x) / sd(y)
beta0r <- mean(x) - beta1 * mean(y)
rbind(c(beta0r, beta1r), coef(lm(x ~ y)))

#       (Intercept) x
# [1,]  24.30325    0.3256475
# [2,]  46.13535    0.3256475




# Centering data
# Now let's show that regression through the origin yields an equivalent slope if you center the data first.

yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])

#               x 
# 0.6462906 0.6462906 




# Normalizing Data
# Now let's show that normalizing variables results in the slope being the correlation.

ynorm <- (y - mean(y))/sd(y)
xnorm <- (x - mean(x))/sd(x)
c(cor(y, x), cor(ynorm, xnorm), coef(lm(ynorm ~ xnorm))[2])

#                     xnorm 
# 0.4587624 0.4587624 0.4587624 




# Exercise


# Question 1

# Install and load the package UsingR and load the father.son data with data(father.son). 
# Get the linear regression fit where the son's height is the outcome and the father's height is the predictor. 
# Give the intercept and the slope, plot the data and overlay the fitted regression line.
data(father.son)
names(father.son)

yfs <- father.son$sheight
xfs <- father.son$fheight
beta1fs <- cor(yfs, xfs) *  sd(yfs) / sd(xfs)
beta0fs <- mean(yfs) - beta1fs * mean(xfs)
rbind(c(beta0fs, beta1fs), coef(lm(yfs ~ xfs)))

library(ggplot2)
g = ggplot(father.son, aes(x=fheight, y=sheight))
g = g+geom_point()
g = g+ geom_smooth(method=lm, se=FALSE, lwd=2)
g

# Answer: 
#       (Intercept) x
# [1,]  33.8866     0.514093
# [2,]  33.8866     0.514093




# Question 2
# Refer to problem 1. 
# Center the father and son variables and refit the model omitting the intercept. 
# Verify that the slope estimate is the same as the linear regression fit from problem 1.

ycfs <- yfs - mean(yfs)
xcfs <- xfs - mean(xfs)
beta1cfs <- sum(ycfs*xcfs) / sum(xcfs^2)
c(beta1cfs, coef(lm(ycfs~xcfs))[2])

# Answer: 
#          xcfs 
# 0.514093 0.514093 





# Question 3
# Refer to problem 1. 
# Normalize tbe father and son data and see that the fitted slope is the correlation.


ynormfs <- (yfs - mean(yfs))/sd(yfs)
xnormfs <- (xfs - mean(xfs))/sd(xfs)
c(cor(ynormfs,xnormfs), coef(lm(ynormfs~xnormfs))[2])


# Answer: 
#           xnormfs 
# 0.5013383 0.5013383 




# Question 4
# Go back to the linear regression line from Problem 1. 
# If a father's height was 63 inches, what would you predict the son's height to be?
beta0fs + beta1fs*63

# Answer: 66.27




# Question 5
# Consider a data set where the standard deviation of the outcome variable is double that of the predictor. 
# Also, the variables have a correlation of 0.3. 
# If you fit a linear regression model, what would be the estimate of the slope?

# Answer: 
# We know that the fit is cor(y,x)*sd(y)/sd(x)
# So cor(y,x) = 0.3, and ratio of sd(y)/sd(x) = 2
# Therefore, the slope is 0.3*2 = 0.6




# Question 6
# Consider the previous problem. 
# The outcome variable has a mean of 1 and the predictor has a mean of 0.5. 
# What would be the intercept?

# Answer:
# We know that intercept / beta0 = mean(y) - beta1 * mean(x)
# Therefore, beta0 = 1 - 0.6*0.5 = 0.7




# Question 7
# True or false, if the predictor variable has mean 0, 
# the estimated intercept from linear regression will be the mean of the outcome?

# Answer:
# We know that intercept / beta0 = mean(y) - beta1 * mean(x)
# Therefore, beta0 = mean(y) - beta1*0.0 = mean(y)
# TRUE




# Question 8
# Consider problem 5 again. What would be the estimated slope if the predictor and outcome were reversed?

# Answer: 
# swapping the fit = cor(y,x)*sd(x)/sd(y)
# So cor(y,x) = 0.3, and ratio of sd(y)/sd(x) = 0.5
# Therefore, the slope is 0.3*0.5 = 0.15



