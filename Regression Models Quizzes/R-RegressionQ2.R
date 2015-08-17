# Coursera
# R Regression Models

# Quiz 2




# Question 1

# Consider the following data with x as the predictor and y as as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.

# call
model <- lm(y ~ x)

# see results
summary(model) # p-value: 0.05296




# Question 2

# Consider the previous problem, give the estimate of the residual standard deviation.
summary(model) # Residual standard error: 0.223




# Question 3

# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). 
# Get a 95% confidence interval for the expected mpg at the average weight. 
# What is the lower endpoint?

data(mtcars)
weight <- mtcars$wt
mpg <- mtcars$mpg
fit <- lm(mpg ~ weight)
predict(fit,data.frame(weight=mean(weight)), interval="confidence") # lwr 18.99098




# Question 4

# Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?
help(mtcars)
# wt	 Weight (lb/1000)
# The estimated expected change in mpg per 1,000 lb increase in weight.




# Question 5

# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs). 
# A new car is coming weighing 3000 pounds. 
# Construct a 95% prediction interval for its mpg. What is the upper endpoint?

predict(fit,data.frame(weight=(3)), interval="prediction") # upr 27.57355




# Question 6

# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs).
# A “short” ton is defined as 2,000 lbs. 
# Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. 
# Give the lower endpoint.

# Create the model
fit2 <- lm(mpg ~ I(weight / 2))

# Extract coefficients of mean, se and df 
tbl2 <- summary(fit2)$coefficients
mean <- tbl2[2,1]
se <- tbl2[2,2]
df <- fit2$df

# Construct the Two sides T-Tests
mean + c(-1,1) * qt(0.975, df=df) * se # lwr -12.97262   upr-8.40527




# Question 7 

# If my X from a linear regression is measured in centimeters and, 
# I convert it to meters what would happen to the slope coefficient?

# It would get multiplied by 100.




# Question 8

# I have an outcome, Y, and a predictor, X 
# and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1. 
# What would be the consequence to the subsequent slope and intercept,
# if I were to refit the model with a new regressor, X+c for some constant, c?

# The new intercept would be β0−cβ^1



# Question 9

# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. 
# About what is the ratio of the the sum of the squared errors, ∑ni=1(Yi−Y^i)2 
# when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?


# get residual from model 1
fitRes <- fit$residuals


# create new model and get residual
fitIntercept <- lm(mpg ~ 1, data = mtcars)
fitInterceptRes <- fitIntercept$residuals


# find the ratio
sum(fitRes^2)/sum(fitInterceptRes^2) # 0.2471672



# Question 10
# Do the residuals always have to sum to 0 in linear regression?
sum(fitRes) # If an intercept is included, then they will sum to 0.


