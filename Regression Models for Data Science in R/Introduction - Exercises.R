# Introduction

# Exercises




# Question 1

# Consider the dataset given by x=c(0.725,0.429,-0.372 ,0.863) 
# What value of mu minimizes sum((x - mu)^2)? 

# mu is the arithmetic average
x=c(0.725,0.429,-0.372 ,0.863) 
mean(x)

# Answer: 0.41125




# Question 2
# Reconsider the previous question i.e finding mu. 
# Suppose that weights were given, w = c(2, 2, 1, 1) 
# so that we wanted to minimize sum(w * (x - mu) ^ 2) for mu. 
x=c(0.725,0.429,-0.372 ,0.863) 
w = c(2, 2, 1, 1) 
sum(w*x)/sum(w)

# Answer: 0.4665




# Question 3

# Take the Galton and obtain the regression through the origin slope estimate 
# where the centered parental height is the outcome and the child's height is the predictor. 

# Solution: reverse the equation structure
lm(I(parent - mean(parent))~ I(child - mean(child)) - 1, data = galton)

# Or, you can use
lm(galton$parent ~ galton$child)

# Answer: 0.3256 