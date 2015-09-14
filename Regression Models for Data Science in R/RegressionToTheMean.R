# Regression to the mean

# A historically famous idea, regression to the mean
# Here is a fundamental question. Why is it that the children of tall parents tend to be tall, 
# but not as tall as their parents? Why do children of short parents tend to be short, 
# but not as short as their parents? Conversely, why do parents of very short children, tend to be short, 
# but not a short as their child? And the same with parents of very tall children?

# We can try this with anything that is measured with error. 
# Why do the best performing athletes this year tend to do a little worse the following? 
# Why do the best performers on hard exams always do a little worse on the next hard exam?

# These phenomena are all examples of so-called regression to the mean




library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g




# Exercises


# Question 1
# You have two noisy scales and a bunch of people that you'd like to weigh. You weigh each person on both scales. 
# The correlation was 0.75. If you normalized each set of weights, 
# what would you have to multiply the weight on one scale to get a good estimate of the weight on the other scale? 

# Answer: Multiply it with the correlation = 0.75




# Question 2
# Consider the previous problem. 
# Someone's weight was 2 standard deviations above the mean of the group on the first scale. 
# How many standard deviations above the mean would you estimate them to be on the second? 

# Answer: 2 * correlation = 2 * 0.75 = 1.50 




# Question 3
# You ask a collection of husbands and wives to guess how many jellybeans are in a jar. 
# The correlation is 0.2. 
# The standard deviation for the husbands is 10 beans while the standard deviation for wives is 8 beans. 
# Assume that the data were centered so that 0 is the mean for each. 
# The centered guess for a husband was 30 beans (above the mean). 
# What would be your best estimate of the wife's guess? 

# Answer:
# This is a GLS problem
# Beta1 = 0.2*(8/10) = 0.16
# Threrefore, the estimate would be 30*0.16 = 4.8

