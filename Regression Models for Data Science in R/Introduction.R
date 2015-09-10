# Introduction

# Exploratory Analysis of Galton's Data




# Install packange "UsingR" and load the library
install.packages("UsingR")
library(UsingR)

# Install packange "reshape" and load the library
install.packages("reshape")
library(reshape)
# Info on melting data http://www.r-bloggers.com/melt/

# Load data 'galton'
data(galton)
str(galton)
summary(galton)

#melt data
long <- melt(galton)

g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g




# Using manipulate to find the least square estimate
install.packages("manipulate")
library(manipulate)
myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g 




# Comparing children's heights and parents heights
ggplot(galton, aes(x = parent, y = child)) + geom_point()

# The overplotting is clearly hiding some data. 
# Here you can get the code to make the size and color of the points be the frequency.


install.packages("dplyr")
library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g



# Regression through the origin
# Solution

lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

# Coefficients:
# I(parent - mean(parent))  
# 0.6463  

# Coefficients suggest that to every inch increase in parents height, we increase 0.646 in the child's height

