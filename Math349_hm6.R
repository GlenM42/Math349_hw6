library(astsa)

## Problem 1 -- part A

library(ggplot2)
library(forecast)
library(tseries)

# plot of oil prices per barrel versus time
ggplot(oil, aes(x, y)) + geom_line()+ylab("Weekly Crude Oil Prices in dollars per barrel") + xlab("Time")

adf.test(oil, alternative = "stationary")

# Even though the test value gives us p<5%, as we can see on the graph,
# the mean is hardly stationary. Moreover, its ACF dies down
# very slowly, without any cuts. Therefore there is need 
# for transforming the data. 

z = diff(oil, differences = 1)
plot(z, type='l')
adf.test(z, alternative = "stationary")

# Now we do have a stationary data. 

## Problem 1 -- part B

par(mfrow=c(1,2))
Acf(z)
Pacf(z, main ="PACF for Differenced Series")

# As we can see on the ACF graph, the values lag around a quarter of the 
# first year. That would give us the order of MA model of 10.

# For the order of the AR model, we have to look at the PACF graph.
# There, the cut off happens a bit earlier that quarter. That would give us
# the order of 8.

sarima(z, 8,1,10)
sarima(z, 9,1,11)
sarima(z, 7,1,9)

# Now, looking at the combination of AIC, AICC, and BIC, the first model 
# is the best one. 

## Problem 1 -- part C
sarima(z, 8,1,10, no.constant = TRUE)

# The normality assumption holds, since most of the data is on the line;
# The variance is fairly constant (especially excluding the area around 2009)
# However, the p-values for Ljung-Box show us that the independence assumption
# is violated, and there is strong dependence. 

## Problem 1 -- part D

sarima.for(oil, 6,8,1,10)

# Therefore, the 95% prediction intervals are:
# (67.73932 +- 1.96*2.429506)
# (67.60261 +- 1.96*3.722673) 
# (67.51752 +- 1.96*4.707686) 
# (68.03066 +- 1.96*5.605405)
# (67.97478 +- 1.96*6.375258)
# (66.95087 +- 1.96*7.122402)

## Problem 2 -- part A

library(readxl)
Toothpaste <- read_excel("/Users/hm/Desktop/Roosevelt_University/MATH349/hm6/Toothpaste.xlsx")
attach(Toothpaste)

ggplot(Toothpaste, aes(t, y)) + geom_line()+ylab("Weekly Toothpaste Sales") + xlab("Time")

adf.test(Toothpaste$y, alternative = "stationary")

# Clearly, based on graph, and the ADF test, we have to transform the data
# to make it stationary.

par(mfrow=c(1,2))
Acf(Toothpaste$y)
Pacf(Toothpaste$y, main ="PACF for Differenced Series")

z = diff(Toothpaste$y, differences = 1)
plot(z, type='l')
adf.test(z, alternative = "stationary")

# This is much better, but the p-value is still below 5%. Let's increase the 
# difference.

z = diff(Toothpaste$y, differences = 2)
plot(z, type='l')
adf.test(z, alternative = "stationary")
