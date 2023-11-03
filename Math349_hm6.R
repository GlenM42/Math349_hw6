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

z = diff(Toothpaste$y, differences = 1)
plot(z, type='l')
adf.test(z, alternative = "stationary")

# This is much better, but the p-value is still below 5%. Let's increase the 
# difference.

z = diff(Toothpaste$y, differences = 2)
plot(z, type='l')
adf.test(z, alternative = "stationary")

# Now we do have a stationary time series, as the p-value is lower
# than 5%.

par(mfrow=c(1,2))
Acf(z)
Pacf(z, main ="PACF for Differenced Series")

# ACF cuts off at 2, and the PACF cuts off at 2. Therefore we have p=2, q=2,
# and the d = 2. 

sarima(Toothpaste$y, 2,2,2)
sarima(Toothpaste$y, 1,1,1)
sarima(Toothpaste$y, 1,2,1)

# We try a couple of similar models just to check the numbers, and it looks
# like by the combination of all, the best one has parameters 2,2,2. 

## Problem 2 -- part C

sarima(Toothpaste$y, 2,2,2)

# Judging based on the Normal QQ plot, the normality assumption holds.
# The variance is fairly constant throughout the time interval. 
# The p-values are non-zero, so the independence assumption holds. 


## Problem 2 -- part D

sarima.for(Toothpaste$y, 6,2,2,2)

# 1039.040 +- 2.693958 * 1.96
# 1046.730 +- 5.542272 * 1.96
# 1053.865 +- 7.983641 * 1.96
# 1060.852 +- 10.153826 * 1.96
# 1067.801 +- 12.165849 * 1.96
# 1074.740 +- 14.087426 * 1.96

