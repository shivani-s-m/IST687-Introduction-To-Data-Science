################################################
# Intro to data science, Sampling: Week 4
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 4
# Date due:  9/20/2020
#
# Set working directory 
setwd("~/Desktop/IST687/Homework")

#---------------------------------------------------------------------------
# Part A: 	Write	a	function	to	compute statistics	for a	vector of	numeric	values

# 1. defining a function that returns the list of statistics of the vector
vectorStats <- function(vectorInput){
  meanVector <- mean(vectorInput)
  medianVector <- median(vectorInput)
  return( c(meanVector, medianVector))
}

# 2.  testing by calling it and giving input from 1 to 10
# The mean and median both are 5.5, so the function works
vectorStats(1:10)

# 3. Adding max, min and std deviation to the function's return
vectorStats <- function(vectorInput){
  meanVector <- mean(vectorInput)
  medianVector <- median(vectorInput)
  maxVector <- max(vectorInput)
  minVector <- min(vectorInput)
  sdVector <- sd(vectorInput)
  return( c(meanVector, medianVector, maxVector, minVector, sdVector))
}

# 4. retesting by calling it and giving input from 1 to 10
# The max, min and std dev are 10, 1, 3.02765 respectively, so the function works
vectorStats(1:10)

#---------------------------------------------------------------------------
# Part B:	Sample repeatedly from the	airquality	built-in	data frame 
# 5. Using the air quality data frame and storing into a variable
# 
myAQuality <- airquality
View(myAQuality)

# The Ozone column is the mean ozone in ppm, 
# Solar.R is solar radiation, 
# Wind is avg wind speed in miles per hour
# Temp is maximum daily temperature at La Guardia Airport
# Month is numeric month
# Day is numeric day of month
help("airquality")

# 6. Sample five wind observations without replacing
set.seed(0)
sampleWind <- sample(myAQuality$Wind, 5,  replace = TRUE )

# 7. Calling function vectorStats on wind samples
vectorStats(sampleWind)

# 8. Sampling the population 10 times.
replicate(10, sample(myAQuality$Wind, 5, replace = TRUE), simplify = TRUE)

# 9. Why does the replicate function not produce duplicate results?
# The different ways to sample with replacing are 153^5. To find two samples with the same values is highly
# improbable.

# 10. Running with 1000 replications and storing the value
sampleWind1000 <- replicate(1000, sample(myAQuality$Wind, 5,  replace = TRUE ), simplify = TRUE)

# 11. Generate a histogram of means from the previous result
# Histogram show highest frequency  of over 250 present from 9-10 and 10-11 with over 100 instances in
# 8-9, 11-12
mean1 <- replicate(1000, mean(sample(myAQuality$Wind, 5,  replace = TRUE )), simplify = TRUE)
hist(mean1)

# 12. Generating histogram for 1000 samples of size 50
mean(myAQuality$Wind)
# The law of large numbers works here, as we increase the sample size to 50 from 5
# the highest frequency is from range 9-11 which is range in which the mean is present in the population
mean2 <- replicate(1000, mean(sample(myAQuality$Wind, 50,  replace = TRUE )), simplify = TRUE)
hist(mean2)

