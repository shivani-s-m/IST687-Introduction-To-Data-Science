################################################
# IST687, Homework- 2
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 2
# Date due:  9/6/2020
#
# Loading the mtcars data set
myCars <- mtcars
summary(myCars)
View(myCars)

# Step 1: Exploring mpg column 
# A. Finding mean mpg 
mean(myCars$mpg)

# B. Finding highest mpg
max(myCars$mpg)

# c. Finding lowest mpg
min(myCars$mpg)

# D. Creating sorted data frame based on mpg 
mtCarsSorted <- myCars[order(myCars$mpg),]

# Step 2: Highest hp
# E. Which is better high or low hp?
# Ans: The hp of a car is a strong factor in its overall performance, therefore, higher HP is better

# F. Which car has highest HP?
myCars[which.max(myCars$hp),]

#G. Which car has lowest HP?
myCars[which.min(myCars$hp),]

# Step 3: Combination of mpg and hp
# H. Scaling mpg column
scaledMPG <- scale(myCars$mpg, center=0, scale=T)

# I. What does scaling do?
# Ans. For different units of measurements, it normalizes the values and makes it easy to compare.

# J. Scaling HP
scaledHP <- scale(myCars$hp, center=0, scale=T)

# K. Adding the two scaled measurements
# This way we can measure which cars do well in both of the parameters having separate units of measurements
myCars$scaledSum <- scaledHP + scaledMPG

# L. Car with highest combination of mpg and hp
myCars[(which.max(myCars$scaledSum)),]

# M. Creating sorted list based on combined mpg and hp
myCarsCombined <- myCars[order(myCars$scaledSum),]
View(myCarsCombined)
