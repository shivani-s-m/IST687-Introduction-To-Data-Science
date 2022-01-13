################################################
#
# IST687, Homework Assignment Week 3
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 3
# Date due: 09-13-2020
#
#
# Set working directory 
setwd("~/Desktop/IST687/Homework") # Change to the folder containing your homework data files

# Step 1: Use read_csv() to read a csv file into a data frame

# A. Read csv file from a URL by using the package readr
library(tidyverse)
urlToRead <- "https://raw.githubusercontent.com/CivilServiceUSA/us-states/master/data/states.csv"
dfStates <- data.frame(read_csv(url(urlToRead)))

# Step 2: Create	a new data frame that	only	contains	states	with	Twitter	URLs

# B. Examine the dfStates data frame
# View() displays the data in a spreadsheet style format
View(dfStates)
# head() shows the topmost rows in the dataframe
head(dfStates,1)
# tail() shows the bottom-most rows in the data frame
tail(dfStates)

# C. Create a variable to indicate if state is missing a Twitter URL
noTwitter <-  is.na(dfStates$twitter_url)

# D. Use table() to summarize noTwitter
# The number of TRUE instances in noTwitter shows that 15 states have missing values for twitter URL
table(noTwitter)

# E. Create a new data frame to store only those states that have a twitter URL
twitterStates <- dfStates[!noTwitter,]

# F. Confirming it is a 35 by 19 data frame using dim()
dim(twitterStates)

# Step 3:	Calculate	the	mean	for	each	of	the	three numeric	variables

# G. Calculate mean of three numerical values
str(twitterStates)
# The 3 columns that have numeric values are admission_number, population and population_rank
mean(twitterStates$admission_number)
twitterStatesMeanPop <- mean(twitterStates$population)
mean(twitterStates$population_rank)

# H. Noting the mean of the population 
# Mean of the population is 6,532,234

# I. Creating another dataframe and finding the mean of the population of the remaining 15 states
noTwitterStates <- dfStates[noTwitter,]
noTwitterStatesMeanPop <- mean(noTwitterStates$population)
noTwitterStatesMeanPop/twitterStatesMeanPop
# The ratio of no Twitter States population to Twitter States mean Population is 0.89

# Step 4: Extract	the	Twitter	handle	from	the	URL
# J. Using gsub() to remove the beginning from twitter URL
gsub("https://twitter.com/","",	twitterStates$twitter_url)

# K. Examine results from J
# The gsub() function seems to work in most instances but it only removes ones with https: and the ones with 
# http: remain unaffected. It only removes the ones which are an exact match

# L. Assign the results from J to a column
twitterStates$handle <- gsub("https://twitter.com/","",	twitterStates$twitter_url)

# Step 5: Create	a	function	to	extract	Twitter	handles

# M. Defining function to extract Twitter handles
getTwitterHandleFromURL	<- function(URL)	{
  #This funtion removes the three variations of twitter URLs to extract twitter handles
  #Removing the part beginning with "https://twitter.com/" from the input argument
  fixTry1	<- gsub("https://twitter.com/","",	URL)
  # Removing the part beginning with "http://twitter.com/" from the previous statement's output
  fixTry2	<- gsub("http://twitter.com/","",	fixTry1)
  # Removing the part beginning with "http://www.twitter.com/" from the previous statement's output
  fixTry3	<- gsub("http://www.twitter.com/","",	fixTry2)
  return(fixTry3)
}

# N. Calling the function for the twitter handles in the twitter_url colums\
getTwitterHandleFromURL(twitterStates$twitter_url)
#There is one erroneous entry which is a Facebook url and that is why our function is unable to extract 
# a twitter handle and ends up giving the original erroneous entry as the output

# O. Assigning the results of N to a column in twitterStates
twitterStates$handle	<- getTwitterHandleFromURL(twitterStates$twitter_url)

## Using the gsub() method can be very useful in data cleaning where bulk of the data in a column is identical
## and we take out the important parts out to make the data compact without losing meaningful data.
## is.na() can help to identify missing data and a call can be made whether to interpolate the missing data or 
## skip the rows with the missing data like it is was done in this assignment
