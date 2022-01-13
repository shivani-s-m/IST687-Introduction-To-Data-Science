################################################
#
# IST687, Homework Week 7
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 7
# Date due: 10/11/2020
#
# Set working directory 
library(tidyverse)
setwd("C:/Users/shiva/Documents/R/Code/Week7") # Change to the folder containing your homework data files

# Step	1: Load and repair the median income	data

#A. Storing the MedianZip csv file in a data frame myData
myData<-read_csv("MedianZip.csv")

#B. Removing the missing values in the mean column
# We can assign the values in the missing mean data from the median columns of the same rows. 
# Median is the middle point of the data, and assuming the data is not skewed, the mean and median are close
# to each other. Thus, median values can reasonably be assumed to be the mean value.
myData[is.na(myData$Mean),3] <- myData[is.na(myData$Mean), 2]

#C. Examine	the	data	with	View(	) 
# The data frame has 4 columns
# zip- It is the zip code of the row. Since, it is of numeric data type, the 0 preceding the zip code to make it 5 digit code is missing.
# Median - Median income of the households
# Mean - Mean income of the households
# Pop - Total population in the zip
View(myData)

#Step	2:	Merge the median	income data with	the	detailed	zipcode	data

#D. Read the csv file containing coordinates in a data frame
zipcodes <- read_csv("us_zip_code_latitude_and_longitude.csv")
View(zipcodes)

#E. Merge the two dataframes into a new frame dfNew by the zip attribute
dfNew <- merge(myData, zipcodes, all.x = TRUE, by.x ="zip", by.y="zip" )

#F.Examine dfNew
#It contains 1 common column from both the dataframes called the zip column
# It has the median, mean and pop columns from the myData df which is the median income, mean income and 
# the number of households
# It also has city, state and latitude, longitude, timezone of the area in zips
# the daylight savings time column indicates whether daylight savings occurs in the area
str(dfNew)

#Step 3: Merge	the	new	dataset	with	stateName.csv data

# Read the csv file containing the state names
stateNames <- read_csv("stateNames.csv")
#Merging stateNames and dfNew into a new data frame dfMerged by the state attribute
dfNew <- merge(dfNew, stateNames, all.x = TRUE, by.x = "state", by.y="state")
View(dfNew)

# Step 4: Visualize	the	data

#F. Plot points for each zip code
library(ggplot2)
library(ggmap)
us <- map_data("state")
dummyDF <- data.frame(state.name,  stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map <- ggplot(dummyDF, aes(map_id= state))
map <- map + geom_map(map=us, fill="white", color= "black")
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("USA Map")

#Adding the points on the map
#G. The map is very crowded and does not convey any information about the mean of the population. The area that is covered by a point
# is very small as compared to the whole map and there are many points indicated on the map
map <- map + geom_point(data=dfNew, aes(x = Longitude, y= Latitude, color = Mean))
map
#Step	5:	Use	aggregate()	to	Make	a	Data	Frame	of	State-by-State	Income
#H. Adding comments to the next block of code
# Adds a new column of the total income of the region by multiplying it with the mean with the population
dfNew$Total <- dfNew$Mean*dfNew$Pop
# Groups the income data by state and gives the total population and total income aggregated by the state
dfSimple = aggregate(dfNew[c("Pop", "Total")],
                     by = list(dfNew$name),
                     FUN = sum)
#Gives the state name as a new column instead of just the group name
dfSimple$name <- dfSimple$Group.1
dfSimple$Group.1 <- NULL

# I. Creating a new column with average income
dfSimple$Avg<- dfSimple$Total/dfSimple$Pop

dfSimple$state <- tolower(dfSimple$name)

#Step 6: Use	ggplot	and	ggmap	to	shade	a	map	of	the	U.S.	with	average	income	
# J. Using the US map with the state borders and adding parameters to it
us1 <- map_data("state")
dummyDF1 <- data.frame(state.name,  stringsAsFactors=FALSE)
dummyDF1$state <- tolower(dummyDF1$state.name)
map1 <- ggplot(dummyDF1, aes(map_id= state))
map1 <- map1 + geom_map(map=us, fill="white", color= "black")
map1 <- map1 + expand_limits(x=us$long, y=us$lat)
map1 <- map1 + coord_map() + ggtitle("USA Map")
map1

#Merging the state coordinates with the state avg income

trial <- merge(us1, dfSimple, all.x = TRUE, by.x = "region", by.y="state")

#Plotting the map with state wise average income
mapAvg <- ggplot(trial, aes(long,lat))+ geom_polygon(aes(fill=Avg, group= region)) 
mapAvg <- mapAvg + coord_map() + ggtitle("Avg income by state")
mapAvg
