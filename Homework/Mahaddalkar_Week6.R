################################################
#
# IST687, Homework 6
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 6
# Date due: 10/4/2020
#
# Set working directory 
setwd("~/Desktop/IST687/Homework") # Change to the folder containing your homework data files

# Using the packages necessary for homework
library(ggplot2)

# Step	1:	Make	a	copy	of the	data

#A. Copy the inbuilt airquality data frame into a new data frame air
air <- airquality

#B. What is the interval between the data?
# The interval is of one day. Every day the measurements are recorded for 5 months

#Step	2:	Clean-up	the NAs with	Missing	Data	Mitigation

#C. Write	a	comment	describing	how	that	statement	works
# The is.na() function gives a true or false value for every row if NA is present. 
#The statement below shows the entries in the Ozone column where NA values are present in the Ozone column
air$Ozone[is.na(air$Ozone)]

#D. Write	three	more	statements	to	report	missing	data	for	the	other	variables
# Finding all missing values of Solar.R
air$Solar.R[is.na(air$Solar.R)]
# Finding all missing values of Wind
air$Wind[is.na(air$Wind)]
#Finding all missing values of Temp
air$Temp[is.na(air$Temp)]

#E. Install	the	imputeTS package	and	use	na_interpolation(	)	on your	four	variables
install.packages("imputeTS")
library(imputeTS)
??na_interpolation
#Assigns the interpolated values to the missing values
air$Ozone<- na_interpolation(air$Ozone)
air$Solar.R <- na_interpolation(air$Solar.R)
air$Wind <- na_interpolation(air$Wind)
air$Temp <- na_interpolation(air$Temp)

#F. Running the code above to ensure no more missing values exist
air$Ozone[is.na(air$Ozone)]
air$Solar.R[is.na(air$Solar.R)]
air$Wind[is.na(air$Wind)]
air$Temp[is.na(air$Temp)]

#Step	3:	Use	ggplot	to	explore the	distribution of each	variable
#G. Create	a	histogram	for	Ozone
ggplot( air, aes(x= Ozone)) + geom_histogram(bins=15,color='white', fill = 'black')+ggtitle("Frequency of Ozone measurement")

#H. Create	histograms	of	each	of	the	other	three	variables with	ggplot(	)
#The parameter changed is the number of bins parameter to get a better idea for different columns
ggplot( air, aes(x= Solar.R)) + geom_histogram(bins=12,color='white', fill = 'black')+ggtitle("Frequency of Solar measurement")
ggplot( air, aes(x= Wind)) + geom_histogram(bins=10,color='white', fill = 'black')+ggtitle("Frequency of Wind measurement")
ggplot( air, aes(x= Temp)) + geom_histogram(bins=10,color='white', fill = 'black')+ggtitle("Frequency of Temp measurement")

#Step	4:	Explore	how	the	data	changes	over	time
#I. Run	the	following	line	of	code	and	write	a	comment	that	describes	what	it	does
#This adds a column Date which adds the default year ie 1973, the month from the month column and the day 
# from the day column as a date data type
air$Date	<- as.Date(paste("1973",	airquality$Month,	airquality$Day,	sep="-"))

#J.	Create	a	line	chart,	with	Date	on	the	X-axis	and	Ozone	on	the	Y-axis
ggplot( air, aes(x= Date, y= Ozone)) + geom_line(color= 'red') + ggtitle("Mean Ozone levels")

#K. Create	time	series	graphs	of	each	of	the	other	three	variables
ggplot( air, aes(x= Date, y= Solar.R)) + geom_line(color= 'blue') + ggtitle("Solar Radiation")
ggplot( air, aes(x= Date, y= Wind)) + geom_line(color= 'green') + ggtitle("Average Wind Speeds")
ggplot( air, aes(x= Date, y= Temp)) + geom_line(color= 'yellow') + ggtitle("Maximum Daily Temperature")

#L. Generate	a	bar	chart	of	average	temperate	per	month
agg	=	aggregate(air,
                by	=	list(air$Month),
                FUN	=	mean)	
ggplot(agg, aes(x=Month, y = Temp))+ geom_bar(stat='identity') +ggtitle("Monthly Average Temperate ")

#M. 	create	a	scatter	plot,	showing	Wind	on	the	x	axis,	Temp	on	the	y	axis, and	
# having	the	color	and	size	of	the	point	represent	Ozone.
ggplot(air, aes(x=Wind, y=Temp, color =Ozone)) + geom_point()

#N. Interpret	these	visualizations	
#It shows that with decrease in temperature the ozone levels are down, they seem to have a positive correlation.
#The ozone levels seem to have no relation with the avg wind speed
