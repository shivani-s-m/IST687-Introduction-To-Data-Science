################################################
# IST687, Week 9 HW
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 9
# Date due: 10/27/2020
#

#Setting working directory to the location where the data file is present
setwd("C:/Users/shiva/Documents/R/Code")

#Loading the data and storing it into a variable
load("titanic.raw")
badboat <- titanic.raw

# Part 1: Exploring the dataframe

# A. Using View command on the df shows that the dataframe has 2201 rows with 4 columns
View(badboat)

# B. The people in each category of Survived column
# It shows that 711 people survived as opposed to 1490 who did not
table(badboat$Survived)

# C. Showing the percentage of the survival rate using the prop.table function
survivalTable <- table(badboat$Survived)
#This indicates that 67.6965 % people did not survive as opposed to 32.3035 % people who did
prop.table(survivalTable)* 100

# D. Showing the percentage of Class, Sex and Age on the boat

# Percentage table for class
prop.table(table(badboat$Class))*100

# Percentage table for Sex
prop.table(table(badboat$Sex))*100

# Percentage table for Age
prop.table(table(badboat$Age))*100

# E. Showing a contingency table for Age and Sex 
# The table shows that percent of female adults was 19.309, female children was 2.044, male adults was 75.738 and male children was 2.907
prop.table(table(badboat$Age, badboat$Sex))*100

# Part	2: Coerce	the	data	frame	into	transactions

# F. Installing and using library arules and arulesViz
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# G. Coerce the badboat dataframe into a sparse transaction matrix

badboatX <- as(badboat,"transactions")

# H. Exploring the badboatX data set
# Using inspect which stores all the information in a class as one column and transaction id in another column
inspect(badboatX)
#This gives the frequency of all the different the different occurrences
itemFrequency(badboatX)
#Shows the frequency plot for different classification
itemFrequencyPlot(badboatX)

# I. Difference between badboat and badboatX
# Both badboat and badboatX store different instances in this case, information about the Class, Sex, Age and Survival of the people on the Titanic.
#  The difference between them is that badboat is a dataframe whereas, badboatX is a sparse transaction matrix.
# The dataframes stores the information in rows and columns whereas the badboatX stores the information in a class. 
# The functions used on a dataframe cannot be applied to the transaction matrix. We have to use the arules package for the same.

# Part	3:	Use	arules	to	discover	patterns

# J. generate	a	set	of	rules	with	support	over	0.005	and	confidence	over 0.5
rules1 <- apriori(badboatX, parameter=list(supp=0.005, conf=0.50),
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=('Survived=Yes')))

# K. Using inspect to review the set of rules
#This spells out the 14 rules stored in the rules1 rule set
inspect(rules1)

# L. If	you	wanted	to	be	certain to	have	survived	the	Titanic	disaster,	what	kind	of	person	would	you	want	to	have	been?	
# With a confidence of 1, a female child traveling 2nd class would be the best choice as a passenger of the titanic.
# What struck me as odd was that a female child traveling 1st class would not show up as a rule. On running the code below, it
# can be seen that on the titanic there was only one such person who survived. As the number was low, it will not show as a rule.
# But it can be seen that an adult female traveling 1st class also had confidence of 0.97.
badboat[badboat$Age=='Child' & badboat$Sex=='Female' & badboat$Class=='1st',]
