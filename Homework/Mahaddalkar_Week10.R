################################################
#
# IST687, Homework Week 10
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 10
# Date due: 11/1/2020
#

# Part 1: Load	and	condition	the	data
# A. Using library() on the packages required
library(ggplot2)
data.frame(diamonds)
install.packages("kernlabs")
library(kernlab)

#B. Creating dataframe containing only Premium and Ideal cuts.
diamondsDF <- data.frame(diamonds)
#Assigning the values to df1
df1 <- diamondsDF[diamondsDF$cut == 'Premium' | diamondsDF$cut == 'Ideal',]
str(df1)

#C. Update	the	dataframe's	cut	attribute	to	be	a	factor	with	just	two	choices	
df1$cut <- factor(df1$cut)
levels(df1$cut)

#D. Changing the ordered factors color and clarity to numeric values
df1$color <- as.numeric(df1$color)
df1$clarity <- as.numeric(df1$clarity)
str(df1)

#E. Explaining each attribute in dataframe
#carat: weight of diamond. It is numeric
#color: diamond color which had ordered factors now converted to numeric data
#clarity: measurement of how clear the diamond is which had ordered factors now converted to numeric data
#depth: total depth percentage 
#table: width of top of diamond
#price: price in US dollars
#x:length in mm
#y:width in mm
#z:depth in mm

#Part 2: 	Create	training and	test	data	sets
#F. Creating training sets with 60% data from the original dataframe and testing data sets with 40% data from the original dataframe 
# The cut attribute is used as balance
library(caret)
#Creating training dataset
trainIndices <- createDataPartition(y=df1$cut,p=0.60,list=FALSE)
trainSet <- df1[trainIndices,]

#Creating testing data set
testSet <- df1[-trainIndices,]

#G. Using the dim() function to check if the correct number of classes
#This shows that both training and testing sets have the correct number of classes
if (floor(dim(df1)[1]*0.6)+1 == dim(trainSet)[1]) {print("training has 60% of indices from original data set")} else {print("training does not have 60% from original data")}

if (floor(dim(df1)[1]*0.4) == dim(testSet)[1]) {print("testing has 40% of indices from original data set")} else {print("testing has 40% from original data")}

#Part 3:	Build	a	Model	using	ksvm(	)
#H. Build	a	support	vector	model	using	the	ksvm(	) function using	all	of	the	variables	to	predict	cut
svmOutput <- ksvm(cut ~ .,data =trainSet,kernel= "rbfdot",	kpar	=	"automatic",	C	=	5,	cross	=	3,	prob.model	=	TRUE)

#I. Summarizing parameters
# cut represents the variable to be predicted
# ~. represents that all the other variables need to be taken for prediction
# data parameter is the data set to be used
# kernel ='rbfdot' is using the radial basis function which takes the dot product of two vectors to come up with a scalar value
# kpar argument refers to the parameters that control operation of radial basis function. By setting to automatic the algorithm sets the parameters
# C argument is the cost of constraints. A large margin can be obtained by using small C or the other way round. With a low value of C, we get a generalized model with greater error in training data.
# cross value ensures that the model does not overfit the training data.

#J. Output of ksvm 
#Cross validation error is 0.08
svmOutput

#Part	4:	Predict	Values	in	the	Test	Data	and	Create	a	Confusion	Matrix

#K. Use	the	predict(	)	function	to	validate	the	model	against	test	data.
svmPred <- predict(svmOutput,testSet, type='response')

#L. Review	the	contents	of	svmPred	using	str(	) and	head(	)\
#The svmPred variable has factors with 2 levels 'Premium' and 'Ideal'
str(svmPred)
head(svmPred)

#M. Create	a	confusion	matrix	(a	2	x	2	table)	that	compares	svmPred	to	the	contents	of testData$cut.
compTable <- data.frame(testSet[,2], svmPred)
table(compTable)

#N. Calculate	an	error	rate	based	on	what	you	see	in	the	confusion	matrix.
#We see in the diagonal there are 4852 + 8110 = 12962 correct predictions out of 14136 predictions
#The accuracy is 0.9169
sum(diag(table(data.frame(testSet[,2], svmPred))))/sum(table(data.frame(testSet[,2], svmPred)))

#O. Comparing	calculations	with	the	confusionMatrix()	function	from	the	caret	package
#Shows that accuracy is 0.9169
confusionMatrix(svmPred, testSet$cut)

#P. Why	it	is	valuable	to	have	a	"test"	dataset	that	is separate	from	a	"training" dataset?
#If the data we trained on is used to test, the algorithm will have learned from it directly and will give the same answer it has seen before.
#It will not necessarily be correct when it predicts for data it has not seen before. The accuracy for the same will not be accurately known.
#Therefore, it is important that data does not leak from the train data set into the test data set

##	Use	lm(	)	to	reproduce	what	you	did	above	with	ksvm(	)
#When The following function is used, it gives a warning that it is not meaningful for ordered factors
modelLM <- lm(formula = cut ~ ., data = trainSet)
summary(modelLM)
