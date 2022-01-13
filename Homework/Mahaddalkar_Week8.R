################################################
# IST687, HW- Week8
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 8
# Date due: 10/18/2020
#
#A. Installing and using psych package, and using the sat.act dataset from the package
install.packages("psych")
library(psych)
df <- sat.act
summary(df)

#B. Creating scatterplots with the ACT scores on the Y-axis
library(ggplot2)
#Scatterplot of ACT on Y-axis vs gender on X-axis
myScatter1 <- ggplot(df, aes(x=gender, y=ACT)) + geom_point()
myScatter1

#Scatterplot of ACT on Y-axis vs age on X-axis
myScatter2 <- ggplot(df, aes(x=age, y=ACT)) + geom_point()
myScatter2

#Scatterplot of ACT on Y-axis vs education on X-axis
myScatter3 <- ggplot(df, aes(x=education, y=ACT)) + geom_point()
myScatter3

#C. create	one	regression	model	predicting	ACT	scores	from	the	three	predictors
model1 <- lm(formula = ACT ~ gender+age+education, data = df)

#D. Summary of model1
# The adjusted R-squared value of the model suggests that the model is not a good predictor of the ACT score as the value is 0.02301
# The p-value(0.00025) of the model is <0.05 suggests that the R-squared value is statistically significant.
# The statistically significant predictor is education with a value of 0.00174 which is less than 0.05, the slope of the predictor 
# is 0.47890
summary(model1)

#E. Overall interpretation of the model
#The independent variables gender and age do not seem to be good predictors to predict the ACT score.
# The independent variable education is statistically significant and therefore can be used to predict the ACT score.

#F. Creating one row data frame to store the values of independent variables to predict the dependent variable. Predicting the ACT score.
predDF	<- data.frame(gender=2,	education=2,	age=20)
predict(model1, predDF, type = "response")

#G. Creating two new linear models for SATV and SATQ  as the outcome variables
# model for SATV as the outcome variable
model2 <- lm(formula = SATV ~ gender+age+education, data = df)
summary(model2)

#model for SATQ as the outcome variable
model3 <- lm(formula = SATQ ~ gender+age+education, data = df)
summary(model3)

#H. Reviewing the models
# Model 1: The p-value(0.02301) suggested that the R-squared was statistically significant, and the adjusted R-squared was 0.02301.
#          The only significant variable was education.

# Model 2: The p-value(0.08076) suggests that the model is statistically insignificant and the outcome of the model could be by chance.
#          Hence, we do not consider the model a good model.

#Model 3: The p-value(1.52e-05) suggests that the model is statistically significant, and the adjusted R-squared is 0.031373
#         All three variables i.e gender, age and education are statistically significant and contribute in predicting.