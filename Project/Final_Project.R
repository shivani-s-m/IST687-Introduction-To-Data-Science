## install and library required packages

install.packages("jsonlite")
install.packages("ggplot2")
install.packages("splitstackshape")
install.packages("arules")
install.packages("ggmap")
install.packages("maps")
install.packages("mapproj")
install.packages("kernlab")
install.packages("caret")
library(jsonlite)
library(ggplot2)
library(splitstackshape)
library(arules)
library(ggmap)
library(maps)
library(mapproj)
library(kernlab)
library(caret)

## read in and clean data

mydata.list <- jsonlite::fromJSON("completeSurvey.json")

survey <- data.frame(mydata.list)
str(survey)
dim(survey)
survey$Flight.date <- as.character(survey$Flight.date)
split <- cSplit(survey, 'Flight.date', sep = "/", type.convert=FALSE)
survey$Month <- split$Flight.date_1
survey$status <- ifelse(survey$Likelihood.to.recommend >8, "Promoter",
                        ifelse(survey$Likelihood.to.recommend<7, "Detractor",
                               "Neutral"))

survey$Airline.Status <- as.factor(survey$Airline.Status)
survey$Gender <- as.factor(survey$Gender)
survey$Year.of.First.Flight <- as.factor(survey$Year.of.First.Flight)
survey$Type.of.Travel <- as.factor(survey$Type.of.Travel)
survey$Class <- as.factor(survey$Class)
survey$Partner.Name <- as.factor(survey$Partner.Name)
survey$Month <- as.factor(survey$Month)
survey$status <- as.factor(survey$status)

survey2 <- survey[,-32]
survey2 <-survey2[,-16]

## boxplots

g <- ggplot(survey2, aes(y=Likelihood.to.recommend, group=Flight.cancelled)) 
g <- g + geom_boxplot(aes(colour=factor(Flight.cancelled)))
g <- g + ylab("Likelihood to Recommend")
g

g2 <- ggplot(survey2, aes(y=Likelihood.to.recommend, group=Type.of.Travel)) 
g2 <- g2 + geom_boxplot(aes(colour=factor(Type.of.Travel)))
g2 <- g2 + ylab("Likelihood to Recommend")
g2

g3 <- ggplot(survey, aes(y=Likelihood.to.recommend, group=Airline.Status)) 
g3 <- g3 + geom_boxplot(aes(colour=factor(Airline.Status)))
g3 <- g3 + ylab("Likelihood to Recommend")
g3

summary(survey2$Likelihood.to.recommend[survey2$Partner.Code == "EV"])

g4 <- ggplot(survey2, aes(y=Likelihood.to.recommend, group=Partner.Code)) 
g4 <- g4 + geom_boxplot(aes(colour=factor(Partner.Code)))
g4 <- g4 + ylab("Likelihood to Recommend")
g4

pc_agg <- aggregate(not_cancelled$Likelihood.to.recommend, list(not_cancelled$Partner.Code), FUN=mean)
pc_agg

g5 <- ggplot(survey2, aes(y=Likelihood.to.recommend, group=Price.Sensitivity)) 
g5 <- g5 + geom_boxplot(aes(colour=factor(Price.Sensitivity)))
g5 <- g5 + ylab("Likelihood to Recommend")
g5

## more data cleaning

cancelled <- survey2[survey2$Flight.cancelled == 'Yes',]
not_cancelled <- survey2[survey2$Flight.cancelled == 'No', ]

not_cancelled <- not_cancelled[complete.cases(not_cancelled),]
# only 205 of 86463 rows weren't complete cases
str(not_cancelled)
cols <- c(3:14, 16,18,19,20, 21, 22, 24,25,26,31,32)
not_cancelled <- not_cancelled[,cols]

## linear model

cols2 <- c(1:13, 16:22)
lm_data <- not_cancelled[,cols2]
str(lm_data)
lm <- lm(formula=Likelihood.to.recommend ~ ., data=lm_data )
summary(lm)


## association rules

not_cancelledX <- not_cancelled
not_cancelledX$Age <- as.factor(not_cancelledX$Age)
not_cancelledX$Price.Sensitivity <- as.factor(not_cancelledX$Price.Sensitivity)
not_cancelledX$Flights.Per.Year <- as.factor(not_cancelledX$Flights.Per.Year)
not_cancelledX$Loyalty <- as.factor(not_cancelledX$Loyalty)
not_cancelledX$Total.Freq.Flyer.Accts <- as.factor(not_cancelledX$Total.Freq.Flyer.Accts)
not_cancelledX$Shopping.Amount.at.Airport <- as.factor(not_cancelledX$Shopping.Amount.at.Airport)
not_cancelledX$Eating.and.Drinking.at.Airport <- as.factor(not_cancelledX$Eating.and.Drinking.at.Airport)
not_cancelledX$Scheduled.Departure.Hour <- as.factor(not_cancelledX$Scheduled.Departure.Hour)
not_cancelledX$Departure.Delay.in.Minutes <- as.factor(not_cancelledX$Departure.Delay.in.Minutes)
not_cancelledX$Arrival.Delay.in.Minutes <- as.factor(not_cancelledX$Arrival.Delay.in.Minutes)
not_cancelledX$Flight.Distance <- as.factor(not_cancelledX$Flight.Distance)
not_cancelledX$status <- as.factor(not_cancelledX$status)
not_cancelledX$Partner.Code <- as.factor(not_cancelledX$Partner.Code)
not_cancelledX$Origin.State <- as.factor(not_cancelledX$Origin.State)
not_cancelledX$Destination.State <- as.factor(not_cancelledX$Destination.State)

cols3 <- c(1:18, 20, 22, 23)
not_cancelledX <- not_cancelledX[,cols3]
str(not_cancelledX)
not_cancelledX <- as(not_cancelledX,"transactions")

r1 <- apriori(not_cancelledX, parameter=list(supp=.1, conf=.50),
              control=list(verbose=F),
              appearance=list(default="lhs",rhs=("status=Detractor")))
r1
inspect(r1)

r2 <- apriori(not_cancelledX, parameter=list(supp=.15, conf=.50),
              control=list(verbose=F),
              appearance=list(default="lhs",rhs=("status=Promoter")))
r2
inspect(r2)

## table analysis

bus_travel <- not_cancelled[not_cancelled$Type.of.Travel == "Business travel",]
pers_travel <- not_cancelled[not_cancelled$Type.of.Travel == "Personal Travel",]
mile_tickets <- not_cancelled[not_cancelled$Type.of.Travel == "Mileage tickets",]

prop.table(table(pers_travel$status))
prop.table(table(bus_travel$status))
prop.table(table(mile_tickets$status))
prop.table(table(not_cancelled$status))

detractors <- 86258 * .2939206
pers_detractors <- 26283 * .63048358
perc_det_pers <- pers_detractors / detractors

## US Map

us <- map_data("state")

map_agg <- aggregate(survey$Likelihood.to.recommend, list(survey$Origin.State), FUN=mean)
map_agg$state <- tolower(map_agg$Group.1)
map_agg$avg_LTR <- map_agg$x
mean(map_agg[1:48,]$avg_LTR)
map_agg$avg_LTR[map_agg$state == "wyoming"] <- 7.355996
map_agg_cols <- c(3:4)
map_agg <- map_agg[,map_agg_cols]
str(map_agg)

map.LTR <- ggplot(map_agg, aes(map_id=state))
map.LTR <- map.LTR + geom_map(map =us, aes(fill=avg_LTR))
map.LTR <- map.LTR + expand_limits(x=us$long, y=us$lat)
map.LTR <- map.LTR + coord_map() + ggtitle("State Likelihood to Recommend")
map.LTR



