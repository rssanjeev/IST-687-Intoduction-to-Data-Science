#install.packages("ggplot2")
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
#install.packages("methods")
#install.packages("dplyr")
library(dplyr)
#install.packages("jtools")
library(jtools)
#install.packages('pscl')
library(pscl)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("effects")
library(effects)
#install.packages("lattice")
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)
#install.packages("maps")
library(maps)
#install.packages("rgeos")
library(rgeos)
#install.packages("maptools")
library(maptools)
#install.packages("ggmap")
library(ggmap)
#install.packages("geosphere")
library(geosphere)
#install.packages("plyr")
library(plyr)

#Setting working Directory
setwd("D:/ADS/IST 687 Introduction to Data Science/Project")
#unclean<-read.csv(file.choose())
unclean_data<-data.frame(read.csv("satisfactionSurvey.csv"), stringsAsFactors=FALSE)
#test<-unclean_data
unclean_data$Satisfaction<-as.numeric(as.character(unclean_data$Satisfaction))
#summary(test$Satisfaction)
##str(unclean_data)
#typeof(unclean_data$Satisfaction)
#unique(unclean_data$Satisfaction)

#Checking the values of the Dependent Variable
#summary(unclean_data$Satisfaction)
#typeof(unclean_data$Satisfaction)

#look.for <- ("4.00.2.00")
#unclean_data[unclean_data$Satisfaction %in% look.for, ]
#look.for <- ("4.00.5")
#unclean_data[unclean_data$Satisfaction %in% look.for, ]


#unclean_data$Satisfaction[38898]
#unclean_data$Satisfaction[38899]
#unclean_data$Satisfaction[38900]

#Removing the three entries (Run the below command thrice)
#unclean_data<-unclean_data[-38898,]

#unique(unclean_data$Satisfaction)
#unclean$Flight.date


#mdy(unclean$Flight.date)

#Number of missing values
sum(is.na(unclean_data$Departure.Delay.in.Minutes))
sum(is.na(unclean_data$Arrival.Delay.in.Minutes))
sum(is.na(unclean_data$Flight.time.in.minutes))
sum(is.na(unclean_data$Satisfaction))


#Summary of the three variables with missing variables
summary(unclean_data$Departure.Delay.in.Minutes)
summary(unclean_data$Arrival.Delay.in.Minutes)
summary(unclean_data$Flight.time.in.minutes)
summary(unclean_data$Satisfaction)

#Replacing the NA's
unclean_data$Departure.Delay.in.Minutes[is.na(unclean_data$Departure.Delay.in.Minutes)] <- median(unclean_data$Departure.Delay.in.Minutes, na.rm=TRUE)
unclean_data$Arrival.Delay.in.Minutes[is.na(unclean_data$Arrival.Delay.in.Minutes)] <- mean(unclean_data$Arrival.Delay.in.Minutes, na.rm=TRUE)
unclean_data$Flight.time.in.minutes[is.na(unclean_data$Flight.time.in.minutes)] <- median(unclean_data$Flight.time.in.minutes, na.rm=TRUE)
unclean_data$Satisfaction[is.na(unclean_data$Satisfaction)] <- mean(unclean_data$Satisfaction, na.rm=TRUE)

#Summary of the three variables after removing the missing variables
summary(unclean_data$Departure.Delay.in.Minutes)
summary(unclean_data$Arrival.Delay.in.Minutes)
summary(unclean_data$Flight.time.in.minutes)
summary(unclean_data$Satisfaction)

#Dealing with the Flight Data variable
unclean_data$Flight.date<-mdy(unclean_data$Flight.date)
head(unclean_data$Flight.date)

data<-unclean_data

#Plot of Age vs Avg. Satisfaction
#Mean satisfaction for Age
satmean <- data %>%
  group_by(Age) %>%
  summarize(m1 = mean(Satisfaction))

satmean<-as.data.frame(satmean)

#Plotting average Satisfaction Score against Age
agesat<-ggplot(satmean,aes(Age,m1)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 75, hjust = 1))+labs(y="Average Satisfaction Score")
agesat


satmean <- data %>%
  group_by(Satisfaction) %>%
  summarize(m1 = mean(Age))

agemean<-as.data.frame(satmean)
plot3.1<-ggplot(agemean,aes(Satisfaction,m1)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 75, hjust = 1))+labs(y="Average Age Score")
plot3.1


#Plotting Satisfaction against the Flight Date
flightdate <- data %>%
  group_by(Flight.date) %>%
  summarize(m2 = mean(Satisfaction))

flightdate<-as.data.frame(flightdate)

#head(flightdate)
plot15<-ggplot(flightdate, aes(Flight.date,m2, group=1))+geom_line()+labs(x="From 1st Jan to 31st March 2014", y="Mean Satisfaction")
plot15

#Plotting Airline Status against Satisfaction
plot2<-ggplot(data, aes(Airline.Status, Satisfaction))+geom_boxplot()+labs(x="Airline Status", y="Satisfaction")
plot2
#Plotting Gender against Satisfaction
ggplot(data, aes(Gender, Satisfaction, fill=Gender))+geom_boxplot()+labs(x="Gender", y="Satisfaction")

ggplot(data, aes(Gender, Satisfaction))+geom_count(aes(color=..n..))+labs(x="Gender", y="Satisfaction")

#Plotting Price Sensitivity against Satisfaction
ggplot(unclean_data, aes(Price.Sensitivity, Satisfaction))+geom_count()+labs(x="Price Sensitivity", y="Satisfaction")

#Year of First Flight
firstflight <- unclean_data %>%
  group_by(Year.of.First.Flight) %>%
  summarize(m3 = mean(Satisfaction))

firstflight<-as.data.frame(firstflight)

plot6<-ggplot(firstflight, aes(Year.of.First.Flight,m3))+geom_line()+scale_x_continuous(breaks = c(2001,2002,2004,2006,2008,2010,2012,2014))+labs(x="Year of First Flight", y="Mean Satisfaction")
plot6


#Calculating the Effective Delay
unclean_data$EffectiveDelay<-abs(unclean_data$Departure.Delay.in.Minutes-unclean_data$Arrival.Delay.in.Minutes)
unclean_data$Departure.Delay.in.Minutes<-NULL
unclean_data$Arrival.Delay.in.Minutes<-NULL
data<-unclean_data
lmodel<-lm(formula = Satisfaction   ~ Airline.Status + Gender + Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a. + 
             Type.of.Travel + Type.of.Travel + Shopping.Amount.at.Airport + Class, data = unclean_data )
summary(lmodel)

#Creating a Categoricall Variable from the dependent variable 
data$Sat<-replicate(length(data$Satisfaction),0)
data$Sat[data$Satisfaction>3]<-1
data$Sat<-as.factor(data$Sat)
str(data)

#Removing the unwanted variables
data<-data[,-c(1)]
data$Airline.Code<-NULL
data$Airline.Name<-NULL
data$Orgin.City<-NULL
data$Origin.State<-NULL
data$Destination.City<-NULL
data$Destination.State<-NULL

#Initial model
model<-glm(Sat~., family=binomial(link="logit"),data=data)
summary(model)

#Removing insignificant Variables
data$Day.of.Month<-NULL
data$Flight.date<-NULL
data$Flight.time.in.minutes<-NULL
data$Flight.Distance<-NULL
data$EffectiveDelay<-NULL
data$Eating.and.Drinking.at.Airport<-NULL
data$X..of.Flight.with.other.Airlines<-NULL

#Train and Test data split
rand<-sample(1:dim(data)[1])
cutpoint2_3<-floor(2*dim(data)[1]/3)
cutpoint2_3

traindata<-data[rand[1:cutpoint2_3],]
testdata<-data[rand[(cutpoint2_3+1):dim(data)[1]],]

#Second model
model2<-glm(Sat~., family=binomial(link="logit"),data=traindata)
summary(model2)

#K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(Sat ~.,  data=traindata, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

#Predicting the outcome using the model and the test data
pred = predict(mod_fit, newdata=testdata)

#Confusion Matrix
confusionMatrix(data=pred, testdata$Sat)

#Model Summary
summary(mod_fit)

#Variable Importance
varImp(mod_fit)

#Plotting the model and its effects
plot_summs(model2)
plot(allEffects(model2))

str(data)
rand<-sample(1:dim(data)[1])
cutpoint2_3<-floor(2*dim(data)[1]/3)
cutpoint2_3

traindata<-data[rand[1:cutpoint2_3],]
testdata<-data[rand[(cutpoint2_3+1):dim(data)[1]],]

#K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(Sat ~.,  data=traindata, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=testdata)
confusionMatrix(data=pred, testdata$Sat)

summary(mod_fit)
varImp(mod_fit)

pred = predict(mod_fit, newdata=testdata)
confusionMatrix(data=pred, testdata$Sat)

#Creating a copy of the dataset for the smaller model
data2<-data

#Removing variable with low significance
data2$Year.of.First.Flight<-NULL
data2$No..of.other.Loyalty.Cards<-NULL
data2$Class<-NULL
data2$Price.Sensitivity<-NULL
data2$Scheduled.Departure.Hour<-NULL
data2$Age<-NULL
data2$Flight.cancelled<-NULL
data2$Gender<-NULL
data2$Shopping.Amount.at.Airport<-NULL

#Data split
rand<-sample(1:dim(data2)[1])
cutpoint2_3<-floor(2*dim(data2)[1]/3)
cutpoint2_3

traindata<-data2[rand[1:cutpoint2_3],]
testdata<-data2[rand[(cutpoint2_3+1):dim(data2)[1]],]

#Model Bulding
tiny<-glm(Sat~., family=binomial(link="logit"),data=traindata)

#K-Fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(Sat ~.,  data=traindata, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

#Variable importance
varImp(mod_fit)

#Predicting the result
pred = predict(mod_fit, newdata=testdata)

#Confusion Matrix
confusionMatrix(data=pred, testdata$Sat)

#PLotting the result and its effects
plot_summs(tiny)
plot(allEffects(tiny))

summary(mod_fit)
varImp(mod_fit)
tiny<-glm(Sat~., family=binomial(link="logit"),data=traindata)
summary(tiny)
pR2(tiny)
plot(allEffects(tiny))

"Unlike linear regression with ordinary least squares estimation, there is no R2 statistic which explains 
the proportion of variance in the dependent variable that is explained by the predictors. 
However, there are a number of pseudo R2 metrics that could be of value. 
Most notable is McFadden's R2, which is defined as 1???[ln(LM)/ln(L0)] where ln(LM) is the log likelihood value 
for the fitted model and ln(L0) is the log likelihood for the null model with only an intercept as a predictor. 
The measure ranges from 0 to just under 1, with values closer to zero indicating that the model has no predictive power."
pR2(mod_fit)
"> pR2(model2)
          llh       llhNull            G2      McFadden          r2ML          r2CU 
-6.075748e+04 -9.000319e+04  5.849141e+04  3.249408e-01  3.625749e-01  4.835052e-01"

rrplot(allEffects(mod_fit))
effects<-allEffects(model2)
effects$Gender$model.matrix

#Curve for Logistic Regression
summ(model2)
summ(model2, confint = TRUE, digits = 6)
plot_summs(model2, scale=TRUE)