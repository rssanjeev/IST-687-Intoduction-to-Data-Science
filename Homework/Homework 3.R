#IST687 - HW 3 - Descriptive Stats & Functions
#Sanjeev Ramasamy Seenivasagamani
#Homework 3
#Due Date: Sept 20, 2018
#Submitted: Sep 20, 2018


"Loading the Csv File"

data=read.csv("D:/ADS/IST 687 Introduction to Data Science/Homework/HW3.csv",stringsAsFactors = FALSE)
View(data)

"Cleaning the Dataframe"
View(data)
head(data)
tail(data)
data[1,]

"Deleting the last row!! (Puerto Rico)"
beta<-head(data,-1)
View(beta)

"Deleting the First Row!! (United States)"
theta<-beta[-1,]
View(theta)

"Now we have just the states and the District of Columbia"
"Summary of United States"
summary(theta)

"Removing unneeded columns"
theta<-subset(theta,select=-c(1:3))
theta
colnames(theta)[3]<-"Population"
colnames(theta)[4]<-"popover18"
colnames(theta)[5]<-"poercentOver18"

theta
"Creating a function"

cleaning_the_dataframe <-function(){
  data=read.csv("D:/ADS/IST 687 Introduction to Data Science/Homework/HW3.csv",stringsAsFactors = FALSE)
  
  "Cleaning the Dataframe"
  head(data)
  tail(data)
  "Removing unneeded columns"
  data<-subset(data,select=-c(1:3))
  
  "Deleting the last row!!"
  beta<-head(data,-1)
  "Deleting the First Row!!"
  theta<-beta[-1,]

  "Summary of United States"
  summary(theta)
  
  colnames(theta)[3]<-"Population"
  colnames(theta)[4]<-"popover18"
  colnames(theta)[5]<-"poercentOver18"
  
}

"Final Data Frame"
View(theta)

"Calculating teh average population of the States"
avgpop=mean(theta$Population)
avgpop

"Calculating the State with the Highest Population"
theta[which.max(theta$Population),] #California


"Histogram of State Population"

histogram(theta$Population)

"The data is right skewed with some outliers"

"Sorting the dataframe by population"
gamma<-theta[order(theta$Population),]
View(gamma)

"10 States with lowest population"
head(gamma,10)

"Barplot"
barplot(gamma$Population)

'After the sorting we can see that the bar plot depicting the population is gradually incrasing
from the left to right i.e. left skewed.'
