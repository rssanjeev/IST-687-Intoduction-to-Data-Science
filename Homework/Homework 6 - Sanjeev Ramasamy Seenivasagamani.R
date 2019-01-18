#IST687 - HW 6 - Data Viz
#Sanjeev Ramasamy Seenivasagamani
#Homework 6
#Due Date: Oct 10, 2018
#Submitted: Oct 10, 2018


#install.packages("tibble")
library(tibble)
#install.packages('dplyr')
library(dplyr)
#Reading the data
lean<-function(){
  data=read.csv("D:/ADS/IST 687 Introduction to Data Science/Homework/HW3.csv",stringsAsFactors = FALSE)
  arrests<-USArrests
  rownames(data)<-as.vector(data[,"statesName"])
  df<-merge(data,arrests,by="row.names")
  arrests<- arrests %>% rownames_to_column('statesName') 
  data<-data[,-c(1:4)]
  colnames(data)<-c('statesName','population','popover18','percentover18')
  #Deleting the unneeded rows!!
  data<-data[-c(1,10,53),]
  #View(arrests)
  nrow(data)
  nrow(df)
  common<-intersect(names(arrests$statesName),names(data$STATE))
  df<-merge(data,arrests,by.x='statesName',by.y = 'statesName')
  }

lean()

#Histogram using ggplot2
#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)
#Histogram for Population
ggplot(df, aes(df$population))+ geom_histogram(color="black", fill="white", bins = 500)+  scale_x_continuous("Population")+ scale_y_continuous("Count")

#Histogram for Murder Rate
ggplot(df, aes(df$Murder))+ geom_histogram(color="black", fill="red", bins = 50)+  scale_x_continuous("Murder Rate")+ scale_y_continuous("Count")

#Histogram for Assault
ggplot(df, aes(df$Assault))+ geom_histogram(color="black", fill="red", bins = 50)+  scale_x_continuous("Murder Rate")+ scale_y_continuous("Count")

#Histogram for UrbanPop
ggplot(df, aes(df$Urbanpop))+ geom_histogram(color="black", fill="red", bins = 50)+  scale_x_continuous("Murder Rate")+ scale_y_continuous("Count")

#Histogram for Rape
ggplot(df, aes(df$Murder))+ geom_histogram(color="black", fill="red", bins = 50)+  scale_x_continuous("Murder Rate")+ scale_y_continuous("Count")


#Box plot for population
ggplot(df, aes(x="", y=df$population, color=population))+geom_boxplot()+scale_y_continuous(name="Population", labels = comma)+
  stat_summary(fun.ymin=median, fun.ymax=median, fun.y=median, geom="crossbar")

#Box plot for Murder Rate
ggplot(df, aes(x="", y=df$Murder, color=Murder))+geom_boxplot()+scale_y_continuous(name="Murder", labels = comma)+
  stat_summary(fun.ymin=median, fun.ymax=median, fun.y=median, geom="crossbar")

#Bar Charts
#Most number of murder rates
plot<-ggplot(df, aes(x=statesName, y=Murder))+geom_bar(stat = "identity", fill='white',color='red')
plot

#Bar Chart with the number of murders per state
ggplot(df, aes(x=statesName, y=(Murder*population)/1000))+geom_bar(stat = "identity", fill='white',color='black')+
  ggtitle("Chart with the number of murders per state")+geom_text(aes(label=Murder),vjust=-1.75,hjust=0.75, color="red", size=3)+labs(x="States",y="Murder Rate")
 
#Bar Chart with the number of murders per state (with the x-axis labels rotated)
ggplot(df, aes(x=statesName, y=Murder), angle=90)+geom_bar(stat = "identity", fill='white',color='black')+
  geom_text(aes(label=Murder),vjust=0.4,hjust=-0.25, color="red", size=3, angle=90) +
  ggtitle("Chart with the Murder Rate per state")+
  theme(axis.text.x=element_text(angle=90))+
  labs(x="States",y="Murder Rate")

#Another way to do achieve this is to rotate the whole chart 90 degrees
ggplot(df, aes(x=statesName, y=Murder))+geom_bar(stat = "identity", fill='white',color='black')+
  ggtitle("Chart with the Murder Rate per state")+geom_text(aes(label=Murder),vjust=0.35,hjust=-1, color="red", size=3)+labs(x="States",y="Murder Rate")+coord_flip()

#Sorted x-axis
df$statesName<-reorder(df$statesName,df$Murder)
ggplot(df, aes(x=statesName, y=Murder), angle=90)+geom_bar(stat = "identity", fill='white',color='black')+
  geom_text(aes(label=Murder),vjust=0.4,hjust=-0.15, color="red", size=3, angle=90) +
  ggtitle("Chart with the number of murders per state")+theme(axis.text.x=element_text(angle=90))

#Sorted x-axis with Percentover18 as the color of the bar
df$statesName<-reorder(df$statesName,df$Murder)

ggplot(df,aes(x=statesName, y=Murder, fill=percentovet18))+
  geom_bar(stat = "identity")+geom_text(aes(label=Murder),vjust=0.4,hjust=-1, color="red", size=3, angle=90) +
  ggtitle("Chart with the number of murders per state")+
  theme(axis.text.x=element_text(angle=90))

#Scatter Plot
ggplot(df, aes(x=population, y=percentover18))+geom_point(aes(size=Murder, color=Murder))

