#IST687 - HW 4 - sampling & Decisions
#Sanjeev Ramasamy Seenivasagamani
#Homework 4
#Due Date: Sept 26, 2018
#Submitted: Sep 26, 2018

#Distribution of a Vector

printVecinfo<- function(vec){
  cat("Mean: ",mean(vec), sep="\n")
  cat("\nMedian: ",median(vec), sep="\n")
  cat("\nMinimum: ",min(vec), sep="\n")
  cat("\nMax: ",max(vec), sep="\n")
  cat("\nStandard Deviation :",sd(vec), sep="\n")
  cat("\n1st Quant: ",quantile(vec,0.05), sep="\n")
  cat("\n2nd Quant: ",quantile(vec,0.95), sep="\n")
}
tile<-c(1:10)

#Printing the distribution of the vector
printVecinfo(tile)

#Creating a function to get the data and clean it
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

cleaning_the_dataframe()

View(theta)

"Step 6: Sampling the data"
samp<-sample(theta$Population,20,replace = TRUE)
samp
printVecinfo(samp)
hist(samp)

"As we repeat the above step 5 times, we can clearly notice that each time the data distribution
in the histogram vary. This is because the sample that is generated every time is different from
one another."
samp<-replicate(2000,sample(theta$Population,20,replace = TRUE))
printVecinfo(samp)
hist(samp)

"This time, when we generate the histogram we can notice that the data is more spread 
compared to the previous sample. The reason is that the earlier sample was of size 20,
whereas the final vector after the replication is 40000. This is the reason for the difference
and we can also see the difference in the details we received from the printVecInfo() func."