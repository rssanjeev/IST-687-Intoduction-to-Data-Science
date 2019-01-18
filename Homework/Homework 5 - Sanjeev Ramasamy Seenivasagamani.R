#IST687 - HW 5 - sampling & Decisions
#Sanjeev Ramasamy Seenivasagamani
#Homework 5
#Due Date: OCt 3, 2018
#Submitted: Oct 3, 2018

install.packages("RJSONIO")
library('RJSONIO')
install.packages('RCurl')
library('RCurl')

dataset<-getURL("http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD")
df<-fromJSON(dataset,simply=FALSE, nullValue=NA)
myList <- df[[2]]
numRows <- length(myList)
df<-data.frame(matrix(unlist(mylist),nrow=numRows,byrow=T),stringsAsFactors = FALSE)

#Deleting the First 8 Columns"
df <- df[,-c(1:8)]
#Changing the Column Names"
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE",
                    "DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT",
                    "DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME",
                    "VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1",
                    "COLLISION_WITH_2")
colnames(df)<-namesOfColumns

#Number of Accidents with Injury"
nrow(df[ which(df$INJURY=='YES'), ])

#Number of Accidents on a Sunday"
length(grep('SUNDAY',df$DAY_OF_WEEK))

#Number of Injuries in each day of the week"
aggregate(df$CASE_NUMBER, by=list(df$DAY_OF_WEEK), FUN=length)

install.packages('dplyr')
library('dplyr')

#Total Number of Accidents with injuries
count(filter(df, INJURY=='YES'))

#Number of Accidents on Sunday
nrow(filter(df,trimws(DAY_OF_WEEK) == "SUNDAY"))

#Injuries occured each day of the week
df.inju<-as.data.frame(filter(df, INJURY=='YES'))
df.GroupBydays <- group_by(df.inju, DAY_OF_WEEK)
accidents <- (summarize(subset.GroupBydays, count = n()))
accidents

"DPLYR is much more easier as we can easily summarise a whole dataset based on a given filter. 
Also we can apply different filters on different columns at the same time"

#Distribution of the number of vehicles in accident on Friday
friday<-na.omit(filter(df,trimws(DAY_OF_WEEK) == "FRIDAY"))
hist(as.integer(friday$VEHICLE_COUNT))
quantile(as.integer(friday$VEHICLE_COUNT))

" As we can see from the histogram & the quantile distribution, the vehicles in accident on Friday is right skewed.
And the peak indicates that the maximum number of accidents involved 2 vehicles."


sunday<-na.omit(filter(df,trimws(DAY_OF_WEEK) == "SUNDAY"))
hist(as.integer(sunday$VEHICLE_COUNT))
quantile(as.integer(sunday$VEHICLE_COUNT))

"In this distribution, we can clearly see that the most number of accidents on Sunday involved just one 
vehicle. And the distribution is right skewed as well. Also from the quantile command we can see that 
more than 50% of accidents involved just one vehicle."
