library(jsonlite)

Survey <- fromJSON("hotelSurveyBarriot.json")
Survey <- data.frame(Survey)

#Creating a dummy variable for Overall Customer Satisfaction
Survey$CustSatBucket <- replicate(length(Survey$overallCustSat), "Average")
Survey$CustSatBucket[Survey$overallCustSat > 7] <- "High"
Survey$CustSatBucket[Survey$overallCustSat < 7] <- "Low"
str(Survey)

#Creating a dummy variable for Check-in Satisfaction
Survey$checkInSatBucket <- replicate(length(Survey$checkInSat), "Average")
Survey$checkInSatBucket[Survey$checkInSat > 7] <- "High"
Survey$checkInSatBucket[Survey$checkInSat < 7] <- "Low"

#Creating a dummy variable for Hotel Cleanliness
Survey$hotelCleanBucket <- replicate(length(Survey$hotelClean), "Average")
Survey$hotelCleanBucket[Survey$hotelClean > 7] <- "High"
Survey$hotelCleanBucket[Survey$hotelClean < 7] <- "Low"

#Creating a dummy variable for Hotel Friendliness
Survey$hotelFriendlyBucket <- replicate(length(Survey$hotelFriendly), "Average")
Survey$hotelFriendlyBucket[Survey$hotelFriendly > 7] <- "High"
Survey$hotelFriendlyBucket[Survey$hotelFriendly < 7] <- "Low"

#Creating a dummy variable for Hotel Size
q <- quantile(Survey$hotelSize, c(0.4, 0.6))
Survey$hotelSizeBucket <- replicate(length(Survey$hotelSize), "Average")
Survey$hotelSizeBucket[Survey$hotelSize <= q[1]] <- "Low"
Survey$hotelSizeBucket[Survey$hotelSize > q[2]] <- "High"

#Creating a dummy variable for Guest Age
q <- quantile(Survey$guestAge, c(0.4, 0.6))
Survey$guestAgeBucket <- replicate(length(Survey$guestAge), "Average")
Survey$guestAgeBucket[Survey$guestAge <= q[1]] <- "Low"
Survey$guestAgeBucket[Survey$guestAge > q[2]] <- "High"

#Creating a dummy variable for Length of Stay
q <- quantile(Survey$lengthOfStay, c(0.4, 0.6))
Survey$lengthOfStayBucket <- replicate(length(Survey$lengthOfStay), "Average")
Survey$lengthOfStayBucket[Survey$lengthOfStay <= q[1]] <- "Low"
Survey$lengthOfStayBucket[Survey$lengthOfStay > q[2]] <- "High"

#Creating a dummy variable for When Booked trip
q <- quantile(Survey$whenBookedTrip, c(0.4, 0.6))
Survey$whenBookedTripBucket <- replicate(length(Survey$whenBookedTrip), "Average")
Survey$whenBookedTripBucket[Survey$whenBookedTrip <= q[1]] <- "Low"
Survey$whenBookedTripBucket[Survey$whenBookedTrip > q[2]] <- "High"

str(Survey)

#Table for Age
table(Survey$guestAgeBucket)

#Table for Hotel Friendliness
table(Survey$hotelFriendlyBucket)

#Distribution of Age in percentage
prop.table(table(Survey$guestAgeBucket, dnn = "Distribution of various Age groups in Percentage"))

#Distribution of Hotel Friendliness in percentage
prop.table(table(Survey$hotelFriendlyBucket, dnn = "Distribution of Hotel Friendliness in Percentage "))

#Contingency Table of percentages for Age and Overall Satisfaction
prop.table(table(Survey$hotelFriendlyBucket, Survey$CustSatBucket, dnn = c("Age", "Customer Satisfaction")))

#Observation: 34.5% of the customer are young and they provided average ratings to the hotels, whereas 12% rated 'Low'.
#24% of the customers were at an average age have rated 'High'.But, old age customers who rated 'High'are 
#mere 5%.

#Loading the required packages & libraries
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

head(Survey)

#Creating a new dataframe with just the buckets and the overall customer satisfaction
nSurvey<-Survey[,c(12,13,14,15,16,17,18,19)]
#Converting the variable type to factors for all the variables
newSurvey<-data.frame(sapply(nSurvey,as.factor))

#Coercing the dataframe into a sparce 'transaction' matrix
SurveyX <- as(newSurvey,"transactions")
 
#Exploring SurveyX
inspect(SurveyX)
itemFrequency(SurveyX)
itemFrequencyPlot(SurveyX)

#Pattern recognition 
rules<-apriori(SurveyX, parameter = list(supp=0.1, conf=0.5), appearance=list(default="lhs",rhs=("CustSatBucket=High")))

#Inspecting the 'rules'
inspect(rules)
rules<-sort(rules, decreasing = TRUE, by="confidence")
inspect(rules)
arulesViz::inspectDT(rules)

"After inspecting the rules, I would suggest the hotel owner to keep the hotel neat & clean, keep the check-in
process smooth & highly satisfactory, to resuest the customers to book very mnuch in advance &  mostly of all be friendly
with the customers. This would ensure a high overall customer satisfaction rating." 
