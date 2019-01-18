#IST687 - HW 8 - Linear Model
#Sanjeev Ramasamy Seenivasagamani
#Homework 8
#Due Date: Oct 24, 2018
#Submitted: Oct 24, 2018

library(rjson)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)


us<-map_data("state")
data=fromJSON(file.choose())
df<-data.frame(data)


str(df)
plot(df$hotelSize,df$overallCustSat)

#1.
hotelsize<-ggplot(df, aes(x=jitter(hotelSize), y=overallCustSat))+geom_point()+labs(x="Size of the Hotel",y="Overall Customer Satisfaction")
hotelsize
#Observation: Almost all the observations are in the range of 5-10 with a few outliers.

#2
checkin<-ggplot(df, aes(x=jitter(checkInSat), y=overallCustSat))+geom_point()+labs(x="Check in Sat",y="Overall Customer Satisfaction")
checkin
#Obeservation: Most values are in the range of 3-10 with some outliers

#3
df$hotelState<-tolower(df$hotelState)
hotelstate<-ggplot(df, aes(map_id=df$hotelState))+geom_map(data=df, map=us, color="black",aes(fill=factor(df$overallCustSat)))+expand_limits(x=us$long,y=us$lat)
hotelstate
#Obeservation: Many states have a Overall Customer Satisfaction Score of 7

#Another way to plot the same data
#Mean Overall Customer Satisfaction Statewise
states <- df %>% group_by(hotelState) %>%  summarize(Mean_Overall_Customer_Satisfaction = mean(overallCustSat))
states <- as.data.frame(states)
states2<-ggplot(states,aes(hotelState,Mean_Overall_Customer_Satisfaction)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 0.85))
states2
#Obeservation: we can see a result similar to the previous one.

#4
hotelclean<-ggplot(df, aes(x=jitter(hotelClean), y=overallCustSat))+geom_point()+labs(x="Cleanliness of the Hotel",y="Overall Customer Satisfaction")
hotelclean
#Obeservation: Higher the cleanliness higher the customer satisfaction

#5
hotelfriend<-ggplot(df, aes(x=jitter(hotelFriendly), y=overallCustSat))+geom_point()+labs(x="Friendliness of the Hotel",y="Overall Customer Satisfaction")
hotelfriend
#Obeservation: Higher the friendliness higher the satisfaction rating

#6

plot(df$gender,df$overallCustSat, xlab="Gender", ylab="Overall Customer Satisfaction")
#Obeservation: The average rating by male and female are around 7

#7
guestage<-ggplot(df, aes(x=jitter(guestAge), y=overallCustSat))+geom_point()+labs(x="Age of the Guest",y="Overall Customer Satisfaction")
guestage
#Obeservation: Most of the customers who rated around 7-8 were within an age group 40-60

#8--------------------------------------------------------------------------------------
stay<-ggplot(df,aes(jitter(lengthOfStay),overallCustSat)) + geom_point()+labs(x="Length of Stay",y="Overall Customer Satisfaction")
stay
#Obeservation: The ratings are well spreadout without a pattern

#9
trip<-ggplot(df, aes(x=jitter(whenBookedTrip), y=overallCustSat))+geom_point()+labs(x="When Booked Trip", y="Overall Customer Satisfaction")
trip
#Obeservation: With a few outliers, most of ratings were inbetween 6 and 8 when the booking was done withon 40 days.

#Part:B
#Generate linear model without the categorical variables
model<-lm(formula = overallCustSat ~ hotelSize + checkInSat +hotelState+ hotelClean + hotelFriendly +gender+ guestAge + lengthOfStay + whenBookedTrip, data = df )
summary(model)

#Adjusted R-Squared: 0.668
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                     8.321e+00  1.024e-01  81.276  < 2e-16 ***
#  hotelSize                       7.043e-05  1.117e-04   0.630   0.5284    
#checkInSat                     -2.381e-01  5.544e-03 -42.940  < 2e-16 ***
#  hotelStatealaska               -5.985e-02  1.366e-01  -0.438   0.6613    
#hotelStatearizona               8.216e-02  6.001e-02   1.369   0.1710    
#hotelStatearkansas              4.340e-02  7.690e-02   0.564   0.5725    
#hotelStatecalifornia            2.606e-02  4.901e-02   0.532   0.5950    
#hotelStatecolorado             -8.696e-02  6.227e-02  -1.397   0.1626    
#hotelStateconnecticut           3.016e-02  6.835e-02   0.441   0.6591    
#hotelStatedelaware             -7.653e-02  1.277e-01  -0.599   0.5489    
#hotelStatedistrict of columbia -1.008e-01  1.366e-01  -0.738   0.4605    
#hotelStateflorida              -1.347e-02  5.107e-02  -0.264   0.7919    
#hotelStategeorgia               7.055e-02  5.534e-02   1.275   0.2023    
#hotelStatehawaii                8.258e-02  9.439e-02   0.875   0.3816    
#hotelStateidaho                -3.911e-02  9.281e-02  -0.421   0.6735    
#hotelStateillinois              2.974e-02  5.415e-02   0.549   0.5829    
#hotelStateindiana               6.519e-02  6.015e-02   1.084   0.2785    
#hotelStateiowa                  7.377e-02  7.384e-02   0.999   0.3178    
#hotelStatekansas               -9.678e-03  7.267e-02  -0.133   0.8941    
#hotelStatekentucky              7.134e-03  6.492e-02   0.110   0.9125    
#hotelStatelouisiana             4.052e-02  6.703e-02   0.605   0.5455    
#hotelStatemaine                -1.323e-01  9.789e-02  -1.352   0.1764    
#hotelStatemaryland              9.191e-03  6.184e-02   0.149   0.8819    
#hotelStatemassachusetts         5.491e-02  5.874e-02   0.935   0.3499    
#hotelStatemichigan              1.010e-01  5.630e-02   1.794   0.0729 .  
#hotelStateminnesota             2.388e-02  6.280e-02   0.380   0.7038    
#hotelStatemississippi           6.802e-02  6.865e-02   0.991   0.3218    
#hotelStatemissouri             -2.985e-02  6.131e-02  -0.487   0.6264    
#hotelStatemontana              -2.722e-02  1.098e-01  -0.248   0.8042    
#hotelStatenebraska              8.879e-02  8.935e-02   0.994   0.3204    
#hotelStatenevada               -8.026e-02  7.464e-02  -1.075   0.2823    
#hotelStatenew hampshire         4.062e-03  8.807e-02   0.046   0.9632    
#hotelStatenew jersey            5.971e-02  5.669e-02   1.053   0.2923    
#hotelStatenew mexico            7.146e-02  7.725e-02   0.925   0.3550    
#hotelStatenew york              4.431e-02  5.140e-02   0.862   0.3887    
#hotelStatenorth carolina        3.346e-02  5.634e-02   0.594   0.5526    
#hotelStatenorth dakota          2.139e-01  1.147e-01   1.864   0.0624 .  
#hotelStateohio                 -2.965e-02  5.429e-02  -0.546   0.5850    
#hotelStateoklahoma              4.172e-02  6.852e-02   0.609   0.5426    
#hotelStateoregon               -2.080e-02  6.789e-02  -0.306   0.7593    
#hotelStatepennsylvania          1.897e-02  5.385e-02   0.352   0.7247    
#hotelStaterhode island          2.979e-02  1.032e-01   0.289   0.7728    
#hotelStatesouth carolina        5.992e-02  6.175e-02   0.970   0.3318    
#hotelStatesouth dakota          3.161e-02  9.985e-02   0.317   0.7515    
#hotelStatetennessee            -2.498e-02  5.946e-02  -0.420   0.6743    
#hotelStatetexas                -3.663e-03  5.002e-02  -0.073   0.9416    
#hotelStateutah                  7.941e-02  7.311e-02   1.086   0.2774    
#hotelStatevermont               8.604e-02  1.205e-01   0.714   0.4752    
#hotelStatevirginia             -1.275e-02  5.782e-02  -0.221   0.8254    
#hotelStatewashington            3.520e-02  5.899e-02   0.597   0.5508    
#hotelStatewest virginia         7.765e-02  8.478e-02   0.916   0.3597    
#hotelStatewisconsin             1.293e-02  6.155e-02   0.210   0.8336    
#hotelStatewyoming               1.080e-01  1.529e-01   0.706   0.4800    
#hotelClean                      4.042e-02  6.941e-03   5.824 5.93e-09 ***
#  hotelFriendly                   1.122e+00  8.863e-03 126.557  < 2e-16 ***
#  genderMALE                      1.218e-02  1.116e-02   1.091   0.2752    
#guestAge                       -1.205e-01  1.815e-03 -66.400  < 2e-16 ***
#  lengthOfStay                   -3.284e-01  1.677e-02 -19.575  < 2e-16 ***
#  whenBookedTrip                  6.421e-03  1.005e-03   6.387 1.77e-10 ***

#Significant Variables: CheckinSat, hotelClean, hotelFriendly, guuestAge, lengthOfStay, wheBookedTrip

#Apart from the hotelState, the model receives a significant inpact from the number of days the hotel was booked in advance 
#and the cleanliness of the hotel.The more clean the hotel and earlier the customer books higher is the satisfaction 


#PArt C
m1<-lm(formula = overallCustSat ~ hotelFriendly, data = df)
summary(m1)

#Based on the values of R Squared, its the friendliness of the hotel that can best describe the satisfaction of the customer. 


