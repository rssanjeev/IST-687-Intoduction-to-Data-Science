#IST687 - HW 2 - Dataframes & Sorting
#Sanjeev Ramasamy Seenivasagamani
#Homework 2
#Due Date: Sept 6, 2018
#Submitted: Sep 12, 2018

#Initializing the dataframe
arrests<-USArrests
str(arrests)

#Exploring the assault rate
summary(arrests$Assault)
#Lowest is the best assault rate

#Which city has the best assault rate?
arrests$Assault.sort()
narrests<-arrests[order(arrests$Assault),]
head(narrests,1)
#North Dakota has the best assault rate

#Explore the murder rate
nmurder <- arrests[order(-arrests$Murder),] 
head(nmurder,1)
#Georgia has the highest murder rate

#10 states with highest murder rates
head(nmurder,10)

#Value in the 20th row 3rd column
nmurder[20,3]

#Least Safe State
"To find the least ssafe state, first we have to bring all the values into a single scale. To achieve this, I am dividing each variabe into five sectors and assigning
values to each sector. For example: if the Murder rate is less than or equal to 4 those states are given a Murder_score of 1, else if they're less than or equal to 8
its 2. Thus we bring each an every variable to a single scale."
summary(narrests$Murder)
narrests$Murder_Score<-ifelse(narrests$Murder<=4,1,
                              ifelse (narrests$Murder<=8,2,
                                      ifelse (narrests$Murder<=12,3,
                                              ifelse (narrests$Murder<=16,4,5))))
summary(narrests$Assault)
narrests$Assault_Score<-ifelse(narrests$Assault<=70,1,
                               ifelse (narrests$Assault<=140,2,
                                       ifelse (narrests$Assault<=210,3,
                                               ifelse (narrests$Assault<=280,4,5))))
summary(narrests$UrbanPop)
narrests$UrbanPop_Score<-ifelse(narrests$UrbanPop<=20,1,
                                ifelse (narrests$UrbanPop<=40,2,
                                        ifelse (narrests$UrbanPop<=60,3,
                                                ifelse (narrests$UrbanPop<=80,4,5))))
summary(narrests$Rape)
narrests$Rape_Score<-ifelse(narrests$Rape<=10,1,
                            ifelse (narrests$Rape<=20,2,
                                    ifelse (narrests$Rape<=30,3,
                                            ifelse (narrests$Rape<=40,4,5))))
"After calculating all the scores for the variables, we now calculate the total score by adding all the individual scores."
narrests$Total_score<-(narrests$Murder_Score+narrests$Assault_Score+narrests$UrbanPop_Score+narrests$Rape_Score)

narrests

"Arranging the dataframe in descending order based on the total_score"
final<-narrests[order(-narrests$Total_score),]
head(final,3)

#Thus we can conclude that NEVADA is the unsafest state.