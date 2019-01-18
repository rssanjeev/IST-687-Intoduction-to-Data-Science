#IST687 - HW 1 - Intro Homework
#Sanjeev Ramasamy Seenivasagamani
#Homework 1
#Due Date: Sept 5, 2018
#Submitted: Aug 30, 2018

#Step A: Create a Vector  

#Creating vectors with the specified values
grades<-c(4.0,3.3,3.7)
courseName<-c("Bio","Math","History")
BetterThanB<-3

#Step B: Calculating statistics using R  

#Calculating the average of the grades vector
mean(grades)
#Calculating the length of the vector and assigning it to a new variable
total.length<-length(grades)
total.length
#Calculating the sum of all the grades and assigning it to a new variable
total<-sum(grades)
total

#Calculating the mean using the two new variables
total/total.length

#Step C: Using the max/min functions in R

#Calculating the maximum grade
maxG<-max(grades)
maxG
#Calculating the lowest grade
minG<-min(grades)
minG

#Step D: Vector Math

#Incrementing the values in the vector by 0.3
betterGrades<-(grades+0.3)
betterGrades

#Finding the average of the new grades
mean(betterGrades)

#Step E: Using Conditional if statements

#Checking if maxG is greated than 3.5
if(maxG>3.5) "yes" else "no"
#Checking if minG is greater than the variable 'BetterThanB'
if(minG>BetterThanB) "yes" else "no"

#Step F: Accessing an element in a vector

#Retrieving the second class from the Coursename vector
courseName[2]