library(jsonlite)
library(kernlab)

#Part:A Load and Condition the data
rdata<-fromJSON(file.choose())

#Pat B: Creating a dummy variable for Overall Customer Satisfaction
rdata$CustSatBucket <- replicate(length(rdata$overallCustSat), "Not_Happy")
rdata$CustSatBucket[rdata$overallCustSat > 7] <- "Happy"
rdata<-data.frame(rdata)

str(rdata)
length(rdata)

#Part C: Creating Training & Test datasets
rand<-sample(1:dim(rdata)[1])
summary(rand)
length(rand)
tdata<-rdata
tdata<-tdata[,-c(1)]

cutpoint2_3<-floor(2*dim(tdata)[1]/3)
cutpoint2_3

traindata<-tdata[rand[1:cutpoint2_3],]
testdata<-tdata[rand[(cutpoint2_3+1):dim(rdata)[1]],]
str(testdata)

#Part D: Build a mmodel using ksvm()
svmoutput<-ksvm(CustSatBucket ~ hotelSize+checkInSat+hotelState+hotelClean+hotelFriendly+gender+guestAge+lengthOfStay+whenBookedTrip,data=traindata, kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
svmoutput

"Training error is 7.1% whereas the Cross Validation Error is 10.5%. With the threefold cross validation we can generate the 
probabilities associated with whether the customer is happy or not. Thus by using the threefold (k-fold) we overcome the 
problem of overfitting where the model gets used to the training data and replicates it whenever necesssary. This will fail
when a different dataset is used to test the model. Thus, the cross-validation will generalise the model so that it'll do justice
to the test dataset."

#Part E: Predict Values in the Test Data and Create a Confusion Matrix
svmpred<-predict(svmoutput, testdata, type="votes")
str(svmpred)
comptable<-table(data.frame(testdata$CustSatBucket,svmpred[1,]))

comptable

rate<-(comptable[1,1]+comptable[2,2])/sum(comptable)
rate

#Part F: Find a good prediction
svmoutput2<-ksvm(CustSatBucket ~ checkInSat+hotelClean+hotelFriendly+guestAge+lengthOfStay+whenBookedTrip,data=traindata, kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
svmpred2<-predict(svmoutput2, testdata, type="votes")
comptable2<-table(data.frame(testdata$CustSatBucket,svmpred2[1,]))
rate2<-(comptable2[1,1]+comptable2[2,2])/sum(comptable2)
rate2

"Through cross validation we created a generalised model. But to test that model we need data that wasn't involed in the training 
of the model. Thus by splitting the dataset into training and test we can use the former to train the model and the latter to 
test it. This works perfectly as we already have the result with us, we can now check the efficiency & performace of the model
by creating a confusion matric which will show the actualy outcome variable and the outcome that was predicted by the model."