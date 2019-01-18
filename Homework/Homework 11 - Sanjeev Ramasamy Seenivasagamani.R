#install.packages("kernlab")
library(kernlab)
library(RJSONIO)
library(jsonlite)

#install.packages("tm")
library(tm)


install.packages("wordcloud")
library(wordcloud)
#Part A: Load and condition the text file that contains the speech  
#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).
JsonData<-fromJSON(txt = "hotelSurveySherison.json")

#Convert Json data to a data frame
data <- data.frame(JsonData)
#View(Survey)
str(data)
summary(data)

pos <- "positive-words.txt"
p <- scan(pos, character(0), sep="\n")
str(p)
summary(p)
View(p)


neg <- "negative-words.txt"
n <- scan(neg, character(0), sep="\n")
str(n)
summary(n)
View(n)

p <- p[-c(1:34)]

n <- n[-c(1:34)]


head(p, 30)
head(n, 30)

#2.	The key column to focus on is the 'freeText' column.

words.vec <- VectorSource(data$freeText)
words.corpus <- Corpus(words.vec)
words.corpus

#3
words.corpus <-tm_map(words.corpus, content_transformer(tolower))
words.corpus <-tm_map(words.corpus, removeNumbers)
words.corpus <-tm_map(words.corpus, removeWords, stopwords("English"))
words.corpus <-tm_map(words.corpus, removePunctuation)
tdm<-TermDocumentMatrix(words.corpus)
tdm
m <- as.matrix(tdm)
wordsCount <- rowSums(m)
wordsCount <- sort(wordsCount, decreasing= TRUE)
wordsCount

#4
totalwords <- sum(wordsCount)
words <- names(wordsCount)
matched <- match(words, p , nomatch = 0)
matching <- wordsCount[which(matched != 0)]
matchingwords <- names(matching)
nPos <- sum(matching)
nPos

matched <- match(words, n , nomatch = 0)
count <- wordsCount[which(matched != 0)]
negmatching <- names(count)
nNeg <- sum(count)
nNeg

#4.	Calculate the percent positive words and negative words.
totalwords <- length(words)
posratio <- nPos/totalwords 
print(posratio)
negratio <- nNeg/totalwords
print(negratio)

#5: The positive words are more frequent than the negative words

#6
wordcloud(words,wordsCount, min.freq = 2, max.words = 50, rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

#7.	
negdata <- data.frame(word=negmatching,freq=count)
posdata <- data.frame(word=matchingwords, freq = matching)
install.packages('ggplot2')
library(ggplot2)
HighfreqNwords <- subset(negdata, negdata$freq > 1)
NBarPlot <- ggplot(HighFrequencyNegWords, aes(x=reorder(word, freq),y=freq)) + geom_col() +
  theme(axis.text.x=element_text(angle = 80))
NBarPlot

HighfreqPwords <- subset(posdata, posdata$freq > 1)
PosWordsBarPlot <- ggplot(HighFrequencyPosWords, aes(x=reorder(word, freq),y=freq)) + geom_col() +
  theme(axis.text.x=element_text(angle = 80))
PBarPlot

#8:Hotel, Room, Safety and few of the most freq words. As the wordcloud pictorically represents the frequency of the words in the corrpus.
#The same is represented in a much more traditional approach through bar plots for positive and negative words separately. 

#9: Yes, they do. Words such as nice, friendly and expensive, worse are represented as positive and negative words which make complete sense,
#And, the wordcloud is much more vibrant and attractive compared to the traditional bar plot

#10 & 11
happy <- subset(df, df$overallCustSat > 7)
sad<- subset(df, df$overallCustSat <=7 )

#Happy Customers
words.vec <- VectorSource(happy$freeText)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <-tm_map(words.corpus, content_transformer(tolower))
words.corpus <-tm_map(words.corpus, removeNumbers)
words.corpus <-tm_map(words.corpus, removeWords, stopwords("English"))
words.corpus <-tm_map(words.corpus, removePunctuation)
tdm<-TermDocumentMatrix(words.corpus)
tdm
m <- as.matrix(tdm)
wordsCount <- rowSums(m)
wordsCount <- sort(wordsCount, decreasing= TRUE)
wordsCount

totalwords <- sum(wordsCount)
words <- names(wordsCount)
matched <- match(words, p , nomatch = 0)
matching <- wordsCount[which(matched != 0)]
matchingwords <- names(matching)
nPos <- sum(matching)
nPos

matched <- match(words, n , nomatch = 0)
count <- wordsCount[which(matched != 0)]
negmatching <- names(count)
nNeg <- sum(count)
nNeg

#Calculate the percent positive words and negative words.
totalwords <- length(words)
posratio <- nPos/totalwords 
print(posratio)
negratio <- nNeg/totalwords
print(negratio)
wordcloud(words,wordsCount, min.freq = 2, max.words = 50, rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

negdata <- data.frame(word=negmatching,freq=count)
posdata <- data.frame(word=matchingwords, freq = matching)
HighfreqNwords <- subset(negdata, negdata$freq > 1)
NBarPlot <- ggplot(HighFrequencyNegWords, aes(x=reorder(word, freq),y=freq)) + geom_col() +
  theme(axis.text.x=element_text(angle = 80))
NBarPlot

HighfreqPwords <- subset(posdata, posdata$freq > 1)
PosWordsBarPlot <- ggplot(HighFrequencyPosWords, aes(x=reorder(word, freq),y=freq)) + geom_col() +
  theme(axis.text.x=element_text(angle = 80))
PBarPlot

#Sad Customers
words.vec <- VectorSource(sad$freeText)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <-tm_map(words.corpus, content_transformer(tolower))
words.corpus <-tm_map(words.corpus, removeNumbers)
words.corpus <-tm_map(words.corpus, removeWords, stopwords("English"))
words.corpus <-tm_map(words.corpus, removePunctuation)
tdm<-TermDocumentMatrix(words.corpus)
tdm
m <- as.matrix(tdm)
wordsCount <- rowSums(m)
wordsCount <- sort(wordsCount, decreasing= TRUE)
wordsCount

totalwords <- sum(wordsCount)
words <- names(wordsCount)
matched <- match(words, p , nomatch = 0)
matching <- wordsCount[which(matched != 0)]
matchingwords <- names(matching)
nPos <- sum(matching)
nPos

matched <- match(words, n , nomatch = 0)
count <- wordsCount[which(matched != 0)]
negmatching <- names(count)
nNeg <- sum(count)
nNeg

#Calculate the percent positive words and negative words.
totalwords <- length(words)
posratio <- nPos/totalwords 
print(posratio)
negratio <- nNeg/totalwords
print(negratio)
negdata <- data.frame(word=negmatching,freq=count)
posdata <- data.frame(word=matchingwords, freq = matching)
HighfreqNwords <- subset(negdata, negdata$freq > 1)
NBarPlot <- ggplot(HighFrequencyNegWords, aes(x=reorder(word, freq),y=freq)) + geom_col() + theme(axis.text.x=element_text(angle = 80))
NBarPlot
HighfreqPwords <- subset(posdata, posdata$freq > 1)
PosWordsBarPlot <- ggplot(HighFrequencyPosWords, aes(x=reorder(word, freq),y=freq)) + geom_col() + theme(axis.text.x=element_text(angle = 80))
PBarPlot

#12
"Positive Ratio for Happy Customers: 44.48% 
and 
Negative Ratio for Happy Customers: 17.2"
#Higher Postive ratio for Happy Customers


"Positive Ratio for Sad Customers: 9.9%
and
Negative Ratio for Sad Customers: 16.3"
#Higher Negative Ratio for Sad Customers


