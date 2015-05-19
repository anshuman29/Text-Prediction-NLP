###Loading the Required Packages

library("ggplot2") #Charting functionality
library("wordcloud")
library("stylo")
library("tm") #For Text Mining & Corpus workings
library("stringi")
library("data.table")

# import the blogs and twitter datasets in text mode
blogs <- readLines("./SwiftKeyData/en_US/en_US.blogs.txt", encoding="UTF-8")
tweets <- readLines("./SwiftKeyData/en_US/en_US.twitter.txt", encoding="UTF-8")

# import the news dataset in binary mode
con <- file("./SwiftKeyData/en_US/en_US.news.txt", open="rb")
news <- readLines(con, encoding="UTF-8")
close(con)
rm(con)

#Creating Training Sets 
set.seed(300000)
blogsLineNumbers <- sample(1:899288, 300000, replace=F)
blogsLen<- length(blogsLineNumbers)
blogsTrainLineNumbers<-blogsLineNumbers[1:(blogsLen-1000)]
blogsTestLineNumbers<-blogsLineNumbers[((blogsLen-1000)+1):blogsLen]
blogsTraining<-blogs[blogsTrainLineNumbers]
blogsTesting<-blogs[blogsTestLineNumbers]

set.seed(300000)
newsLineNumbers <- sample(1:1010242, 300000, replace=F)
newsLen<- length(newsLineNumbers)
newsTrainLineNumbers<-newsLineNumbers[1:(newsLen-1000)]
newsTestLineNumbers<-newsLineNumbers[((newsLen-1000)+1):newsLen]
newsTraining<-news[newsTrainLineNumbers]
newsTesting<-news[newsTestLineNumbers]

set.seed(300000)
tweetsLineNumbers <- sample(1:2360148, 300000, replace=F)
tweetsLen<- length(tweetsLineNumbers)
tweetsTrainLineNumbers<-tweetsLineNumbers[1:(tweetsLen-1000)]
tweetsTestLineNumbers<-tweetsLineNumbers[((tweetsLen-1000)+1):tweetsLen]
tweetsTraining<-tweets[tweetsTrainLineNumbers]
tweetsTesting<-tweets[tweetsTestLineNumbers]

training<-c(blogsTraining,newsTraining,tweetsTraining) 

#Removing the large datasets to free memory
rm(tweets)
rm(blogs)
rm(news)

#Removing non UTF-8 characters 
training <- iconv(training, from = "UTF-8", to = "latin1") 
training<-iconv(training, "latin1", "ASCII")

blogsTesting <- iconv(blogsTesting, from = "UTF-8", to = "latin1") 
blogsTesting<-iconv(blogsTesting, "latin1", "ASCII")

newsTesting <- iconv(newsTesting, from = "UTF-8", to = "latin1") 
newsTesting<-iconv(newsTesting, "latin1", "ASCII")

tweetsTesting <- iconv(tweetsTesting, from = "UTF-8", to = "latin1") 
tweetsTesting<-iconv(tweetsTesting, "latin1", "ASCII")


### Task 1 : Tokenization and Profanity Filtering

#Function to PreProcess
preProcessCorpus<-function(training){
  training<-removePunctuation(training,preserve_intra_word_dashes = TRUE) 
  #Build a Corpus, which is a collection of text documents
  #VectorSource specifies that the source is character vectors
  corpus <- VCorpus(VectorSource(training))
  corpus<-tm_map(corpus, content_transformer(tolower))
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus, PlainTextDocument)
  corpus
}

cleanTrainingCorpus<-preProcessCorpus(training)
cleanTrainingDataFrame<-data.frame(text=unlist(sapply(cleanTrainingCorpus, `[`, "content")), stringsAsFactors=F)


#Creating 1Gram, 2Gram, 3Gram tokens
oneGramTokens<-txt.to.words(cleanTrainingDataFrame)
twoGramTokens<-make.ngrams(oneGramTokens, ngram.size = 2)
threeGramTokens<-make.ngrams(oneGramTokens, ngram.size = 3)
fourGramTokens<-make.ngrams(oneGramTokens, ngram.size = 4)

### Task 2 : Exploratory Analysis


##Creating a data frame with the word frequencies
oneWord<-data.frame(table(oneGramTokens))
twoWord<-data.frame(table(twoGramTokens))
threeWord<-data.frame(table(threeGramTokens))
fourWord<-data.frame(table(fourGramTokens))

names(oneWord)<-c("Token1","Freq")
names(twoWord)<-c("Token1","Freq")
names(threeWord)<-c("Token1","Freq")
names(fourWord)<-c("Token1","Freq")

#Removing single occurences
fourWord<-fourWord[!fourWord$Freq==1,]
threeWord<-threeWord[!threeWord$Freq==1,]
twoWord<-twoWord[!twoWord$Freq==1,]
oneWord<-oneWord[!oneWord$Freq==1,]

#rm(variableName) for releasing memory
