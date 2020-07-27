spam.raw <- read.csv('text data.csv', stringsAsFactors = FALSE)
str(spam.raw)

#prepsring the file
spam <- spam.raw
spam$type <- as.factor(spam$type)
str(spam)

#############################################################
#working on the file

library(ggplot2)
ggplot(spam, aes(type)) + geom_bar()

head(spam)

#library for text mining
install.packages('tm')
library(tm)

#build our data set as Corpus
spam.corpus <- Corpus(VectorSource(spam$text))

clean.corpus <- tm_map(spam.corpus, removePunctuation)
clean.corpus[[1]][[1]]
clean.corpus <- tm_map(clean.corpus, removeNumbers)
clean.corpus <- tm_map(clean.corpus, content_transformer(tolower))
clean.corpus <- tm_map(clean.corpus, removeWords, stopwords())
clean.corpus <- tm_map(clean.corpus, stripWhitespace)

dtm <- DocumentTermMatrix(clean.corpus)
dim(dtm)

#we want to remove words that apear in under 10 documents
dtm.freq <- DocumentTermMatrix(clean.corpus, list(dictionary = findFreqTerms(dtm,10)))
dim(dtm.freq)

inspect(dtm.freq[1:10, 1:20])

conv_01 <- function(x){
  x <- ifelse(x>0,1,0)
  return (as.integer(x))
}

dtm.final <- apply(dtm.freq, MARGIN = 1:2, conv_01)
dtm.df <- as.data.frame(dtm.final)
#turn dtm to a data fram
conv_01_type <- function(x){
  if(x=='ham')
    return(as.integer(0))
  return(as.integer(1))
}

spam$type <- sapply(spam$type, conv_01_type)
dtm.df$type <- spam$type
str(dtm.df)

#split to test and train
library(caTools)

filter <- sample.split(dtm.df$type, SplitRatio = 0.7)
spam.train = subset(dtm.df, filter==T)
spam.test = subset(dtm.df, filter==F)
dim(spam.train)
dim(spam.test)

#make the model
spam.model <- glm(type ~., family = binomial(link = 'logit'), data = spam.train)
summary(spam.model)

#prediction of the test set
prediction <- predict(spam.model, spam.test, type = 'response')
hist(prediction)

#confusion matrix
actual <- spam.test$type
confusion.matrix <- table(actual,prediction > 0.5)

precision <- confusion.matrix[2,2]/(confusion.matrix[2,2]+confusion.matrix[1,2])
recall <- confusion.matrix[2,2]/(confusion.matrix[2,2]+confusion.matrix[2,1])

#we will perform oversampling
install.packages('ROSE')
library(ROSE)

spam.train.over <- ovun.sample(type~., data = spam.train, method = 'over', N = 600)$data
table(spam.train.over$type)

#make the model - AGAIN
spam.model.over <- glm(type ~., family = binomial(link = 'logit'), data = spam.train.over)
summary(spam.model.over)

#prediction of the test set
prediction.over <- predict(spam.model.over, spam.test, type = 'response')
hist(prediction.over)

#confusion matrix
actual <- spam.test$type
confusion.matrix <- table(actual,prediction.over > 0.5)

precision.over <- confusion.matrix[2,2]/(confusion.matrix[2,2]+confusion.matrix[1,2])
recall.over <- confusion.matrix[2,2]/(confusion.matrix[2,2]+confusion.matrix[2,1])











