shopping.raw <- read.csv('online_shoppers_intention.csv')
shopping <- shopping.raw

#check the structure
str(shopping)

#target balance- we want our target column to be balanced!

#for graphs 
library(ggplot2)

ggplot(shopping, aes(Revenue)) + geom_bar()
#we learned that most of the people dont buy

ggplot(shopping, aes(Administrative, fill=Revenue)) + geom_bar()
ggplot(shopping, aes(Administrative, fill=Revenue)) + geom_bar(position='fill')

coercx <- function(x, by){
  if(x<=by) return(x)
  return(by)
}

shopping$Administrative <- sapply(shopping$Administrative, coercx, by=10)

ggplot(shopping, aes(PageValues, fill=Revenue)) + geom_histogram()
ggplot(shopping, aes(PageValues, fill=Revenue)) + geom_histogram(position='fill')

ggplot(shopping, aes(Informational, fill=Revenue)) + geom_bar()
ggplot(shopping, aes(Informational, fill=Revenue)) + geom_bar(position='fill')

shopping$PageValues <- sapply(shopping$PageValues, coercx, by=10)

#install.packages('corrplot')
library(corrplot)

#df that contains only duration columns
dur.df <- shopping[,c("Administrative_Duration","Informational_Duration","ProductRelated_Duration")]
str(dur.df)

corr.matrix <- cor(dur.df)
corrplot(corr.matrix, method = 'circle')

head(dur.df)

#build a decidion tree
library(caTools)
#more important libraries
library(rpart)
library(rpart.plot)
#create the filter
filter <- sample.split(shopping$Revenue, SplitRatio = 0.7)
shopping.train <- subset(shopping, filter == T)
shopping.test <- subset(shopping, filter == F)

model.dt <- rpart(Revenue ~., shopping.train)
rpart.plot(model.dt, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

#make a prediction on the test set
prediction <- predict(model.dt, shopping.test)
#build actual column
actual <-shopping.test$Revenue

#build the confusion matrix
cf <- table(actual,prediction >0.5)
precision <- cf['TRUE', 'TRUE']/(cf['TRUE', 'TRUE']+cf['TRUE', 'FALSE'])
recall <- cf['TRUE', 'TRUE']/(cf['TRUE', 'TRUE']+cf['FALSE', 'TRUE'])

#create a random forest model
install.packages("randomForest")
library(randomForest)
###################################################################
#model doesnt work! why???
model.rf <- randomForest(Revenue ~ ., data = shopping.train, importance = TRUE)

#when it works check precision and recall
#make a prediction on the test set
prediction <- predict(model.dt, shopping.test)
#build actual column
actual <-shopping.test$Revenue

#build the confusion matrix
cf <- table(actual,prediction >0.5)
precision <- cf['TRUE', 'TRUE']/(cf['TRUE', 'TRUE']+cf['TRUE', 'FALSE'])
recall <- cf['TRUE', 'TRUE']/(cf['TRUE', 'TRUE']+cf['FALSE', 'TRUE'])
###################################################################

#ROC curve
install.packages("pROC")
library(pROC)

#create ROC curve for the tree prediction
rocCurveDC <- roc(actual, prediction, direction = ">", levels = c("TRUE","FALSE"))
#create ROC curve for the random forest tree
rocCurveDC <- roc(actual, prediction.rf, direction = ">", levels = c("TRUE","FALSE"))

#plot the chart
plot(rocCurveDC, col = 'red', main = "ROC Chart")
par(new = TRUE)
plot(rocCurveRF, col = 'blue', main = "ROC Chart")


