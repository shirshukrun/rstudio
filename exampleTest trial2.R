churn.raw <- read.csv('churn.csv')
churn <- churn.raw

str(churn)
summary(churn)

#id attrubute provides no information 
churn$customerID <- NULL

table(churn.raw$SeniorCitizen)
#Make SeniorCitizen a factor 
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

#Question 2

#How tenior affects chrun 
#install.packages('ggplot2')
library(ggplot2)
ggplot(churn, aes(tenure, fill = Churn)) + geom_bar()
ggplot(churn, aes(tenure, fill = Churn)) + geom_bar(position = 'fill')

#remove tenior 0 from the dataset 
# Customers never churn on the when thye just begin  
filter <- churn$tenure == 0

churn <- churn[!filter,]

#does gender affect churn?
ggplot(churn, aes(gender, fill = Churn)) + geom_bar()
#almost no effect on churn 


#does contract affect churn? 
ggplot(churn, aes(Contract, fill = Churn)) + geom_bar()
ggplot(churn, aes(Contract, fill = Churn)) + geom_bar(position='fill')
#A big impact Nonth-to-Month customers tend to churn much more than one year
#And two year customers 

#precision and recall for PaperlessBilling
?table
cf <- table(churn$PaperlessBilling, churn$Churn)

precision <- cf['Yes','Yes']/(cf['Yes','Yes'] + cf['Yes','No'] )
recall <- cf['Yes','Yes']/(cf['Yes','Yes'] + cf['No','Yes'] )

#divide into trainig set and test set 
library(caTools)

filter <- sample.split(churn$Partner , SplitRatio = 0.7)

churn.train <- subset(churn, filter ==T)
churn.test <- subset(churn, filter ==F)


dim(churn.train)
dim(churn.test)

install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)






