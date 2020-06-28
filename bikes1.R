setwd("C:/R/rstudio")
bikes.data.raw <- read.csv("bike sharing.csv")

#important to check the str of the df
str(bikes.data.raw)
library(ggplot2)

#we work on different df to not damage the original data
bikes.data.prepared <- bikes.data.raw

#to change kind of column:
#bikes.data.prepared$datetime <- as.character(bikes.data.prepared$datetime)
#then check with str

#we want to tell the system that we only want to use English
Sys.setlocale("LC_TIME", "English")
date <- substr(bikes.data.prepared$datetime, 1, 10)#2020-00-00
days <- weekdays(as.Date(date))

bikes.data.prepared$weekday <- as.factor(days)
str(bikes.data.prepared)

#draw the month and define it
month <- as.integer(substr(date, 6, 7))

bikes.data.prepared$month <- month

#draw the year and define it
year <- as.integer(substr(date, 1, 4))
bikes.data.prepared$year <- year

str(bikes.data.prepared)

hour <- as.integer(substr(bikes.data.prepared$datetime, 12, 13))
bikes.data.prepared$hour <- hour

#find seasons
table(bikes.data.prepared$season, bikes.data.prepared$month)

#turn season into factor
bikes.data.prepared$season <- factor(bikes.data.prepared$season, levels = 1:4, labels = c('winter', 'spring', 'summer', 'automn'))
str(bikes.data.prepared)




#homework solution
bikes.data.prepared$weather <- factor(bikes.data.prepared$weather, levels = 1:4, labels = c("Clear", "Cloudy", "Light Rain", "Heavy Rain"))

#check if it turned to a factor
str(bikes.data.prepared)                                         

#box plot- to show how weather affects count
ggplot(bikes.data.prepared, aes(weather,count)) + geom_boxplot()

#show how season affects count
ggplot(bikes.data.prepared, aes(season, count)) + geom_boxplot()

str(bikes.data.prepared)

#look at time impact
ggplot(bikes.data.prepared, aes(as.factor(year), count)) + geom_boxplot()
ggplot(bikes.data.prepared, aes(as.factor(month), count)) + geom_boxplot()
ggplot(bikes.data.prepared, aes(weekday, count)) + geom_boxplot()
ggplot(bikes.data.prepared, aes(as.factor(hour), count)) + geom_boxplot()

#binning- הפחתה של מספר המשתנים, deviding the time data to early morning, morning, launch, evening...
#כל שניתן למודל יותר נתונים כך יהיה לנו סיכון גדול יותר של אוברפיטינגת overfitting
#how to do it with R tools

#binning hour
breaks <- c(0,7,8,9,17,20,21,22,24)
labels <- c("0-6", "7", "8", "9-16", "17-19", "20", "21", "22-23")
bins <- cut(bikes.data.prepared$hour, breaks, include.lowest = T, right = F, labels = labels)

#add to df
bikes.data.prepared$hourb <- bins
str(bikes.data.prepared)
ggplot(bikes.data.prepared, aes(hourb, count)) + geom_boxplot()

#how temperature affects count
ggplot(bikes.data.prepared, aes(temp, count)) + geom_point() + stat_smooth(method = lm)

#how wind affects count
ggplot(bikes.data.prepared, aes(windspeed, count)) + geom_point() + stat_smooth(method = lm)

#how humidity affects count
ggplot(bikes.data.prepared, aes(humidity, count)) + geom_point() + stat_smooth(method = lm)


str(bikes.data.prepared)

#נבטל את כל השדות שאנו לא רוצים להשתמש בהם
bikes.data.final <- bikes.data.prepared

bikes.data.final$datetime <- NULL
bikes.data.final$season <- NULL
bikes.data.final$atemp <- NULL
bikes.data.final$casual <- NULL
bikes.data.final$registered <- NULL
bikes.data.final$hour <- NULL

str(bikes.data.final)

#כאן הקובץ מוכן לעבודת המודל

#devid to test set and training set
install.packages("caTools")
library(caTools)

filter <- sample.split(bikes.data.final$weekday, SplitRatio = 0.7)

#training set
bikes.train <- subset(bikes.data.final, filter==T)

#test set
bikes.test <- subset(bikes.data.final, filter==F)

dim(bikes.data.final)
dim(bikes.train)
dim(bikes.test)

#creating the model
model <- lm(count ~ ., bikes.train)

#רוצים לבחון את המודל
summary(model)

#חיזוי מתבצע על ידי הפונקציה פרדיקט
predict.train <- predict(model, bikes.train)
#המספרים שמקבלים הם החיזוי
predict.test <- predict(model, bikes.test)

#############################################################
install.packages('lubridate')
library(lubridate)

#see prediction per day
#נעשה זאת על כל הנתונים לא רק על האימון או המבחן
predicted.all <- predict(model, bikes.data.final)

#עכשיו רוצים לראות על גרף את התוצאות שהמודל נותן מול התוצאות באמיתיות
bikes.analysis <- bikes.data.final

bikes.analysis$predicted <- predicted.all

#אנו רוצים לראות כמה הוא מדויק בחיזוי של יום שלם
datetime <- bikes.data.prepared$datetime
bikes.analysis$date <- as.Date(datetime) 

bikes.analysis$month.year <- floor_date(bikes.analysis$date, 'month') 

bikes.month <- bikes.analysis %>% group_by(month.year) %>% 
  summarize(actual = sum(count), predicted = sum(predicted))

ggplot() + 
  geom_line(data = bikes.month, aes(month.year, actual), color = 'red') + 
  geom_line(data = bikes.month, aes(month.year, predicted), color = 'blue')







