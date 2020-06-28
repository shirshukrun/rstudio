setwd("C:/Users/שיר/Desktop/שנה ד/כריית ידע ולמידת מכונה/R files")
house.raw <- read.csv("kc_house_data.csv")
houses.prepared <- house.raw

str(houses.prepared)

#remove non informative columns
houses.prepared$id <- NULL
houses.prepared$date <- NULL

#EDA- Exploratory Data Analysis

#בהתחלה כדאי לבדוק איך משתנה המטרה מתפלג
ggplot(houses.prepared, aes(price)) + geom_histogram(binwidth = 100000)

#start working on the features
table(houses.prepared$bedrooms)

#מהטבלה רואים שיש בתים מסוימים שאינם רלוונטיים לסטטיסטיקות שנבצע 
#נוכל להכריח אותם להתאחד לעמודה אחת, כי אחרת הן לא יתרמו לנו למודל
limit.bedrooms <- function(beds){
  if (beds == 0) return(1)
  if (beds>7) return(7)
  return(beds)
}

houses.prepared$bedrooms <- sapply(houses.prepared$bedrooms, limit.bedrooms)
table(houses.prepared$bedrooms)

#see how bedrooms affect prices
ggplot(data = houses.prepared, aes(bedrooms,price)) + geom_point() + stat_smooth(method = lm)



#bathrooms
table(houses.prepared$bathrooms)

limit.bathrooms <- function(baths){
  if(baths < 1) return(1)
  if(baths > 4.5) return(5)
  return(baths)
}
houses.prepared$bathrooms <- sapply(houses.prepared$bathrooms, limit.bathrooms)
table(houses.prepared$bathrooms)
ggplot(houses.prepared, aes(bathrooms,price)) + geom_point() + stat_smooth(method = lm)

#sqft living
ggplot(houses.prepared, aes(sqft_living, price)) + geom_point() + stat_smooth(method = lm)

#sqft lot
ggplot(houses.prepared, aes(sqft_lot, price)) + geom_point() + stat_smooth(method = lm)

#floors
table(houses.prepared$floors)

limit.floors <- function(floors){
  if(floors>2.5) return(2.5)
  return(floors)
}

houses.prepared$floors <- sapply(houses.prepared$floors, limit.floors)
table(houses.prepared$floors)
ggplot(houses.prepared, aes(floors,price)) + geom_point() + stat_smooth(method = lm)

#waterfront
ggplot(houses.prepared, aes(as.factor(waterfront), price)) + geom_boxplot()

str(houses.prepared)

#view
table(houses.prepared$view)
ggplot(houses.prepared, aes(as.factor(view), price)) + geom_boxplot()

#condition
table(houses.prepared$condition)
ggplot(houses.prepared, aes(as.factor(condition), price)) + geom_boxplot()

#grade
table(houses.prepared$grade)

limit.grade <- function(grade){
  if(grade<5) return(5)
  if(grade>11) return(11)
  return(grade)
}
houses.prepared$grade <- sapply(houses.prepared$grade, limit.grade)
table(houses.prepared$grade)
ggplot(houses.prepared, aes(as.factor(view), price)) + geom_boxplot()

#sqfr above
table(houses.prepared$sqft_above)
ggplot(houses.prepared, aes(sqft_above, price)) + geom_point() + stat_smooth(method = lm)

#sqft basement
table(houses.prepared$sqft_basement)
ggplot(houses.prepared, aes(sqft_basement, price)) + geom_point() + stat_smooth(method = lm)

#yr_built
ggplot(houses.prepared, aes(yr_built, price)) + geom_point() + stat_smooth(method = lm)
ggplot(houses.prepared, aes(yr_built)) + geom_histogram()

#yr_renovated
ggplot(houses.prepared, aes(yr_renovated)) + geom_histogram()
table(houses.prepared$yr_renovated)

limit.renovated <- function(year){
  if(year>2000) return(0)
  return(1)
}
houses.prepared$yr_renovated <- sapply(houses.prepared$yr_renovated, limit.renovated)
table(houses.prepared$yr_renovated)
ggplot(houses.prepared, aes(as.factor(yr_renovated),price)) + geom_boxplot()

#remove the original col
houses.prepared$yr_renovated <- NULL

#zipcode
houses.prepared$zipcode <- as.factor(houses.prepared$zipcode)
str(houses.prepared)
ggplot(houses.prepared, aes(as.factor(zipcode), price)) + geom_boxplot()

#lat
ggplot(houses.prepared, aes(lat, price)) + geom_point() + stat_smooth(method = lm)

#long
ggplot(houses.prepared, aes(long, price)) + geom_point() + stat_smooth(method = lm)
houses.prepared$long <- NULL

#sqft_lot15
ggplot(houses.prepared, aes(sqft_lot15, price)) + geom_point() + stat_smooth(method = lm)

houses.prepared$sqft_lot15 <- NULL
houses.prepared$sqft_living15 <- NULL
str(houses.prepared)

#seperating into train and test
library(caTools)

filter <- sample.split(houses.prepared$price, SplitRatio = 0.7)

#training set
houses.train <- subset(houses.prepared, filter==T)

#test set
houses.test <- subset(houses.prepared, filter==F)

dim(houses.prepared)
dim(houses.train)
dim(houses.test)

model <- lm(price ~ ., houses.train)
summary(model)

?predict
predict.train <- predict(model, houses.train)
predict.test <- predict(model, houses.test)

MSE.train <- mean((houses.train$price - predict.train)**2)
MSE.test <- mean((houses.test$price - predict.test)**2)

RMSE.train <- MSE.train**0.5
RMSE.test <- MSE.test**0.5

#אחרי שבדקנו א ת השגיאה המרובעת אנו מבינים שאין פה אובר פיטינג, אולי אנדר פיטינג בגלל 

predict.all <- predict(model, houses.prepared)

houses.prepared$predict <- predict.all
ggplot()+ 
  geom_line(data = houses.prepared, aes(sqft_living, price), color = 'red') + 
  geom_line(data = houses.prepared, aes(sqft_living, predict), color = 'blue')
