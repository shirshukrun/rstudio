#lesson 5
library(ggplot2)
library(dplyr)

#Data frame 

df.raw <- read.csv(url('https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv'))

df <- df.raw

#build column chart for mort rate
countries <- df %>% distinct(Country)

interesting.countries <- c('Belgium', 'Denmark', 'France', 'Germany', 'Greece', 'Israel', 'Italy', 'Japan', 'Sweden', 'Switzerland', 'United Kingdom', 'US')

df.int.countries <- df %>% filter(Country %in% interesting.countries)

#now filter only for today
today <- as.Date('2020-05-01')

df.int.countries.today <- df.int.countries %>% filter(Date==today)
str(df.int.countries.today)

df$Mort.Rate <- df$Deaths/(df$Confirmed + 1)

#make a column chart going down ("-")
ggplot(df.int.countries.today, aes(reorder(Country,-Mort.Rate), Mort.Rate)) + 
  geom_col()


#working directory, read&save files
setwd("C:/Users/שיר/Desktop/שנה ד/כריית ידע ולמידת מכונה/R files")

write.csv(df.int.countries.today, file = 'corone 01-05.csv')

Countries.age <- read.csv('corone 01-05.csv')
str(Countries.age)

ggplot(Countries.age, aes(Med.Age, Mort.Rate)) + geom_point() + 
  geom_text(aes(label = Country), hjust = 0, vjust = 0)

#הפונקציה לוקחת את כל התאריכים ולכל תאריך מסכמים את כל המאומתים של כל המדינות
df.aggregate <- df %>%
                group_by(Date) %>%
                summarize(total = sum(Confirmed))


str(df.aggregate)
df.aggregate$Date <- as.Date(df.aggregate$Date)

#adding a difference column
#חישוב ההפרשים בין כל יום לקודמו
day.diff <- function(date, df){
  today.df <- df %>% filter(Date == date)
  today <- today.df$total
  yesterday.df <- df %>% filter(Date == date - 1)
  if(nrow(yesterday.df) == 0) return(0)
  yesterday <- yesterday.df$total
  return(today - yesterday)
}

diff <- sapply(df.aggregate$Date, day.diff, df = df.aggregate)
df.aggregate$diff <- diff

ggplot(df.aggregate, aes(Date, diff)) + geom_area()





