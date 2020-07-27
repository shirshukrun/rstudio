#setwd("C:/R/rstudio")

#import
loan.raw <- read.csv("loans.csv")

#to find the structure of the file features
str(loan.raw)

#find basic statistics about the file features (min,median,mean,max 1st & 3rd)
summary(loan.raw)

loan.prepared <- loan.raw 

#remove unnecessary features
loan.prepared$Loan_ID <- NULL

#table of the feature
table(loan.prepared$Loan_Status)

#gender
table(loan.prepared$Gender)
#change "" into 'Male' 
loan.prepared$Gender <- as.character(loan.prepared$Gender)



