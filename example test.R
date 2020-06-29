setwd("C:/R/rstudio")

#importing the packages and libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("")

#Q1
#read the data file into R 
customers.raw <- read.csv("churn.csv")

#create a data frame
customers.prepared <- customers.raw

