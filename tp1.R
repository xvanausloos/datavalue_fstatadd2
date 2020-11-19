## TP 1 INSTRUCTIONS (Version:19/11/20)

# Change your_folder by your project folder
your_folder <- "D:/projects/datavalue_fstatadd2"
setwd(your_folder)

install.packages("arules")
library("arules")


## Exercice a
# Load Groceries (available in R Lib)
data("Groceries")
Groceries

rules <- apriori(Groceries, parameter = list(support = .001))
rules

inspect(head(sort(rules, by = "lift"), 3))

## Exercice b 
# Follow instructions here : 
## https://www.datacamp.com/community/tutorials/market-basket-analysis-r

# download data here : http://archive.ics.uci.edu/ml/datasets/online+retail

#install and load packages
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
install.packages("knitr")
library(knitr)
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("plyr")
library(plyr)
library(dplyr)
install.packages()

# change it according to your path
setwd("D:/projects/datavalue_fstatadd2/tp1_exercice_b_data")


#read excel into R dataframe
retail <- read_xlsx('Online Retail.xlsx')

#complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
retail <- retail[complete.cases(retail), ]

# mutate function is from dplyr package. It is used to edit or add new columns to dataframe. 
# Here Description column is being converted to factor column. as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
retail %>% mutate(Description = as.factor(Description))

retail %>% mutate(Country = as.factor(Country))

#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)

#Extract time from InvoiceDate and store in another variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")

#Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

#Bind new columns TransTime and InvoiceNo into dataframe retail
cbind(retail,TransTime)

cbind(retail,InvoiceNo)

#get a glimpse of your data
glimpse(retail)

#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData
View(transactionData)


#set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL

#set column Date of dataframe transactionData
transactionData$Date <- NULL

#Rename column to items
colnames(transactionData) <- c("items")

#Show Dataframe transactionData
transactionData

