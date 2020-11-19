## TP 1 INSTRUCTIONS 

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
# https://www.lovelyanalytics.com/2018/11/12/regles-dassociation-avec-r/

