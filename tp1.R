## TP 1 INSTRUCTIONS (Version:19/11/20)

## Exercice a : apriori
# Follow instructions here : 
## https://www.datacamp.com/community/tutorials/market-basket-analysis-r

# download data here : http://archive.ics.uci.edu/ml/datasets/online+retail


print("*** START TP 1 ***")

# please uncomment install packages first time for installing the required packages

#install and load packages
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install.packages("knitr")
library(knitr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
#install.packages("plyr")
library(plyr)
library(dplyr)

# change it according to your path
# Change your_folder by your project folder
your_folder <- "D:/projects/datavalue_fstatadd2/tp1_exercice_b_data"
setwd(your_folder)


#read excel into R dataframe
retail <- read_xlsx('c://xaviertemp//datavalue_fstatsadd2/tp1_exercice_b_data/Online Retail.xlsx')


#complete.cases(data) will return a logical vector indicating which rows have no missing values.
# Then use the vector to get only rows that are complete using retail[,].
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

# NAs introduced by coercion” 
# The reason for this is that some of the character strings are not properly formatted numbers 
# and hence cannot be converted to the numeric class.


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

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written
#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')
View(tr)

summary(tr)

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

# Generating association rules
# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)

# Print only 10 first assoc rules
inspect(association.rules[1:10])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))

# Removing redundant rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 3913


subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

# Finding Rules related to given items
# For example, to find what customers buy before buying 'METAL' run the following line of code:
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))

# Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items
inspect(head(metal.association.rules))

# Similarly, to find the answer to the question Customers who bought METAL also bought.... you will keep METAL on lhs:
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))

## Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items
inspect(head(metal.association.rules))

## Visualizing Association Rules
# Scatter plot (nuage de points)
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

# with options
plot(subRules,method="two-key plot")

# Interactive Scatter-Plot
plotly_arules(subRules)

# Graph-Based Visualizations
# Let's select 10 rules from subRules having the highest confidence.
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# From arulesViz graphs for sets of association rules can be exported in the GraphML format or as a Graphviz
# dot-file to be explored in tools like Gephi. For example, the 1000 rules with the highest lift are exported by:
saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")

# Individual Rule Representation
# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")


# ---"*** END TP1 *** "
