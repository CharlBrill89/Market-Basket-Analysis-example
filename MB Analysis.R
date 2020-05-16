#Market Basket Analysis model example 
#Credit: https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce

#Install required packages
install.packages("arules")
install.packages("arulesViz")

#Load required packages
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

#Dataset path
setwd("C:/Users/Charl/Documents/R/Market Basket Analysis")
getwd()

#Import and Read the Dataset
retail = read_excel("Online Retail.xlsx", sheet = "Online Retail")

#Setup some formatting and data cleansing
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp

tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

#Lets check out the list of items bought within each basket
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
summary(tr)

#Analyse the distribution of frequently bought items 
itemFrequencyPlot(tr, topN=20, type='absolute')

#Now we apply the Association Rules library to mine frequent itemsets using apriori 
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]

#Lets check out the list of items bought within each basket
#write.csv(rules,"top_Rules.csv", quote = FALSE, row.names = FALSE)

plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")

plot(topRules, engine = "plotly")