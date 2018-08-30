## for subsetting - got plans for this xD
file[which(file$Category=="Furniture"),]

## from link
## https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/
data <- read.csv("~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/Sample - Superstore.csv")
data_sorted <- data[order(data$Order.ID),]
data_sorted$Order.ID <- as.numeric(data_sorted$Order.ID)
library(dplyr)
library(plyr)
library(arules)
library(arulesViz)
data_item <- ddply(data, c("Order.ID"), function(dd)paste(dd$Product.Name, collapse = ","))
data_item$Order.ID <- NULL
write.csv(data_item,"~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/link.csv")
trans <- read.transactions("~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/link.csv", format = "basket", sep=",", cols=1)
trans@itemInfo$labels <- gsub("\"","", trans@itemInfo$labels)
basket_rules <- apriori(trans,parameter = list(sup = 0, conf = 0.0009, maxlen = 10000, maxtime = 30 , target="rules"))
inspect(basket_rules)
summary(basket_rules)
plot(basket_rules)
arules::itemFrequencyPlot(trans, topN=5)
plot(basket_rules, method="graph", control=list(type="items"), measure = "support", shading = "lift")
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=F)
pie(trans)
