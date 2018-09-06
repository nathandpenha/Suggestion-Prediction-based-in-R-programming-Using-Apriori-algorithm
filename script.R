## from link
## https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/

library(dplyr)
library(plyr)
library(arules)
library(arulesViz)
library(plumber)
library(jsonlite)
library(colorspace)
library(ggplot2)

#' @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}
raw_data <- read.csv("~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/Raw Data.csv")
data <- read.csv("~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/Sample - Superstore.csv")

#' Retun the summary from a particular category
#' @param val the category
#' @post /category
function(val){
  df_cat <- data.frame(Name= data$Product.Name[which(data$Category == val)])
  toJSON(lapply(df_cat, function(x){as.list(summary(x))}), pretty = TRUE, auto_unbox = TRUE)
}

#' Retun total Records in the CSV after Cleaning
#' @get /totalRecords
function(){
  system("wc -l ~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/link.csv | cut -d' ' -f5", intern = T)
}

#' Total Products Sold
#' @get /totalProducts
function(){
  sum(summary(data$Product.Name))
}

#' Segement Pie Chart data
#' @get /segmentData
function(){
  df_seg <- data.frame(Name= data$Segment)
  toJSON(summary(df_seg), force = T)
}

#' Total Users Data
#' @get /users 
function(){
  length(unique(data$Customer.Name))
}

#' Total Profit 
#' @get /profit
function(){
  round(sum(raw_data$Profit))
}

#' lolipop char -> profit
#' requires ggplot2
#' @png
#' @get /profitChart
function(){
  xLoli <- c(paste(head(substr(raw_data$Product.Name, start = 1, stop = 10),7), sep = "\n"))
  yLoli <- c(head(raw_data$Profit,7))
  loli <- ggplot(head(data,7), aes(x=xLoli, y=yLoli)) +
    geom_segment( aes(x=xLoli, xend=xLoli, y=1, yend=yLoli), color="red") +
    geom_point( color="orange", size=4) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    xlab("") +
    ylab("Profit/Loss")
  print(loli)
}


function(){
  library(RColorBrewer)
  wordcloud::wordcloud(data$Product.Name, colors = brewer.pal(12, "Paired"), random.order = T)
}

data_2 <- head(data.frame(qty = raw_data$Quantity, product = paste(substr(raw_data$Product.Name, start = 1, stop = 10), sep = "\n")), 5)

plot(data_2, method = "paracoord", control = list(reorder = TRUE))

data_sorted <- data[order(data$Order.ID),]
data_sorted$Order.ID <- as.numeric(data_sorted$Order.ID)

data_item <- ddply(data, c("Order.ID"), function(dd)paste(dd$Product.Name, collapse = ","))
data_item$Order.ID <- NULL
write.csv(data_item,"~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/link.csv")
trans <- read.transactions("~/Suggestion-Prediction-based-in-R-programming-Using-Apriori-algorithm/link.csv", format = "basket", sep=",", cols=1)
trans@itemInfo$labels <- gsub("\"","", trans@itemInfo$labels)
basket_rules <- apriori(trans,parameter = list(supp = 0.001, minlen = 1, target = "frequent itemsets" ))
inspect(basket_rules)
summary(basket_rules)
#plot(basket_rules)
#arules::itemFrequencyPlot(trans, topN=5)
plot(basket_rules, method="graph", control=list(type="items"), measure = "support", shading = "lift")
#plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=F)
#pie(trans)

#plot_ly(data = raw_data, color = ~State , colors = brewer.pal(12, "Paired"), x= ~Sales, y= ~Profit, type = "scatter", mode = "markers")
