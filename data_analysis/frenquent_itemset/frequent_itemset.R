##########
# Author: Markos Viggiato
##########

library(arules)


## android

dir_android <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/android")), stringsAsFactors = FALSE)
full_android <- data.frame(matrix(ncol = 3, nrow = 0))


for(i in 1:length(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`)){
      data <- read.table(paste0(getwd(),"/commit_history/android/", dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i]), header = TRUE, sep = ',')
      data <- data[,7:9]
      full_android <- rbind(full_android, data)
}

# number of commits
print(nrow(full_android))

for(i in 1:ncol(full_android)) full_android[[i]] <- as.logical(as.integer(as.character(full_android[[i]])))


############## APRIORI algorithm to find frequent itemset

tData <- as (full_android, "transactions") # convert to 'transactions' class

size(head(tData)) # number of items in each observation
LIST(head(tData, 3))
inspect(head(tData, 3))

frequentItems <- apriori(tData, parameter = list(target = "frequent itemsets",
                                                 support = 0.05))
inspect(frequentItems)
itemFrequencyPlot(tData, topN=3, type="absolute", main="Item Frequency") # plot frequent items


############## APRIORI algorithm to find association rules

rules <- apriori (tData, parameter = list(supp = 0.001, conf = 0.8)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules



############################################################################################################

## desktop

dir_desktop <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/desktop")), stringsAsFactors = FALSE)
full_desktop <- data.frame(matrix(ncol = 3, nrow = 0))

for(i in 1:length(dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`)){
      data <- read.table(paste0(getwd(),"/commit_history/desktop/", dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`[i]), header = TRUE, sep = ',')
      data <- data[,7:9]
      full_desktop <- rbind(full_desktop, data)
}

#number of commits
print(nrow(full_desktop))

for(i in 1:ncol(full_desktop)) full_desktop[[i]] <- as.logical(as.integer(as.character(full_desktop[[i]])))

############## ECLAT or APRIORI algorithm to find frequent itemset

tData <- as (full_desktop, "transactions") # convert to 'transactions' class

size(head(tData)) # number of items in each observation
LIST(head(tData, 3))
inspect(head(tData, 3))

frequentItems <- apriori(tData, parameter = list(target = "frequent itemsets",
                                                 support = 0.05))
inspect(frequentItems)
itemFrequencyPlot(tData, topN=3, type="absolute", main="Item Frequency") # plot frequent items


############## APRIORI algorithm to find association rules

rules <- apriori (tData, parameter = list(supp = 0.001, conf = 0.8)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules


