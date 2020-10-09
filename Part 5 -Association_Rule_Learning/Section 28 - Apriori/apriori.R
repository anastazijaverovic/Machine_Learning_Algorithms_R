# Apriori Algorithm

# Data Preprocessing
dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# Sparse Matrix
#install.packages('arules')
library(arules)

sMatrix = read.transactions('Market_Basket_Optimisation.csv', 
                            sep = ',', 
                            rm.duplicates = TRUE)
summary(sMatrix)

# Frequency Plot
itemFrequencyPlot(x = sMatrix, topN = 10)

# Training Apriori on the dataset
# support of a product purchased 3 times a day = 3*7/total number of transactions
# confidence = 0.8, 0.4 or 0.2
# support = 0.003 or 0.004 (product purchased 4 times a day)

rules = apriori(data = sMatrix, 
                parameter = list(support = 0.003, 
                                 confidence = 0.2))

# Visualising the rules descending

inspect(sort(rules,by = 'lift')[1:10])



