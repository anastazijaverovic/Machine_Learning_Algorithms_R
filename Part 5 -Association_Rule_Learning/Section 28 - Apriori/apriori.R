# Apriori Algorithm

# Data Preprocessing
dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# Sparse Matrix
install.packages('arules')
library(arules)

sMatrix = read.transactions('Market_Basket_Optimisation.csv', 
                            sep = ',', 
                            rm.duplicates = TRUE)
summary(sMatrix)

# Frequency Plot
itemFrequencyPlot(x = sMatrix, topN = 10)

# Training Apriori on the dataset
# support of a product purchased 3 times a day = 3*7/total number of transactions
rules = apriori(data = sMatrix, 
                parameter = list(support = 0.003, 
                                 confidence = 0.8))
