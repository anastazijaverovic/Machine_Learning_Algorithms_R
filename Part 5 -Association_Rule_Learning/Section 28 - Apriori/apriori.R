# Apriori Algorithm

# Data Preprocessing

dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# Sparse Matrix

install.packages('arules')
library(arules)

sMatrix = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(sMatrix)

# Frequency Plot

itemFrequencyPlot(x = sMatrix, topN = 10)