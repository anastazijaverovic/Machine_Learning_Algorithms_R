# Eclat Association Rule Learning

# Data Preprocessing

library(arules)
dataset = read.csv("Market_Basket_Optimisation.csv")

sparseMatrix = read.transactions("Market_Basket_Optimisation.csv", sep = ",", rm.duplicates = TRUE)

# Fitting the model to the sparseMatrix

rules = eclat(data = sparseMatrix,
              parameter = list(support = 0.004,minlen = 2))


# Visualise the results

inspect(sort(rules,by = "support")[1:10])