# K-Means Clustering

# Data Preprocessing

# Importing the Mall_Customers dataset
dataset = read.csv('Mall_Customers.csv')

x <- dataset[4:5]

# K-means

# Elbow method - finding the optimal number of clusters

set.seed(6)

wcss <- vector()

for(i in 1:10)
  wcss[i] <- sum(kmeans(x, i)$withinss)

plot(x = 1:10, 
     y = wcss, 
     type = 'b', 
     main = 'Clusters of clients',
     xlab = 'Number of clusters',
     ylab = 'WCSS')

k = 
  
# Applying K-means to the x dataset

set.seed(29)
kmeans <- kmeans(x = x,
                 centers = k,
                 iter.max = 300,
                 nstart = 10)

# Visualising the clusters

library('cluster')

clusplot(x = x,
         clus = kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of clients'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

