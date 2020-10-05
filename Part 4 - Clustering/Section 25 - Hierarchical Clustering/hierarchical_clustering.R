# Hierarchical Clustering

# Data Preprocessing

# Import data
dataset = read.csv("Mall_Customers.csv")
dataset = dataset[4:5]

# Find optimal number of clusters - Dendrogram

dendrogram = hclust(d = dist(dataset,method = "euclidean"),
                    method = "ward.D")

plot(dendrogram,
     main = paste("Dendrogram"),
     xlab = "Customers",
     ylab = "Euclidean distances")

# Fitting hierarchical clustering model to the Mall dataset

hc = hclust(d = dist(dataset,method = "euclidean"),
            method = "ward.D")

y_hc = cutree(tree = hc, k = 5)

# Visualising the clusters

library(cluster)

clusplot (dataset,
          y_hc,
          lines = 0,
          shade = TRUE,
          color = TRUE,
          labels = 2,
          plotchar = FALSE,
          span = TRUE,
          main = paste('Clusters of customers'),
          xlab = "Annual Income",
          ylab = "Spending Score")


  