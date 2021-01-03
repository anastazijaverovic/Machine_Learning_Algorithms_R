#PCA

#Data Preprocessing

# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Wine.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools) #used for splitting the dataset

set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling

training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# PCA
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

pca = preProcess(x = training_set[-14],
                 method = 'pca',
                 pcaComp = 2)

training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]

test_set = predict(pca, test_set)
test_set = test_set[c(2,3,1)]

# Fitting SVM to Training set

classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising Training set results

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid('PC1' = X1,
                       'PC2' = X2)

y_grid = predict(classifier, newdata = grid_set)

plot (set[, -3],
      main = 'SVM (Training set)',
      xlib = 'PC1',
      ylib = 'PC2',
      xlim = range(X1),
      ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2), add = TRUE))
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'deepskyblue', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising Test set results (prediction)

set = test_set

X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid('PC1' = X1,
                       'PC2' = X2)

y_grid = predict(classifier, newdata = grid_set)

plot (set[, -3],
      main = 'SVM (Test set)',
      xlib = 'PC1',
      ylib = 'PC2',
      xlim = range(X1),
      ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2), add = TRUE))
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'deepskyblue', ifelse(set[, 3] == 1, 'green4', 'red3')))


