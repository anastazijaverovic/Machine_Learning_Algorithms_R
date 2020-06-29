# Classification Decision Tree Plotting

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# no Feature Scaling when plotting the decision tree

# Fitting Decision Tree classifier to the Training set
library(rpart)

#option + N = ~
classifier = rpart(formula = Purchased ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3],type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Plotting Decision Tree

plot(classifier)
text(classifier)  #to plot with written conditions

