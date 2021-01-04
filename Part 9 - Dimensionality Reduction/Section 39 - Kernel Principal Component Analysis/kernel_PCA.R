# Kernel PCA

#Logistic Regression

#Data Preprocessing

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools) #used for splitting the dataset

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling

training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Applying Kernel PCA
install.packages('kernlab')
library(kernlab)

k_pca = kpca(x = ~.,
             data = training_set[-3],
             kernel = 'rbfdot',
             features = 2)

training_set_pca = as.data.frame(predict(k_pca, training_set))

training_set_pca$Purchased = training_set$Purchased


test_set_pca = as.data.frame(predict(k_pca, test_set))

test_set_pca$Purchased = test_set$Purchased


# Fitting Logistic Regression to the Training set

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set_pca)

# Predicting the Test set results

# predicted probabilities vector
prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])

# predicted values vector
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix

cm = table(test_set_pca[, 3], y_pred)
accuracy = (57 + 26)/(10+7+57+26)

# Visualising Training set results
library(dplyr)
library(ggplot2)

set = training_set_pca

expand.grid('V1' = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01),
            'V2' = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01))%>%
  mutate(prob_set=predict(classifier, type = 'response', newdata = .),
         y_grid = ifelse(prob_set > 0.5, 1, 0))%>%
  ggplot()+
  geom_point(aes(x=V1, y=V2, color=ifelse(y_grid == 1,y_grid-0.2, -y_grid-0.2)))+
  geom_point(data=training_set_pca, aes(x=V1, y=V2,colour=as.numeric(Purchased)+0.5))


# Visualising Test set results

set = test_set_pca

expand.grid('Age' = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01),#taking the range of the training set observation points for Age and Salary
            'EstimatedSalary' = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01))%>%
  mutate(prob_set=predict(classifier, type = 'response', newdata = .),#predicting the result of the graph using the classifier
         y_grid = ifelse(prob_set > 0.5, 1, 0))%>%#transforming the predictions
  
  ggplot()+
  geom_point(aes(x=V1, y=V2, color=ifelse(y_grid == 1,y_grid-0.2, -y_grid-0.2)))+
  geom_point(data=test_set_pca, aes(x=V1, y=V2,colour=as.numeric(Purchased)+0.5))


