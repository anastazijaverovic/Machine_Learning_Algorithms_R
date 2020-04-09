#Logistic Regression

#Data Preprocessing

# Data Preprocessing Template

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

# Fitting Logistic Regression to the Training set

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results

# predicted probabilities vector
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])

# predicted values vector
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix

cm = table(test_set[, 3], y_pred)

# Visualising Training set results

set = training_set

expand.grid('Age' = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01),
            'EstimatedSalary' = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01))%>%
  mutate(prob_set=predict(classifier, type = 'response', newdata = .),
         y_grid = ifelse(prob_set > 0.5, 1, 0))%>%
  
  ggplot()+
  geom_point(aes(x=Age, y=EstimatedSalary, color=ifelse(y_grid == 1,y_grid-0.2, -y_grid-0.2)))+
  geom_point(data=training_set, aes(x=Age, y=EstimatedSalary,colour=as.numeric(Purchased)+0.5))


# Visualising Test set results

set = test_set

expand.grid('Age' = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01),#taking the range of the training set observation points for Age and Salary
            'EstimatedSalary' = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01))%>%
  mutate(prob_set=predict(classifier, type = 'response', newdata = .),#predicting the result of the graph using the classifier
         y_grid = ifelse(prob_set > 0.5, 1, 0))%>%#transforming the predictions
  
  ggplot()+
  geom_point(aes(x=Age, y=EstimatedSalary, color=ifelse(y_grid == 1,y_grid-0.2, -y_grid-0.2)))+
  geom_point(data=test_set, aes(x=Age, y=EstimatedSalary,colour=as.numeric(Purchased)+0.5))


