# Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')

# we don't need descriptive column
dataset <- dataset[2:3]

# too small dataset to split it
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Comparison of Linear and Polynomial models:

# Fitting Linear Regression model to a dataset

lin_regressor = lm(formula = Salary ~ Level,
                   data = dataset)

# Fitting Polynomial model to a dataset
# Adding polynomial features

# squared level column

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

poly_regressor = lm(formula = Salary ~ .,
                    data = dataset)

# Visualising Linear Regression model results

#install.packages("ggplot2")
#library(ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_regressor, newdata = dataset)),
            colour = 'blue') +  #predictions
  ggtitle('Linear Regression observations and prediction') +
  xlab('Level') +
  ylab('Salary')
  
# Visualising Polynomial Regression model results

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_regressor, newdata = dataset)),
            colour = 'blue') +  #predictions
  ggtitle('Polynomial Regression observations and prediction') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result of a single level with Linear Regression

y_pred = predict(lin_regressor, data.frame(Level = 6.5))

# Predicting a new result of a single level with Polynomial Regression

y_pred = predict(poly_regressor, data.frame(Level = 6.5,
                                            Level2 = 6.5^2,
                                            Level3 = 6.5^3,
                                            Level4 = 6.5^4
                                            ))

