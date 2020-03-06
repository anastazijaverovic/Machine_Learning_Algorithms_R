# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_startups.csv')

# Encoding categorical data

dataset$State = factor(dataset$State,
                 levels = c('California','Florida','New York'),
                 labels = c(1,2,3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set with all independent variables

regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results

y_pred = predict(regressor, newdata = test_set)

# using only R.D.Spend variable to fit the model because it is the most significant

regressor_rd = lm(formula = Profit ~ R.D.Spend,
               data = training_set)

# Predicting the Test set results

y_pred_rd = predict(regressor_rd, newdata = test_set)


# Building the optimal model using Backward Elimination

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)

summary(regressor)

# remove the variable with the  biggest p-value = State
# fit the model without removed variable

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)

summary(regressor)

#to-do: get r value of each predictor in regressor
#and name of each predictor from regressor

for(r in regressor$coefficients) 
{
  if(summary(regressor)$r.squared  < 0.05){
    print("Predictor can stay:",regressor$coefficients)
  }
  else{
    print("Predictor must go:",regressor$coefficients)
  }
}


