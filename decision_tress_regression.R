# Load the Dataset
dataset = read.csv('Position_Salaries')
dataset = dataset[2:3]

library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1)


# Predict a new result
y_pred <- predict(regressor, data.frame(Level = 6.5))