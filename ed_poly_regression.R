# Polynomial Regression

# Import the Dataset
dataset = read.csv('Position_Salaries.csv')

# Select the columns
dataset = dataset[2:3]

# Split the dataset
# Not needed
# No scaling 

# Fitting the Linear Regression model
regressor_linear = lm(formula = Salary ~ .,
                      data = dataset)

dataset$Level2 = dataset$Level ^ 2
dataset$Level3 = dataset$Level ^ 3
dataset$Level4 = dataset$Level ^ 4
regressor_polynomial = lm(formula = Salary ~ .,
                          data = dataset)

#Visualize
ggplot() +
  geom_point(aes(x= dataset$Level, y =dataset$Salary),
             color='red') +
  geom_line(aes(x=dataset$Level, y=predict(regressor_linear, newdata = dataset)),
            color = 'blue') +
  ggtitle('Truth or Bluff (linear regression)') +
  xlab('Level') +
  ylab('Salary')

ggplot() +
  geom_point(aes(x= dataset$Level, y =dataset$Salary),
             color='red') +
  geom_line(aes(x=dataset$Level, y=predict(regressor_polynomial, newdata = dataset)),
            color = 'blue') +
  ggtitle('Truth or Bluff (polynomial regression)') +
  xlab('Level') +
  ylab('Salary')

