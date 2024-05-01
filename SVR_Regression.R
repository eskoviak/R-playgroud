# Regression Template

# Import the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Splitting the dataate (as applicable)
# needs catools
# install(caTools)
# Not needed for this case

# Feature Scaling
# 

# Fitting the Regression Model (SVR)
# Requires pacakge 'e1071'
# install.package('e1071')
library(e1071)
regressor = svm( formula = Salary ~ .,
                 data = dataset,
                 type = 'eps-regression')

# Predict value for 6.5
y_pred = predict(regressor, data.frame(Level = 6.5))


# Plot the results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset $Level, y=predict(regressor, newdata = dataset)),
            color = 'blue') +
  ggtitle('Salary v. Experience (SVR)') +
  xlab('Experience (yrs)') +
  ylab('Salary ($)')