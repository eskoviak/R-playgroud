# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Not needed 
# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Do a linear fit
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# What is in my future?
y_pred = predict(regressor, newdata = test_set)

# I'm from Missouri, show me!
# install.packages('ggplot2')
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary v. Experience (Training Set)') +
  xlab('Experience (yrs)') +
  ylab('Salary ($)')

# Test Set
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary v. Experience (Test Set)') +
  xlab('Experience (yrs)') +
  ylab('Salary ($)')