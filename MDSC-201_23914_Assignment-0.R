# Assignment-1
# Implementing Linear Regression on Datasets

#Dataset-1: Walmart Data

#Importing the Dataset
walmart = read.csv("Walmart.csv")

# Preprocessing Required
walmart = subset(walmart, select = -c(Date)) # Dropping the feature "Date" as it is not relevant.

# Simple Visulization
# Checking whether the feature "Weekly_Sales" is normally distributed
install.packages("ggplot2")
library(ggplot2)

ggplot(walmart, aes(x=Weekly_Sales)) +
  geom_histogram(aes(y=..density..),
                 colour='black', fill='white') +
  geom_density(alpha=.2, fill='#FF6666')
# Conclusion: It is skewed and can be normalized.

# Splitting the Dataset into Training and Testing set
install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(walmart$Weekly_Sales, SplitRatio = 0.8)
print(split)

walmart_training_set = subset(walmart, split== TRUE)
walmart_test_set = subset(walmart, split == FALSE)

# Regressor for the Walmart Dataset
walmart_regressor = lm(formula = Weekly_Sales ~ ., data = walmart_training_set)
print(walmart_regressor)

# Prediction
walmart_pred = predict(walmart_regressor, newdata = walmart_test_set)

print(walmart_pred)
print(walmart_test_set$Weekly_Sales)

# Dataset-2: Housing Price

# Importing the Dataset
housing = read.csv("Housing.csv")

#Preprocessing Required
# Features like "mainroad", "guestroom", "basement", 
# "hotwaterheating", "airconditioning", "prefarea", 
# "furnishingstatus" need to be encoded from "yes" and "no" 
# to 1s and 0s.

encoded_furnishingstatus <- model.matrix(~furnishingstatus-1, data=housing)
housing <- cbind(housing, encoded_furnishingstatus)
housing <- subset(housing, select = -c(furnishingstatus))

housing$mainroad <- ifelse(housing$mainroad == "yes", 1, 0)
housing$guestroom <- ifelse(housing$guestroom == "yes", 1, 0)
housing$basement <- ifelse(housing$basement == "yes", 1, 0)
housing$hotwaterheating <- ifelse(housing$hotwaterheating == "yes", 1, 0)
housing$airconditioning <- ifelse(housing$airconditioning == "yes", 1, 0)
housing$prefarea <- ifelse(housing$prefarea == "yes", 1, 0)
# Now the Dataset is ready

# Splitting the Dataset into Training and Testing set
split = sample.split(housing$price, SplitRatio = 0.8)

housing_training_set = subset(housing, split== TRUE)
housing_test_set = subset(housing, split == FALSE)

# Regressor for the Housing Dataset
housing_regressor = lm(formula = price ~ ., data = housing_training_set)
print(housing_regressor)

# Prediction
housing_pred = predict(housing_regressor, newdata = housing_test_set)

print(housing_pred)
print(housing_test_set$price)