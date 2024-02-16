# Assignment - 2
# Plotting the Residual Plots on different datasets

# Dataset - 1 : The Rocket propellant Data.csv

# Importing the Dataset
rocket = read.csv("The Rocket propellant Data.csv")
rocket = subset(rocket, select = -c(Observation))

# Splitting the Dataset into Training and Testing set
install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(rocket$Shear.strength, SplitRatio = 0.8)
print(split)

training_set = subset(rocket, split== TRUE)
test_set = subset(rocket, split == FALSE)

# Fitting the Multiple Linear Regression Model using Training Set

regressor = lm(formula = Shear.strength ~ Age.of.propellant, data = training_set)
print(regressor)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

print(y_pred)
print(test_set$Shear.strength)

# Residual Plot
install.packages("ggplot2")

library(ggplot2)

ggplot() + 
  geom_point(aes(x= training_set$Age.of.propellant,
                 y= training_set$Shear.strength),
             colour= "red") +
  geom_line(aes(x= training_set$Age.of.propellant, 
                 y= predict(regressor, newdata = training_set)),
             colour = "blue")

# Dataset - 2 : eucalypt_hardwoods.csv

# Importing the Dataset
eucalypt = read.csv("eucalypt_hardwoods.csv")

# Splitting the Dataset into Training and Testing set
split = sample.split(eucalypt$hardness, SplitRatio = 0.8)
print(split)

training_set = subset(eucalypt, split== TRUE)
test_set = subset(eucalypt, split == FALSE)

# Fitting the Multiple Linear Regression Model using Training Set

regressor = lm(formula = hardness ~ density, data = training_set)
print(regressor)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

print(y_pred)
print(test_set$hardness)

# Residual Plot
ggplot() + 
  geom_point(aes(x= training_set$density,
                 y= training_set$hardness),
             colour= "red") +
  geom_line(aes(x= training_set$density, 
                y= predict(regressor, newdata = training_set)),
            colour = "blue")