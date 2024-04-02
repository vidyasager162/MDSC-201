# Question-1
#a
songs = read.csv("question1.csv")
corr = cor(songs$Months, songs$Songs)
print(corr)
#c
install.packages("ggplot2")
library(ggplot2)

ggplot() +
  geom_point((aes(x= songs$Months, y= songs$Songs, colour= "red"))) +
  geom_abline(intercept = -12.88728849, slope= 21.12638093, linetype= "solid", color= "blue")

ggtitle("Months vs #Songs") +
  xlab("Months") +
  ylab("Songs")

#Question-2
#a
gre = read.csv("question2.csv")
regressor = lm(formula = GGPA ~ ., data = gre)
print(regressor)
#b
pvalues = summary(regressor)
pvalues$coefficients[2:3,4]
