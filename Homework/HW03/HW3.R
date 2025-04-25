setwd("/Users/jiatao/Desktop/UMich/25Winter/DATASCI415/Homework/HW03")

library(ISLR)
model <- lm(Sales ~ ., data=Carseats)
summary(model)
reduce <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age, data=Carseats)
summary(reduce)
anova_result <- anova(reduce, model)
anova_result
interact <- lm(Sales ~ CompPrice + Income + Advertising + Price * ShelveLoc + Age, data=Carseats)
summary(interact)
anova(reduce, interact)
