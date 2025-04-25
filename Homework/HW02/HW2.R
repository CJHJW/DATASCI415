setwd("/Users/jiatao/Desktop/UMich/25Winter/DATASCI415/Homework/HW02")

# 1.1
college <- read.csv("College.csv")
college

# 1.2
rownames(college) = college[,1]
college = college[,-1]
View(college)

# 1.3
summary(college)
pairs(college[,2:11])
college$Private <- factor(college$Private)
plot(college$Outstate ~ college$Private,
     xlab = "Private College",
     ylab = "College Outstate",
     main = "Outstate against Private")
Elite = rep("No", nrow(college))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
college$Elite <- factor(college$Elite)
plot(college$Outstate ~ college$Elite,
     xlab = "Elite College",
     ylab = "College Outstate",
     main = "Outstate against Elite")
par(mfrow = c(2,2))
hist(college$PhD, breaks = 50, main = "Histogram of PhD (50 bins)", xlab = "PhD")
hist(college$PhD, breaks = 30, main = "Histogram of PhD (30 bins)", xlab = "PhD")
hist(college$PhD, breaks = 10, main = "Histogram of PhD (10 bins)", xlab = "PhD")
hist(college$PhD, breaks = 5, main = "Histogram of PhD (5 bins)", xlab = "PhD")
summary(lm(college$Accept~college$Apps))

# 2.1
library(ISLR)
model <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(model)z
model1 <- lm(Sales ~ Price +  US, data=Carseats)
summary(model1)
