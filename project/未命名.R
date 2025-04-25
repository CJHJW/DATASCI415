setwd("/Users/jiatao/Desktop/UMich/25Winter/DATASCI415/project")

data <- read.csv("DATA.csv")

colnames(data)

hist(data$RIDAGEYR,
     main = "Histogram of Age",
     xlab = "Age (Years)",
     col = "skyblue",
     border = "white")

library(ggplot2)

data$gender <- factor(data$RIAGENDR, labels = c("Male", "Female"))
data$depressed <- factor(data$depressed, labels = c("Not Depressed", "Depressed"))

ggplot(data, aes(x = gender, fill = depressed)) +
  geom_bar(position = "fill") +  # use "fill" for proportion; use "dodge" for count
  labs(
    title = "Proportion of Depression by Race",
    x = "Gender",
    y = "Proportion",
    fill = "Depression Status"
  ) +
  scale_fill_manual(values = c("Not Depressed" = "skyblue", "Depressed" = "tomato")) +
  theme_minimal()

dim(data)
table(data$RIAGENDR)

data$race <- factor(data$RIDRETH1, labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other"))
ggplot(data, aes(x = race, fill = depressed)) +
  geom_bar(position = "fill") +  # use "fill" for proportion; use "dodge" for count
  labs(
    title = "Proportion of Depression by Gender",
    x = "Race",
    y = "Proportion",
    fill = "Depression Status"
  ) +
  scale_fill_manual(values = c("Not Depressed" = "skyblue", "Depressed" = "tomato")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(data$RIDRETH1)

data$US <- factor(data$DMDBORN4, labels = c("US", "Other"))
ggplot(data, aes(x = US, fill = depressed)) +
  geom_bar(position = "fill") +  # use "fill" for proportion; use "dodge" for count
  labs(
    title = "Proportion of Depression by Region",
    x = "US",
    y = "Proportion",
    fill = "Depression Status"
  ) +
  scale_fill_manual(values = c("Not Depressed" = "skyblue", "Depressed" = "tomato")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(data$DMDBORN4)

data$edu <- factor(data$DMDEDUC2, labels = c("1", "2", "3", "4", "5"))
ggplot(data, aes(x = edu, fill = depressed)) +
  geom_bar(position = "fill") +  # use "fill" for proportion; use "dodge" for count
  labs(
    title = "Proportion of Depression by Education Level",
    x = "Education",
    y = "Proportion",
    fill = "Depression Status"
  ) +
  scale_fill_manual(values = c("Not Depressed" = "skyblue", "Depressed" = "tomato")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(data$DMDEDUC2)

data$marriage <- factor(data$DMDMARTZ , labels = c("Married/Living with Partner", "Widowed/Divorced/Separated", "Never married"))
ggplot(data, aes(x = marriage, fill = depressed)) +
  geom_bar(position = "fill") +  # use "fill" for proportion; use "dodge" for count
  labs(
    title = "Proportion of Depression by Marriage",
    x = "Marriage",
    y = "Proportion",
    fill = "Depression Status"
  ) +
  scale_fill_manual(values = c("Not Depressed" = "skyblue", "Depressed" = "tomato")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(data$DMDMARTZ)

# RIAGENDR, RIDAGEYR, Lifestyle: ALQ111, ALQ121, ALQ151, DBQ700 , DBQ197, DBD895, DBD905, DBD910, SMQ020, 

