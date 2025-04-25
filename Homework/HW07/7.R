setwd("/Users/jiatao/Desktop/UMich/25Winter/DATASCI415/Homework/HW07")

train <- read.csv("spam-train.txt", header = FALSE)
test <- read.csv("spam-test.txt", header = FALSE)

library(tree)
train$V58 <- as.factor(train$V58)
tree.train = tree(V58~., train)
summary(tree.train)

Spam.test= test$V58
tree.pred = predict(tree.train, test ,type="class")
table(tree.pred,Spam.test)
(78 + 62) / (854 + 78 + 62 + 540)
78 / (78 + 540)
62 / (854 + 62)

set.seed(7)
cv.spam = cv.tree(tree.train, FUN=prune.misclass)
#names(cv.spam)
#cv.spam
#par(mfrow=c(1,2))
#plot(cv.spam$size,cv.spam$dev / length(train),ylab="cv error", xlab="size",type="b")
#plot(cv.spam$k, cv.spam$dev / length(train),ylab="cv error", xlab="k",type="b")
prune.spam = prune.misclass(tree.train, best=8)
par(mfrow=c(1,1))
plot(prune.spam)
text(prune.spam,pretty=0)


library(randomForest)
rf <- randomForest(
  V58~., # Model formula
  data=train, # Training data
  mtry=ncol(train)-1, # Use all columns
  importance=TRUE) # Return feature importance measures

rf_preds <- predict(rf, newdata=test)
table(rf_preds, Spam.test)
(20 + 33) / (883 + 20 + 33 + 598)
33 / (883 + 33)
20 / (598 + 20)
df <- as.data.frame(importance(rf))
df$Variable <- row.names(df)
df_sorted <- df[order(df$MeanDecreaseGini, decreasing = TRUE), ]
head(df_sorted, 10)
varImpPlot(rf, n.var=10)
