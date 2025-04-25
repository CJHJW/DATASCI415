library(MASS)

X_train <- c(-3, -2, 0, 1, -1, 2, 3, 4, 5)
Y_train <- factor(c(-1, -1, -1, -1, 1, 1, 1, 1, 1))
training_data <- data.frame(X = X_train, Y = Y_train)

lda_model <- lda(Y ~ X, data = training_data)

lda_pred <- predict(lda_model)$class
lda_error <- mean(lda_pred != Y_train)
lda_error

qda_model <- qda(Y ~ X, data = training_data)

qda_pred <- predict(qda_model)$class
qda_error <- mean(qda_pred != Y_train)
qda_error

X_test <- c(-1.5, -1, 0, 1, 0.5, 1, 2.5, 5)
Y_test <- factor(c(-1, -1, -1, -1, 1, 1, 1, 1))
test_data <- data.frame(X = X_test, Y = Y_test) 

lda_pred <- predict(lda_model, test_data)$class
lda_error <- mean(lda_pred != Y_test)
lda_error

qda_pred <- predict(qda_model, test_data)$class
qda_error <- mean(qda_pred != Y_test)
qda_error

library(ISLR)
data(Auto)
median_mpg <- median(Auto$mpg)
Auto$mpg01 <- ifelse(Auto$mpg > median_mpg, 1, 0)

boxplot(cylinders ~ mpg01, data=Auto,
        main = "Boxplot of Cylinders by mpg_01",
        xlab = "mpg_01",
        ylab = "Cylinders",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$cylinders)

boxplot(displacement ~ mpg01, data=Auto,
        main = "Boxplot of Displacement by mpg_01",
        xlab = "mpg_01",
        ylab = "Displacement",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$displacement)

boxplot(horsepower ~ mpg01, data=Auto,
        main = "Boxplot of horsepower by mpg_01",
        xlab = "mpg_01",
        ylab = "horsepower",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$horsepower)

boxplot(weight ~ mpg01, data=Auto,
        main = "Boxplot of weight by mpg_01",
        xlab = "mpg_01",
        ylab = "weight",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$weight)

boxplot(acceleration ~ mpg01, data=Auto,
        main = "Boxplot of acceleration by mpg_01",
        xlab = "mpg_01",
        ylab = "acceleration",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$acceleration)

boxplot(year ~ mpg01, data=Auto,
        main = "Boxplot of year by mpg_01",
        xlab = "mpg_01",
        ylab = "year",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$year)

boxplot(origin ~ mpg01, data=Auto,
        main = "Boxplot of origin by mpg_01",
        xlab = "mpg_01",
        ylab = "origin",
        col = c("blue", "green"))
plot(x=Auto$mpg, y=Auto$origin)

Auto_mpg <- data.frame(mpg01 = Auto$mpg01, weight = Auto$weight)

library(caTools)

set.seed(123)

split <- sample.split(Auto_mpg$mpg01, SplitRatio = 0.7)

train_set <- subset(Auto_mpg, split == TRUE)
test_set <- subset(Auto_mpg, split == FALSE)

lda_model <- lda(mpg01 ~ weight, data = train_set)

lda_pred <- predict(lda_model, test_set)$class
lda_error <- mean(lda_pred != test_set$mpg01)
lda_error

qda_model <- qda(mpg01 ~ weight, data = train_set)

qda_pred <- predict(qda_model, test_set)$class
qda_error <- mean(qda_pred != test_set$mpg01)
qda_error

log_model <- glm(mpg01 ~ weight, data = train_set, family = binomial)
summary(log_model)

log_probs <- predict(log_model, newdata = test_set, type = "response")
log_pred <- ifelse(log_probs > 0.5, 1, 0)

log_error <- mean(log_pred != test_set$mpg01)
log_error

library(class)
train_X <- scale(train_set$weight)  
test_X <- scale(test_set$weight)   
train_Y <- train_set$mpg01      
test_Y <- test_set$mpg01

K_values <- 1:20

test_errors <- numeric(length(K_values))

for (i in K_values) {
  pred_Y <- knn(train = train_X, test = test_X, cl = train_Y, k = K_values[i])
  test_errors[i] <- mean(pred_Y != test_Y)
}

data.frame(Errors = test_errors, K = K_values)

which.min(test_errors)
min(test_errors)

