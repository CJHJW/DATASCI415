library(ISLR2)
data("Weekly")
head(Weekly)

# Problem 1

## (a)
model_a <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(model_a)

## (b)
model_b <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
summary(model_b)

## (c)
first <- Weekly[1, ]
prob <- predict(model_b, newdata = first, type = "response")
prob


## (d)
n <- nrow(Weekly)

errors <- rep(NA, n)

for (i in 1:n) {
  
  train_data <- Weekly[-i, ]
  
  model <- glm(Direction ~ Lag1 + Lag2, data = train_data, family = binomial)
  
  prob <- predict(model, newdata = Weekly[i, ], type = "response")
  
  pred <- ifelse(prob > 0.5, "Up", "Down")
  
  actual <- as.character(Smarket$Direction[i])
  errors[i] <- ifelse(pred != actual, 1, 0)
}

## (e)
error_rate <- mean(errors)
print(paste("LOOCV Error Rate:", round(error_rate, 3)))

# Problem 2
set.seed(123)
data("Boston")
head(Boston[, c('dis', 'nox')])

## (a)

model_poly3 <- lm(nox ~ poly(dis, 3), data = Boston)

summary(model_poly3)

plot(Boston$dis, Boston$nox, main = "Cubic Polynomial: nox vs dis",
     xlab = "dis", ylab = "nox", pch = 18, col = "red")

dis_values <- seq(min(Boston$dis), max(Boston$dis), length.out = 100)
predicted_nox <- predict(model_poly3, newdata = data.frame(dis = dis_values))

lines(dis_values, predicted_nox, col = "blue", lwd = 2)

## (b)
x <- Boston$dis
y <- Boston$nox

rss <- numeric(10)

for (d in 1:10) {
  model <- lm(nox ~ poly(dis, d), data = Boston)
  rss[d] <- sum(residuals(model)^2)
}

plot(1:10, rss, type = "b", pch = 19,
     xlab = "Polynomial Degree", ylab = "RSS",
     main = "RSS vs Polynomial Degree")

plot(x, y, main = "Polynomial Degrees 1 to 10",
     xlab = "dis", ylab = "nox", col = "grey", pch = 20)

dis_grid <- seq(min(x), max(x), length.out = 300)

colors <- rainbow(10)

for (d in 1:10) {
  model <- lm(nox ~ poly(dis, d), data = Boston)
  pred <- predict(model, newdata = data.frame(dis = dis_grid))
  lines(dis_grid, pred, col = colors[d], lwd = 1.5)
}

legend("topright", legend = paste("Degree", 1:10), col = colors, lwd = 5, cex = 0.8)

## (c)
library(boot)

cv_error <- numeric(10)

for (d in 1:10) {
  model <- glm(nox ~ poly(dis, d), data = Boston)
  cv_result <- cv.glm(Boston, model, K = 10)
  cv_error[d] <- cv_result$delta[1]
  print(cv_error[d])
}

optimal_degree <- which.min(cv_error)
optimal_degree

## (d)
library(splines)

model_spline <- lm(nox ~ bs(dis, df = 4), data = Boston)

summary(model_spline)

plot(Boston$dis, Boston$nox, col = "gray", pch = 20,
     main = "Regression Spline Fit (df = 4)", xlab = "dis", ylab = "nox")

dis_grid <- seq(min(Boston$dis), max(Boston$dis), length.out = 300)
pred_spline <- predict(model_spline, newdata = data.frame(dis = dis_grid))

lines(dis_grid, pred_spline, col = "blue", lwd = 2)

## (e)
x <- Boston$dis
y <- Boston$nox

rss_values <- numeric(8)
dfs <- 3:10

plot(x, y, col = "gray", pch = 20,
     main = "Regression Spline Fits (df = 3 to 10)",
     xlab = "dis", ylab = "nox")


x_grid <- seq(min(x), max(x), length.out = 300)

colors <- rainbow(length(dfs))

for (i in seq_along(dfs)) {
  df_val <- dfs[i]
  model <- lm(nox ~ bs(dis, df = df_val), data = Boston)
  
  rss_values[i] <- sum(residuals(model)^2)
  
  y_pred <- predict(model, newdata = data.frame(dis = x_grid))
  lines(x_grid, y_pred, col = colors[i], lwd = 1.5)
}

legend("topright", legend = paste("df =", dfs), col = colors, lwd = 3, cex = 0.8)

plot(3:10, rss_values, type = "b", pch = 19,
     xlab = "Spline Df", ylab = "RSS",
     main = "RSS vs Spline Df")

## (f)
library(boot)
set.seed(123)
cv.error_df <- rep(3, 10)
for(i in 3:10) {
  df_val <- i
  fit <- glm(nox ~ bs(dis, df = df_val), data = Boston)
  cv.error_df[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
  print(cv.error_df[i])
}
which.min(cv.error_df)

