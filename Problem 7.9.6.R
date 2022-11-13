# a)
library(ISLR)
library(boot)
set.seed(1)
degree <- 10
cv.errs <- rep(NA, degree)
for (i in 1:degree) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}

plot(1:degree, cv.errs, xlab = 'Degree', ylab = 'Test MSE', type = 'l')
deg.min <- which.min(cv.errs)
points(deg.min, cv.errs[deg.min], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
age.range <- range(Wage$age)
age.grid <- seq(from = age.range[1], to = age.range[2])
fit <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# b)
cv.errs <- rep(NA, degree)
for (i in 2:degree) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}

plot(2:degree, cv.errs[-1], xlab = 'Cuts', ylab = 'Test MSE', type = 'l')
deg.min <- which.min(cv.errs)
points(deg.min, cv.errs[deg.min], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

