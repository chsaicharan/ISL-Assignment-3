# a)
set.seed(1)
require(MASS); require(tidyverse); require(ggplot2); require(ggthemes)
require(broom); require(knitr); require(caret)
theme_set(theme_tufte(base_size = 14) + theme(legend.position = 'top'))
data('Boston')

model <- lm(nox ~ poly(dis, 3), data = Boston)
tidy(model) %>%
  kable(digits = 3)

Boston %>%
  mutate(pred = predict(model, Boston)) %>%
  ggplot() +
  geom_point(aes(dis, nox, col = '1')) +
  geom_line(aes(dis, pred, col = '2'), size = 1.5) +
  scale_color_manual(name = 'Value Type',
                     labels = c('Observed', 'Predicted'),
                     values = c('#56B4E9', '#E69F00'))


# b)
errors <- list()
models <- list()
pred_df <- data_frame(V1 = 1:506)
for (i in 1:9) {
  models[[i]] <- lm(nox ~ poly(dis, i), data = Boston)
  preds <- predict(models[[i]])
  pred_df[[i]] <- preds
  errors[[i]] <- sqrt(mean((Boston$nox - preds)^2))
}

errors <- unlist(errors)

names(pred_df) <- paste('Level', 1:9)
data_frame(RMSE = errors) %>%
  mutate(Poly = row_number()) %>%
  ggplot(aes(Poly, RMSE, fill = Poly == which.min(errors))) +
  geom_col() + 
  guides(fill = FALSE) +
  scale_x_continuous(breaks = 1:9) +
  coord_cartesian(ylim = c(min(errors), max(errors))) +
  labs(x = 'Polynomial Degree')

Boston %>%
  cbind(pred_df) %>%
  gather(Polynomial, prediction, -(1:14)) %>%
  mutate(Polynomial = factor(Polynomial, 
                             levels = unique(as.character(Polynomial)))) %>%
  ggplot() + 
  ggtitle('Predicted Values for Each Level of Polynomial') +
  geom_point(aes(dis, nox, col = '1')) + 
  geom_line(aes(dis, prediction, col = '2'), size = 1.5) +
  scale_color_manual(name = 'Value Type',
                     labels = c('Observed', 'Predicted'),
                     values = c('#56B4E9', '#E69F00')) +
  facet_wrap(~ Polynomial, nrow = 3)

# c)
errors <- list()

folds <- sample(1:10, 506, replace = TRUE)
errors <- matrix(NA, 10, 9)
for (k in 1:10) {
  for (i in 1:9) {
    model <- lm(nox ~ poly(dis, i), data = Boston[folds != k,])
    pred <- predict(model, Boston[folds == k,])
    errors[k, i] <- sqrt(mean((Boston$nox[folds == k] - pred)^2))
  }
}

errors <- apply(errors, 2, mean)

data_frame(RMSE = errors) %>%
  mutate(Poly = row_number()) %>%
  ggplot(aes(Poly, RMSE, fill = Poly == which.min(errors))) +
  geom_col() + theme_tufte() + guides(fill = FALSE) +
  scale_x_continuous(breaks = 1:9) +
  coord_cartesian(ylim = range(errors))
# The model with polynomial degree 4 is chosen for testing on out-of-sample data. When we plot different polynomial powers, we can observe that this is the maximum degree that does not exhibit overfitting in a blatant manner.

# d)
require(splines)
require(knitr)
library(broom)
model <- lm(nox ~ bs(dis, df = 4), data = Boston)

kable(tidy(model), digits = 3)

library(ggthemes)
Boston %>%
  mutate(pred = predict(model)) %>%
  ggplot() +
  geom_point(aes(dis, nox, col = '1')) + 
  geom_line(aes(dis, pred, col = '2'), size = 1.5) +
  scale_color_manual(name = 'Value Type',
                     labels = c('Observed', 'Predicted'),
                     values = c('#56B4E9', '#E69F00')) +
  theme_tufte(base_size = 13)
# The model determines that each of the various bases is statistically significant. The prediction line appears to accurately represent the data without being overfit.

# e)
errors <- list()
models <- list()
pred_df <- data_frame(V1 = 1:506)
for (i in 1:9) {
  models[[i]] <- lm(nox ~ bs(dis, df = i), data = Boston)
  preds <- predict(models[[i]])
  pred_df[[i]] <- preds
  errors[[i]] <- sqrt(mean((Boston$nox - preds)^2))
}

names(pred_df) <- paste(1:9, 'Degrees of Freedom')
data_frame(RMSE = unlist(errors)) %>%
  mutate(df = row_number()) %>%
  ggplot(aes(df, RMSE, fill = df == which.min(errors))) +
  geom_col() + guides(fill = FALSE) + theme_tufte() +
  scale_x_continuous(breaks = 1:9) +
  coord_cartesian(ylim = range(errors))

Boston %>%
  cbind(pred_df) %>%
  gather(df, prediction, -(1:14)) %>%
  mutate(df = factor(df, levels = unique(as.character(df)))) %>%
  ggplot() + ggtitle('Predicted Values for Each Level of Polynomial') +
  geom_point(aes(dis, nox, col = '1')) + 
  geom_line(aes(dis, prediction, col = '2'), size = 1.5) +
  scale_color_manual(name = 'Value Type',
                     labels = c('Observed', 'Predicted'),
                     values = c('#56B4E9', '#E69F00')) +
  facet_wrap(~ df, nrow = 3)
# when trained and tested on the same data, a model with very high complexity is deemed the best.

# f)
folds <- sample(1:10, size = 506, replace = TRUE)
errors <- matrix(NA, 10, 9)
models <- list()
for (k in 1:10) {
  for (i in 1:9) {
    models[[i]] <- lm(nox ~ bs(nox, df = i), data = Boston[folds != k,])
    pred <- predict(models[[i]], Boston[folds == k,])
    errors[k, i] <- sqrt(mean((Boston$nox[folds == k] - pred)^2))
  }
}

errors <- apply(errors, 2, mean)

data_frame(RMSE = errors) %>%
  mutate(df = row_number()) %>%
  ggplot(aes(df, RMSE, fill = df == which.min(errors))) +
  geom_col() + theme_tufte() + guides(fill = FALSE) +
  scale_x_continuous(breaks = 1:9) +
  coord_cartesian(ylim = range(errors))
# A simpler model is adopted after validation on out-of-sample data. Similar to the polynomial validation, this model is the most complicated one that does not exhibit overfitting in an obvious manner.