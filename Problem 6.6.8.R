# a)
set.seed(1)
X <- rnorm(100)
noise <- rnorm(100)

# b)
Y <- 3 + 1*X + 4*X^2 - 1*X^3 + noise

# c)
require(leaps)
df <- data.frame(Y, X)
fit <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)

fit_summary <- summary(fit)

require(tidyverse)
require(ggplot2)
require(ggthemes);
library(magrittr) 
library(dplyr)    

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           AdjR2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)
# Similar to how we specified the Y variable, Regsubsets determines that three parameters is the ideal number.

# d)
require(caret)

model_back <- train(Y ~ poly(X, 10), data = df, 
                    method = 'glmStepAIC', direction = 'backward', 
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_back, df), df$Y)
#      RMSE  Rsquared       MAE 
# 0.9314956 0.9569843 0.7488821 


summary(model_back$finalModel)
# The backward stepwise model agrees with the best subsets model.

x_poly <- poly(df$X, 10)

colnames(x_poly) <- paste0('poly', 1:10)
model_forw <- train(y = Y, x = x_poly,
                    method = 'glmStepAIC', direction = 'forward',
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_forw, data.frame(x_poly)), df$Y)
#      RMSE  Rsquared       MAE 
# 0.9314956 0.9569843 0.7488821 
summary(model_forw$finalModel)
# The forward stepwise model also agrees.

# e)
lasso_model <- train(Y ~ poly(X, 10), data = df,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_model)

plot(varImp(lasso_model))

coef(lasso_model$finalModel, lasso_model$bestTune$lambda)

postResample(predict(lasso_model, df), df$Y)
#      RMSE  Rsquared       MAE 
# 0.9265197 0.9577192 0.7566424 
# The amount of predictors required is overstated by the Lasso model. This may be predicted given that we choose the best model solely based on RSS rather than the Aikake Information Criterion or the Bayesian Inference Criterion as used by regsubsets or the Bayesian Inference Criterion and Adjusted R2 as used by stepwise selection.

# f)
Y_7 <- 3 + 8*X^7 + noise
df_2 <- data_frame(Y_7 = Y_7, X = df[,-1])

fit <- regsubsets(Y_7 ~ poly(X, 10), data = df_2, nvmax = 10)

fit_summary <- summary(fit)

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           R2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

lasso_y7_model <- train(Y_7 ~ poly(X, 10), data = df_2,
                        method = 'glmnet', 
                        trControl = trainControl(method = 'cv', number = 5),
                        tuneGrid = expand.grid(alpha = 1, 
                                               lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_y7_model)

plot(varImp(lasso_y7_model))

coef(lasso_y7_model$finalModel, lasso_y7_model$bestTune$lambda)
# 11 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)     36.72505
# poly(X, 10)1  2630.24239
# poly(X, 10)2  1059.42846
# poly(X, 10)3  3491.14380
# poly(X, 10)4   597.14287
# poly(X, 10)5  1363.96802
# poly(X, 10)6    70.93052
# poly(X, 10)7   177.79560
# poly(X, 10)8     .      
# poly(X, 10)9     .      
# poly(X, 10)10    .      

postResample(predict(lasso_y7_model, df_2), df_2$Y_7)
#       RMSE   Rsquared        MAE 
# 14.2854375  0.9996164  4.9531386 
