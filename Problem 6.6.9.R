# a)
require(ISLR)
require(caret)
require(tidyverse)
data('College')
set.seed(1)

inTrain <- createDataPartition(College$Apps, p = 0.75, list = FALSE)

training <- College[inTrain,]
testing <- College[-inTrain,]

preObj <- preProcess(training, method = c('center', 'scale'))

training <- predict(preObj, training)
testing <- predict(preObj, testing)

y_train <- training$Apps
y_test <- testing$Apps

one_hot_encoding <- dummyVars(Apps ~ ., data = training)
x_train <- predict(one_hot_encoding, training)
x_test <- predict(one_hot_encoding, testing)

# b)
lin_model <- lm(Apps ~ ., data = training)

pred <- predict(lin_model, testing)

(lin_info <- postResample(pred, testing$Apps))
#  RMSE       Rsquared       MAE 
#  0.2799768  0.9201765    0.1568743 

# c)
ridge_fit <- train(x = x_train, y = y_train,
                   method = 'glmnet', 
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 0,
                                          lambda = seq(0, 10e2, length.out = 20)))

(ridge_info <- postResample(predict(ridge_fit, x_test), y_test))

coef(ridge_fit$finalModel, ridge_fit$bestTune$lambda)

plot(ridge_fit)

plot(varImp(ridge_fit))


# d)
lasso_fit <- train(x = x_train, y = y_train, 
                   method = 'glmnet',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.0001, 1, length.out = 50)))

(lasso_info <- postResample(predict(lasso_fit, x_test), y_test))
# RMSE       Rsquared       MAE 
# 0.2914812 0.9141364 0.1511801 

coef(lasso_fit$finalModel, lasso_fit$bestTune$lambda)

plot(lasso_fit)

plot(varImp(lasso_fit))

