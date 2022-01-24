rm(list = ls())
library(glmnet)
# Read the dataset
dat <- read.csv("train.csv")
dim(dat)
dat[1:2,]
dat <- dat[,-1]
dim(dat)

# Prepare the dataset
loss <- dat$loss
hist(loss)
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)

quantile(loss)
quantile(loss, p=seq(0,1,.01))
sum(loss < 100)
loss[loss<100]

# dropping losses that are smaller than 100
dat <- dat[dat$loss>=100,]
dim(dat)
loss <- dat$loss
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)


# Test-train split
set.seed(652)
trn <- runif(nrow(dat)) < 0.2
table(trn)
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
dim(train); dim(test)

# OLS with all variables
r0 <- lm(loss~., data=train)
summary(r0)

Y <- train$loss
Y.tst <- test$loss

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )

RMSE.trn_OLS <- do.RMSE.trn(predict(r0, data = train))
RMSE.tst_OLS <- do.RMSE.tst(predict(r0, data = test))
RMSE.trn_OLS; RMSE.tst_OLS

# Lasso regression
X <- model.matrix(loss ~. , train)
X <- X[,-1]

X_train <- data.matrix(train[,c(1:130)])
Y_train <- train[,131]

X_test <- data.matrix(test[,c(1:130)])
Y_test <- test[,131]


lasso_mod <- glmnet(X_train, Y_train, family = "gaussian", alpha = 1, standardize = TRUE, nlambda=50)
plot(lasso_mod, lwd=3, xvar = "lambda")
coef(lasso_mod)

# the in-sample error of the different models:
dim(predict(lasso_mod, X_train))
mse_train <- colMeans((replicate(42, Y_train)-predict(lasso_mod,X_train))^2)
plot(mse_train,type = "o", lwd=3,col="blue",xlab="model complexity")
mse_test <- colMeans((replicate(42, Y_test)-predict(lasso_mod,X_test))^2)
lines(mse_test,type = "o", lwd=3,col="red")
mse_test

lasso_mod$lambda[41]


# Cross-validation to see if there is a better lambda
cv_lasso <- cv.glmnet(X_train, Y_train, alpha = 1, family="gaussian",k=5) #Default is LOOCV

plot(cv_lasso)

lambda_lasso <- cv_lasso$lambda.min
lambda_lasso

lasso_best <- glmnet(X_train, Y_train, family = "gaussian", alpha = 1, lambda = lambda_lasso, standardize = TRUE)
cor(predict(lasso_best,X_test),Y_test)^2
#So the R-square from best lasso is almost the same as the 6th model with lambda
