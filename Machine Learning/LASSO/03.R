setwd("~/Documents/MSBA/Machine Learning/HW03")

#Load in data
train <- read_csv("Documents/MSBA/Machine Learning/HW03/train.csv")

#Size of dataframe 
dat <- read.csv("train.csv")
dim(dat)
dat[1:2,]
dat <- dat[,-1]
dim(dat)

attach(dat)
quantile(loss)
quantile(loss, p=seq(0,1,.01))
sum(loss < 100)
loss[loss<100]


dat <- dat[dat$loss>=100,]
dim(dat)
loss <- dat$loss
hist(log(loss))
dat$loss <- log(loss)
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)

#Split Train and Test set
smp_size <- floor(0.2 * nrow(dat))

## set the seed to make your partition reproducible
set.seed(456)
train_ind <- sample(seq_len(nrow(dat)), size = smp_size)

train_set <- dat[train_ind, ]
test <- dat[-train_ind, ]

# set.seed(652)
# train_set <- runif(nrow(dat)) < 0.2
# #table(train_set)
# train_set <- dat[train_set==TRUE,]
# test <- dat[train_set==FALSE,]
dim(train_set); dim(test)

#Run OLS and display model statistics 
r0 <- lm(loss~., data=train_set)
summary(r0)

Y <- train_set$loss
Y.test <- test$loss

do.RMSE.train_set <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.test <- function(yhat)  sqrt( mean( (Y.test-yhat)^2 ) )

RMSE.trn_OLS <- do.RMSE.train_set(predict(r0, data = train_set))
RMSE.test_OLS <- do.RMSE.test(predict(r0, data = test))
RMSE.trn_OLS; RMSE.test_OLS

Average <- mean(Y)

SSR <- sum(((predict(r0, data = train_set) - Average))^2 )

SST <- sum((Y - Average)^2 )

R2 <- SSR/SST

#Using the LASSO Regression 
install.packages("glmnet")
library(glmnet)

#Convert partitions in to matrices for lasso regression
X <- model.matrix(loss ~. , dat)
X <- X[,-1]

#Partition again 
set.seed(652)
train_ind <- sample(seq_len(nrow(X)), size = smp_size)

X_train <- X[train_ind, ]
Y_train <- dat[train_ind,ncol(dat)]
#X_train <- X_train[,-ncol(X_train)]
X_test <- X[-train_ind, ]
Y_test <- dat[-train_ind,ncol(dat)]
#X_test <- X_test[,-ncol(X_test)]

#Run LASSO Regresssion
lasso_mod <- glmnet(X_train, Y_train,family = "gaussian", alpha = 1, standardize = TRUE, nlambda=10)

plot(lasso_mod, lwd = 1 , xvar = "lambda")

#This can be accomplished for the LASSO -- and for many other ML methods -- via cross validation
cv_lasso <- cv.glmnet(X_train, Y_train, alpha = 1, family="gaussian",k=5) #Default is LOOCV
#Let's illustrate:
plot(cv_lasso)

#So cv gives us the model -- specified by lambda -- with the smallest cross-validated error
lambda_lasso <- cv_lasso$lambda.min
lambda_lasso

lasso_best <- glmnet(X_train, Y_train, family = "gaussian", alpha = 1, lambda = lambda_lasso, standardize = TRUE)

 Y <- Y_train
 Y.test <- Y_test

 RMSE.trn_LASSO <- do.RMSE.train_set(predict(lasso_best, newx = X_train, type = "response"))
 RMSE.test_LASSO <- do.RMSE.test(predict(lasso_best, newx = X_test, type = "response"))
 RMSE.trn_LASSO; RMSE.test_LASSO


#Calculate in sample R^2
Average <- mean(Y)
SSR <- sum(((predict(lasso_best, newx = X_train,
                             type = "response") - Average))^2 )
SST <- sum((Y - Average)^2 )
train_R2 <- SSR/SST

#Calculate out of sample R^2
Average <- mean(Y.test)
SSR <- sum(((predict(lasso_best, newx = X_test,
                     type = "response") - Average))^2 )
SST <- sum((Y.test - Average)^2 )
test_R2 <- SSR/SST


