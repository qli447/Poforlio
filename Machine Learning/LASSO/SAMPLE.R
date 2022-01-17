dat <- read.csv("train.csv")
dim(dat)
dat[1:2,]
dat <- dat[,-1]
dim(dat)
#Preparing the data
#Let¡¯s commence by inspecting the outcome variable:
library(MASS)
loss <- dat$loss
hist(loss)
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)
#So the log-transformation makes the data much more amenable to regression, 
#although we still observe some deviations from the normal distribution for small claims.

quantile(loss)
quantile(loss, p=seq(0,1,.01))
sum(loss < 100)
loss[loss<100]
#So there are a few very small losses that are outliers. 
#We thus disregard losses that are very small and keep only records with loss greater or equal to $100, 
#also because we are interested in these in actual settings.

dat <- dat[dat$loss>=100,]

dim(dat)
loss <- dat$loss
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)
#Looks better.

#For simplicity, we consider a training data set that only consists of 20% of the data. Clearly, 
#that¡¯s not a typical choice in applications, 
#but we choose a small training set here so as to keep runtimes manageable (we only have four days :-)

set.seed(652)
trn <- runif(nrow(dat)) < 0.2
table(trn)
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
dim(train); dim(test)
#Predictive Modeling
#Our objective now is to build a prediction model that minimizes the error evaluated on the test set. 
#The mosty basic candidate is a linear regression that includes all covariates, 
#so let¡¯s run that for comparison purposes (we could also include a step-wise regression 
#via r1 <- stepAIC(r0, data=train) but it takes a very long time to run):
  
r0 <- lm(loss~., data=train)
summary(r0)

Y <- train$loss
Y.tst <- test$loss

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )

RMSE.trn_OLS <- do.RMSE.trn(predict(r0, data = train))
RMSE.tst_OLS <- do.RMSE.tst(predict(r0, data = test))
RMSE.trn_OLS; RMSE.tst_OLS

#Lasso Regression#
install.packages('glmnet')
library(glmnet)
Lasso_mod <-glmnet(X_train, Y_train, family='gaussian', alpha =1, standardize = TRUE,nalmbda=10)
#gaussian means a linear model, and as is evdent, we force standardiaztion (as we did not dot this)
#for simplicity we only use 10 lambda parameters

plot(Lasso_mod, lwd =3 ,xvar ='lambda')
coef(Lasso_mod)
print(Lasso_mod)

mse_train <-colMeans (replicate(10,Y_train)-predict(lasso_mod,))
