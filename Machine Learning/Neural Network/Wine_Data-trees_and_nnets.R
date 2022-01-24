rm(list=ls())
library(psych)
library(pROC)

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
describe(dat)[,1:9]
head(dat)

#	Defining high quality wines
table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

set.seed(42)

# Scaling the data
X<-scale(dat[,1:11])
tst<-1:463
X.trn <- X[-tst,]
X.tst <- X[tst,]
Y.trn <- dat[-tst,12]
Y.tst <- dat[tst,12]
dat.train <- dat[-tst,]
dat.test <- dat[tst,]


# Random Forest
library(randomForest)
attach(dat)
set.seed(42)
rf <- randomForest(quality ~ ., data=dat.train)
summary(rf)
cor(predict(rf,dat.test),Y.tst)^2

rf2 <- randomForest(quality ~ ., ntree=1000,  data=dat.train)
summary(rf2)
cor(predict(rf2,dat.test),Y.tst)^2

yhat.rf <- predict(rf, dat.test)
rf.roc<-roc(Y.tst, yhat.rf,direction="<")
rf.roc

table(dat.test$quality, (yhat.rf > 0.5))


# Gradient Boosting
library(xgboost)
parm <- list(nthread=2, max_depth=2, eta=0.10)
bt <- xgboost(parm,data=X.trn, label=Y.trn, verbose=2, nrounds=10)
cor(predict(bt,X.tst),Y.tst)^2

yhat.bt <- predict(bt, X.tst)
bt.roc<-roc(Y.tst, yhat.bt,direction="<")
bt.roc

table(dat.test$quality, (yhat.bt > 0.5))

# Neural Network
library(nnet)
set.seed(123)
f <- formula(quality ~ fixed.acidity + volatile.acidity + citric.acid   + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)
n1 <- nnet(f, data=dat,linout=TRUE, size = 15, maxit = 750, decay=5)
yhat_n1 <- predict(n1, dat.test)
cor(yhat_n1,Y.tst)^2

n1.roc <- roc(dat.test$quality, yhat_n1, direction = '<')
n1.roc

# Comparing the ROC curves of the 3 models:
plot(n1.roc, lwd=3)
lines(rf.roc, lwd=3, col = "red")
lines(bt.roc, lwd=3, col = "blue")
legend("bottomright",title="ROC Curves",c("nerual net","random forest","gradient boosting"), fill=c("black","red","blue"))

# Deep Learning 
library(neuralnet)

dat[,1:11]  <- scale(dat[,1:11])
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
train2 <- dat[trn==TRUE,]
test2 <- scale(test[,1:11])

n2 <- neuralnet(f, data=train2, hidden = c(2,2,2,2),
                linear.output = FALSE)
yhat_n2 <- compute(n2, test2)
test$yhat.n2 <- yhat_n2$net.result

n2.roc <- roc(test$quality, test$yhat.n2, direction="<")
n2.roc

# Comparing ROC Curves
plot(n2.roc, lwd=3)
lines(n1.roc, lwd=3, col = "orange")
lines(rf.roc, lwd=3, col = "red")
lines(bt.roc, lwd=3, col = "blue")
legend("bottomright",title="ROC Curves",c("deep learning", "nerual net","random forest","gradient boosting"), fill=c("black","orange","red","blue"))
