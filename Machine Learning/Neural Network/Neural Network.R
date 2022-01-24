#PREDICT WINE QUALITY BASED ON PHYSIOCHEMICAL INFORMATION
#https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
describe(dat)[,1:9]
head(dat)

#########################################
#	Define high quality wines
#########################################

table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

#########################################
#	Some exploration
#########################################

library(corrplot)
corrplot(cor(dat), method="number")

doPlot <- function(vvar){
  n <- xtabs(~dat[,vvar])
  x <- unique(dat[,vvar])
  x <- x[order(x)]
  y <- tapply(dat$quality, dat[,vvar], mean)
  plot(x,y, cex=(n/mean(n))^.5, col="navy", lwd=2
       , xlab=vvar, ylab="high quality", main=vvar, las=1)
  abline(h=mean(dat$quality), col="blue", lwd=2)
  abline(lm(y~x, weight=n), col="chartreuse3", lwd=2)
}


par(mfrow=c(1,1))
doPlot("fixed.acidity")
doPlot("volatile.acidity")
doPlot("citric.acid")
doPlot("residual.sugar")
doPlot("chlorides")
doPlot("free.sulfur.dioxide")
doPlot("total.sulfur.dioxide")
doPlot("density")
doPlot("pH")
doPlot("sulphates")
doPlot("alcohol")

#########################################
#	Split dataset
#########################################

set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]

#########################################
#	logistic regression
#########################################
glm <- glm(quality ~ ., family="binomial", data=train)
summary(glm)
yhat_glm <- predict(glm, type="response")
table(train$quality, (yhat_glm >0.5))
test$yhat.glm <- predict(glm, test, type="response")
55/(97+55) #TRUE POSITIVE RATE, SENSITIVITY
TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(y==1) }
TPR(train$quality, (yhat_glm >0.5))
953/(953+31) #TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(y,yhat)  { sum(y==0 & yhat==0) / sum(y==0) }
TNR(train$quality, (yhat_glm >0.5))

table(test$quality, (test$yhat.glm > 0.5))
TPR(test$quality, (test$yhat.glm > 0.5))
TNR(test$quality, (test$yhat.glm > 0.5))
##########################################
#########################################

table(test$quality, (test$yhat.glm > 0.5))
TPR(test$quality, (test$yhat.glm > 0.5))
TNR(test$quality, (test$yhat.glm > 0.5))

#########################################
#build a tree
#########################################
install.packages("tree")
library(tree)
myc <- tree.control(nrow(train),minsize=5, mindev=0.0003)
t1 <- tree(quality ~ .,  data=train, control = myc)
plot(t1)
text(t1)
install.packages('rpart')

library(rpart)
form1 <- formula(quality~.)
t1 <- rpart(form1, data=train, cp=.001, method="class")
plot(t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(t1, cex=.7, col="navy",use.n=TRUE)
plotcp(t1)
CP <- printcp(t1)
########################################
#According to the documentation, 
#¡°a good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line¡±. 
#So let¡¯s go with:

cp <- CP[,1][CP[,4] == min(CP[,4])]
cp
t2 <- prune(t1,cp=cp[1])
plot(t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(t2, cex=.7, col="navy",use.n=TRUE)
##########################################
#new confusion table after pruning
##########################################
yhat.t2 <- predict(t2, test, type="prob")[,2]
table(yhat.t2>0.5,test$quality)
TPR(yhat.t2>0.5,test$quality)
TNR(yhat.t2>0.5,test$quality)
##########################################
#ROC Plot
############################################
library(pROC)
t2.roc <- roc(test$quality, yhat.t2, direction="<")
t2.roc
plot(t2.roc, lwd=3)
par(new=TRUE)
glm.roc<- roc(test$quality, test$yhat.glm, direction="<")
plot(glm.roc, lwd=3)

######################
# RandomForest
######################
install.packages("randomForest")
library(randomForest)

xvars <- names(train)[1:11]
X <- as.matrix(train[,xvars])
X.tst <- as.matrix(test[,xvars])
Y <- factor(train$quality)

set.seed(652)

# mtry is the number of variables to try in each tree
# importance is whether variable importance is assessed
rf1 <- randomForest(x=X, y=Y, data=train,
                    ntree=500, mtry=3, importance=T, na.action=na.omit)

summary(rf1)
names(rf1)
head(rf1$importance)
# variable importance ranking
imp <- rf1$importance[,4]
ord <- order(imp, decreasing=T)
imp <- imp[ord]

par(mar=c(2, 8, 4, 2) + 0.1)
barplot(imp, col='lavender', horiz=TRUE, las=1, cex.names=.8)
title("Random Forest Variable Importance Plot")
######################################ROCÍ¼
yhat.rf <- predict(rf1, test, type="prob")[,2]
test$yhat.rf <- yhat.rf

#####################################
#random forest confusion table
####################################
table(yhat.rf>0.5,test$quality)
TPR(yhat.rf>0.5,test$quality)
TNR(yhat.rf>0.5,test$quality)
#####################################
par(mfrow=c(1,1))
library(pROC)

rf.roc <- roc(test$quality, test$yhat.rf, direction="<")
rf.roc
t2.roc

#svm.roc <- roc(test$quality, test$yhat.svm, direction="<")
#glm.roc <- roc(test$quality, test$yhat.glm, direction="<")
plot(rf.roc, lwd=3)
#lines(svm.roc, lwd=3, col = "yellow")
#lines(glm.roc, lwd=3, col = "green")
lines(t2.roc, lwd=3,col="yellow")
legend("bottomright",title="ROC Curves",c("rf","clasitree"), fill=c("black","yellow"))
#########################################
#   gradient boost
#########################################
install.packages("xgboost")
library(xgboost)
Y <- train$quality

parm <- list(nthread=2, max_depth=5, eta=0.10, gamma=3.5, min_child_weight=20, subsample=.67)
bt <- xgboost(parm, data=X, label=Y, verbose=2, objective='binary:logistic', nrounds=20)
# variable importance 
imp <- xgb.importance(feature_names=colnames(X), model=bt)
imp
xgb.plot.importance(imp, rel_to_first = TRUE, xlab = "Relative importance")
test$yhat.bt <- predict(bt, X.tst) 
##########################################
#confusion table
##########################################
table(test$yhat.bt >0.5,test$quality)
TPR(test$yhat.bt >0.5,test$quality)
TNR(test$yhat.bt >0.5,test$quality)
##########################################ROCÍ¼
par(mfrow=c(1,1))
library(pROC)
bt.roc <- roc(test$quality, test$yhat.bt, direction="<")
bt.roc
plot(bt.roc, lwd=3)
t2.roc <- roc(test$quality, yhat.t2, direction="<")
t2.roc
lines(t2.roc, lwd=3,col="yellow")
lines(rf.roc, lwd=3, col = "purple")
#lines(svm.roc, lwd=3, col = "yellow")
lines(glm.roc, lwd=3, col = "green")
legend("bottomright",title="ROC Curves",c("bf","rf","classitree","glm"), fill=c("black","purple","yellow","green"))
#########################################
#   neural nets
#########################################

dat[,1:11]  <- scale(dat[,1:11])
dat$quality <- factor(dat$quality)
train2 <- dat[trn==TRUE,]
test2 <- scale(test[,1:11])

form1 <- formula(quality ~ fixed.acidity + volatile.acidity + citric.acid   + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

install.packages("nnet")
library(nnet)
n1 <- nnet(form1, data = train2, size = 7, maxit = 500, decay=0.002)

yhat.n1 <- predict(n1, test)
test$yhat.n1 <- yhat.n1 
#########################################ROCÍ¼
par(mfrow=c(1,1))
library(pROC)
nn.roc <- roc(test$quality, test$yhat.n1, direction="<")
nn.roc

rf.roc <- roc(test$quality, test$yhat.rf, direction="<")
bt.roc <- roc(test$quality, test$yhat.bt, direction="<")
plot(nn.roc, lwd=3)
lines(rf.roc, lwd=3, col = "yellow")
lines(bt.roc, lwd=3, col = "green")
legend("bottomright",title="ROC Curves",c("nnet","rf","bt"), fill=c("black","yellow","green"))
#########################################
#   Deep Learning
#########################################
install.packages("neuralnet")
library(neuralnet)

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dat$quality <- I(dat$quality > 6) * 1
dat[,1:11]  <- scale(dat[,1:11])
train2 <- dat[trn==TRUE,]

n2 <- neuralnet(form1, data=train2, hidden = c(2,2,2,2),threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7) #linear.output=FALSE)
yhat.n2 <- compute(n2, test2)
test$yhat.n2 <- yhat.n2$net.result

#net.sinp <- neuralnet(nntr4 ~ nntr0 + nntr1 + nntr2 + nntr3, data=nntr,     hidden=10, threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7)
n2.roc <- roc(test$quality, test$yhat.n2, direction="<")
n2.roc
rf.roc <- roc(test$quality, test$yhat.rf, direction="<")
bt.roc <- roc(test$quality, test$yhat.bt, direction="<")
lines(nn.roc, lwd=3)
plot(n2.roc, lwd=3, col="red")
lines(rf.roc, lwd=3, col = "yellow")
lines(bt.roc, lwd=3, col = "green")
legend("bottomright",title="ROC Curves",c("nn","dnnet","rf","bt"), fill=c("black","Red","yellow","green"))
