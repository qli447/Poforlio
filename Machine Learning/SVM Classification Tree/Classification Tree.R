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
dat <- read.csv("winequality-red.csv")
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

library(pROC)
t2.roc <- roc(test$quality, yhat.t2, direction="<")
t2.roc
plot(t2.roc, lwd=3)
par(new=TRUE)
glm.roc
plot(glm.roc, lwd=3)