rm(list=ls())
#install.packages("earth")
#install.packages("rpart")
#install.packages("tree")
library(psych)

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)

#	Define high quality wines

table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

#	Some exploration

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

#	Split dataset

set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]


# Classification Tree
attach(dat)
library(rpart)
form1<-formula(quality~.)
t1<-rpart(form1, data=train,cp=.001,method="class")
plot(t1,uniform=T,compress=T,margin=0.05, brach=0.3)
text(t1,cex=.7,col="navy", use.n=TRUE)


plotcp(t1)

CP<-printcp(t1)

cp <- CP[,1][CP[,2] == 2]
cp


#prune from that cut
t2 <- prune(t1,cp=cp[1])
plot(t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(t2, cex=.7, col="navy",use.n=TRUE)

X<-scale(dat[,1:11])
tst<-1:463
X.trn <- X[-tst,]
X.tst <- X[tst,]
Y.trn <- dat[-tst,12]
Y.tst <- dat[tst,12]
dat.train <- dat[-tst,]
dat.test <- dat[tst,]

yhat.t1 <- predict(t1,test, type="prob")[,2]
yhat.t2 <- predict(t2, test, type="prob")[,2]
table(yhat.t1>0.5,Y.tst)
table(yhat.t2>0.5,Y.tst)
# can compare the rates from table

TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(yhat==1) }
TPR(yhat.t2>0.5,Y.tst)

library("pROC")
t2.roc<-roc(Y.tst, yhat.t2, direction="<")
t2.roc
plot(t2.roc, lwd=3)

t1.roc <- roc(Y.tst, yhat.t1, direction="<")
t1.roc
plot(t1.roc, lwd=3)

cor(predict(t2,test),Y.tst)^2
