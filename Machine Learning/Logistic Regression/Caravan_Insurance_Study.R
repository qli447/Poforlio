rm(list = ls())

# Load dataset
caravan <- read.csv("Caravan.csv")
head(caravan)

# Dropping Variable X (the number for each observation)
caravan<-caravan[-c(1)]


# Creating variable Pur (purchase) as 0 for no, 1 for yes.
caravan$Purchase <- ifelse(caravan$Purchase=='No',0,1)
head(caravan)

# Selecting 1000 samples for test, rest for training
set.seed(42)
trn <- runif(nrow(caravan)) < 0.83
train <- caravan[trn==TRUE,]
test <- caravan[trn==FALSE,]

# Logistic Regression
glm <- glm(Purchase~., family="binomial", data=train)
summary(glm)

yhat_glm <- predict(glm, type="response") 
hist(yhat_glm)
table(train$Purchase, (yhat_glm >0.3)) 


test$yhat.glm <- predict(glm, test, type="response")


#TRUE POSITIVE RATE, SENSITIVITY
TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(y==1) }
TPR(train$Purchase, (yhat_glm >0.3))

#TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(y,yhat)  { sum(y==0 & yhat==0) / sum(y==0) }
TNR(train$Purchase, (yhat_glm >0.3))

table(test$Purchase, (test$yhat.glm > 0.3))
TPR(test$Purchase, (test$yhat.glm > 0.3))
TNR(test$Purchase, (test$yhat.glm > 0.3))
 


#TRUE POSITIVE RATE, SENSITIVITY
TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(y==1) }
TPR(train$Purchase, (yhat_glm >0.5))
#TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(y,yhat)  { sum(y==0 & yhat==0) / sum(y==0) }
TNR(train$Purchase, (yhat_glm >0.5))

table(test$Purchase, (test$yhat.glm > 0.5))
TPR(test$Purchase, (test$yhat.glm > 0.5))
TNR(test$Purchase, (test$yhat.glm > 0.5))
