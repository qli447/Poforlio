rm(list = ls())

# Reading csv  file
df_telecom <- read.csv('tel.csv')

# Correlation of variables
library(corrplot)
library(RColorBrewer)
M <-cor(df_telecom)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
#reference to corrplot: http://www.sthda.com/english/wiki/correlation-analyses-in-r

# Regression on Hours
names(df_telecom)
reg1<-lm(ï..Hours~ByDa+RWT+SOA+SOB+SOC+Field+Hot+Day, data=df_telecom)
summary(reg1)

# Plotting Reg1 residuals 
hist(reg1$residuals)

#confidence intervals
confint(reg1)


# Adding Friday dummy variably and new regression
df_telecom['Friday'] <- ifelse(df_telecom$Day==5,1,0)
M2 <-cor(df_telecom)
corrplot(M2, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

reg2<-lm(ï..Hours~ByDa+RWT+SOA+SOB+SOC+Field+Hot+Day+Friday, data=df_telecom)
summary(reg2)

hist(reg2$residuals)

# Backwards Selection to find a better model
library(MASS)
reg3<-step(reg2, direction="backward")
summary(reg3)

hist(reg3$residuals)

r2 <- c(summary(reg1)$r.squared,summary(reg2)$r.squared,summary(reg3)$r.squared)
plot(r2,type="o",ylim=c(0, 1),lwd=3,col="blue",xlab="model complexity")

# VALIDATION
PredScores <- predict(reg3) #Determine predictions
Eps <- PredScores - df_telecom$ï..Hours #...and residuals

par(mfrow=c(1,2)) 
hist(Eps)
qqnorm(Eps)
qqline(Eps)

# comparison of R-squares
fits_train <- c(summary(reg1)$r.squared,summary(reg2)$r.squared,summary(reg3)$r.squared)
plot(fits_train,type="o",lwd=3,col="blue",xlab="model complexity")

with(df_telecom, plot(Friday,ï..Hours))
abline(reg3)

hist(df_telecom$ï..Hours, xlab='Hours', main='Histogram of Hours', breaks = 15)
