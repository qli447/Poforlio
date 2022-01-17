mydata<-read.csv("Wage.csv")
dim(mydata)
head(mydata,5)
set.seed(90)
index<-sort(sample(1:nrow(mydata),round(0.75*nrow(mydata))))
train <-mydata[index,]
test<-mydata[-index,]
library(mgcv)
gam1<-gam(wage ~ s(year,k=6)+s(age,bs='cr')+education,family=gaussian, data=mydata)
summary(gam1)
gam2<-gam(wage ~ s(year,bs='cr',k=6)+s(age,bs='cr')+education,family=gaussian, data=mydata)
summary(gam2)
plot(gam2)

          