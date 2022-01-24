dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
nm <- names(dat)
dat <- scale(dat)

ss <- function(x)  sum( ( x-mean(x) )^2 )
wss <- NULL
wss[1] <- sum( apply(dat,2,ss) )
for (k in 2:10) {
  temp <- kmeans(dat, k)
  wss[k] <- sum(temp$withinss)
}

barplot(wss, col="dodgerblue", names.arg=1:length(wss)
        , xlab="Number of Clusters (k)"
        , ylab="Total Within Sum of Squares")
abline(h=0)
title("Within Sum-of-Squares Analysis", col.main="navy")

k <- 7
set.seed(652)
km <- kmeans(dat, k)
clust.km <- km$cluster

pc1 <- prcomp(dat)
pc1 <- prcomp(scale(dat))
pc1
round(pc1$rotation[,1:4], 3)

pcs <- predict(pc1) 
vars <- apply(pcs, 2, var)

barplot(vars[1:10], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

biplot(pc1, col=c("slategrey", "navy"), cex=c(.2, .8))
col <- c("blue","dodgerblue","lightgreen","purple","pink","red","orange","yellow")

clust <- clust.km
plot(pcs, type="n", main="k-means")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)

par(mfrow=c(3,4))
for(i in 1:12){
  boxplot( dat[,i] ~ clust, col=col, varwidth=TRUE)
  abline(h=0, col="navy")
  title(nm[i])
}