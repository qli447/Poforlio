rm(list=ls())
library(psych)

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
describe(dat)[,1:9]
head(dat)

#	Define high quality wines
table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]

X<-scale(dat[,1:11])
tst<-1:463
X.trn <- X[-tst,]
X.tst <- X[tst,]
Y.trn <- dat[-tst,12]
Y.tst <- dat[tst,12]
dat.train <- dat[-tst,]
dat.test <- dat[tst,]

# Clustering
#Elbow Method for finding the optimal number of clusters
#set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(dat, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

install.packages("maps")
install.packages("mapproj")
install.packages("NbClust")
library(corrplot)

k <- 4
set.seed(652)
km <- kmeans(dat, k)
clust.km <- km$cluster

dd <- dist(dat, method="euclidean")
hc1 <- hclust(dd, method="average")
hc1 <- hclust(dd, method="complete")
hc1 <- hclust(dd, method="ward.D")
plot(hc1, hang=-1)
rect.hclust(hc1, k=5, border="dodgerblue")
rect.hclust(hc1, k=4, border="blue")
rect.hclust(hc1, k=3, border="red")
rect.hclust(hc1, k=2, border="green")

hc1 <- hclust(dd, method="ward.D")
rect.hclust(hc1, k=4, border="dodgerblue")

clust.hc1 <- cutree(hc1,4)

reord <- function(cluster){
  avg <- tapply(scored$quality, cluster, mean); avg
  ord <- order(avg); ord
  clus <- factor(cluster, levels=ord); table(clus)
  levels(clus) <- 1:length(clus)
  return( as.numeric(as.character(clus)) )
}

scored <- dat
scored$clust.km <- reord(clust.km)
scored$clust.hc <- reord(clust.hc1)
tapply(scored$quality, clust.km, mean)

tapply(scored$quality, reord(clust.km), mean)
table(clust.km, reord(clust.km))

tapply(scored$quality, clust.hc1, mean)
tapply(scored$quality, reord(clust.hc1), mean)
table(clust.hc1, reord(clust.hc1))
table(scored$clust.km, scored$clust.hc)

# PCA
pc1 <- prcomp(dat)
pc1 <- prcomp(scale(dat))
round(pc1$rotation[,1:2], 3)
pcs <- predict(pc1) 
describe(pcs)[,1:5]
dim(pcs); dim(dat)
corrplot(cor(pcs)) 

vars <- apply(pcs, 2, var)
sum(vars); ncol(dat); ncol(pcs)

barplot(vars[1:10], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

plot(pc1)    
summary(pc1)
biplot(pc1, col=c("slategrey", "navy"), cex=c(.2, .8))

round(pc1$rotation, 4)[,1:2]

col <- c("blue","dodgerblue","lightgreen","pink")

# Comparing Clusters in PC Space
par(mfrow=c(1,2))

clust <- scored$clust.km
plot(pcs, type="n", main="k-means")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)

clust <- scored$clust.hc
plot(pcs, type="n", main="hierarchical clustering")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)

# Analyzing the clusters
col <- c("blue", "dodgerblue", "lightgreen", "pink", "red", "maroon", "darkorange")

clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
summ <- apply(dat, 2, agg)
t(round(summ,2))

scored2 <- dat
scored2$PC1 <- pcs[,1]
scored2$PC2 <- pcs[,2]
scored2$PC3 <- pcs[,3]
clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
SUMMARY <- apply(scored2, 2, agg)
t(round(SUMMARY,2))

# Visualize
names(dat)
show <- 1:12
show <- 7:12
par(mfrow=c(3,4))
for(i in show){
  boxplot(scored[,i] ~ clust, col=col, varwidth=TRUE)
  abline(h=0, col="navy")
  title(names(scored)[i])
}
