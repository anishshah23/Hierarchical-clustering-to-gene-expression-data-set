#A
data <- read.csv("~/DS/STA DM2/HW2/Ch10Ex11.csv", header = FALSE)

#B
  #Complete linkage
hc.com <- hclust(as.dist(1 - cor(data)), method = "complete")
plot(hc.com)

  #Single linkage
hc.single <- hclust(as.dist(1 - cor(data)), method = "single")
plot(hc.single)

  #Average linkage
hc.average <- hclust(as.dist(1 - cor(data)), method = "average")
plot(hc.average)

  #K-means
k<- kmeans(t(data), centers=2)
k$cluster

#C
pr = prcomp(t(data))
summary(pr)

k2 <- kmeans(data, centers=2, nstart=20)
k2$cluster

pc <- prcomp(data, center=TRUE, scale. = TRUE)

par(mfrow=c(2,2))
biplot(pc)
plot(pc$x[,1], pc$x[,2], col=k2$cluster, pch=16, xlab="PC 1", ylab="PC 2")
plot(summary(pc)$importance[2,], type='l', lwd=2, ylab="Proportion Variance Explained", ylim=c(0,1))
plot(summary(pc)$importance[3,], type='l', lwd=2, ylab="Cumulative Variance Explained", ylim=c(0,1))
