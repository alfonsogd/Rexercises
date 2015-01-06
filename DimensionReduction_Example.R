#### Dimension Reduction
set.seed(12345)
par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
par(mar=rep(0.2,4))
heatmap(dataMatrix)
set.seed(678910)
for (i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1,size=1, prob=0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
                dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each=5)
        }
}
par(mar=rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1]) 
par(mar= rep(0.2,4))
heatmap(dataMatrix)
# patterns in rows and columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mar= c(4,4,4,4))
par (mfrow= c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab="Row Mean", ylab="Row", pch=19)
plot(colMeans(dataMatrixOrdered), xlab= "Column", ylab= "Column Mean", pch=19)
### Components of the SVD - u and v
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, , xlab="Row", ylab = "First left singular vector", pch=19)
plot(svd1$v[,1], xlab="Column", ylab="First right singular vector", pch=19)
par (mfrow=c(1,2))
plot(svd1$d, xlab= "column", ylab="singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab= "column", ylab="Prop. of variance explained", pch=19)
pca1 <- prcomp(dataMatrixOrdered, scale=TRUE)
plot(pca1$rotation[,1], svd1$v[,1], pch=19, xlab="Principal Component 1", ylab="Right Singular Vector 1")
abline(c(0,1))

