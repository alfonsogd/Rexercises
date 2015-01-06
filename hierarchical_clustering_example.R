#### hierarchical clustering example
set.seed(1234)
par(mfrow= c(1,1),mar=c(0,0,0,0))
x<- rnorm(12, mean = rep(1:3, each = 4), sd= 0.2)
y <-rnorm(12, mean=rep (c(1,2,1), each=4), sd=0.2)
plot(x,y,col="blue", pch=19, cex=2)
text(x + 0.05, y+0.05, labels = as.character(1:12))
dataFrame <- data.frame(x=x, y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
par(mfrow= c(1,1),mar=c(2,2,2,2))
plot(hClustering)
######## prettier dendogram
Prettier dendrograms
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
        hang = 0.1, ...) {
        ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
        ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
        ## of labels of the leaves of the tree lab.col: colour for the labels;
        ## NA=default device foreground colour hang: as in hclust & plclust Side
        ## effect: A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height, 2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x < 0)]
        x <- x[which(x < 0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot(hclust, labels = FALSE, hang = hang, ...)
        text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
             col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
myplclust(hClustering, lab= rep(1:3, each=4), lab.col = rep(1:3, each = 4))
#### heatmap()
dataFrame <- data.frame(x=x, y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)
