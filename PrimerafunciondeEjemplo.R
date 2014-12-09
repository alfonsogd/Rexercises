add2 <- function(x, y) {
        x + y
}
above <- function (a, n = 1990) {
        use <- a > n
        a[use]
}
columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[1] <- mean(y[,i], na.rm = removeNA)
        }
        means
}