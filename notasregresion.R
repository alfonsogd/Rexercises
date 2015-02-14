#A coefficient will be within 2 standard errors of its estimate about 95% of the time. This means the slope of our
#| regression is significantly different than either 0 or 1 since (.64629) +/- (2*.04114) is near neither 0 nor 1
First check the mean of fit$residuals to see if it's close to 0
Now check the correlation between the residuals and the predictors. Type "cov(fit$residuals, galton$parent)" to see
| if it's close to 0.
We'll see that when we vary or tweak the slope and intercept values of the regression line which are stored in
| fit$coef, the resulting squared residuals are approximately equal to the sum of two sums of squares - that of the
| original regression residuals and that of the tweaks themselves. More precisely, up to numerical error,
 sqe(ols.slope+sl,ols.intercept+ic) == deviance(fit) + sum(est(sl,ic)^2 )
Equivalently, sqe(ols.slope+sl,ols.intercept+ic) == sqe(ols.slope, ols.intercept) + sum(est(sl,ic)^2 )
The left side of the equation represents the squared residuals of a new line, the "tweaked" regression line. The
| terms "sl" and "ic" represent the variations in the slope and intercept respectively. The right side has two terms.
| The first represents the squared residuals of the original regression line and the second is the sum of squares of
| the variations themselves.
Since variances are sums of squares (and hence always positive), this equation which we've just demonstrated,
| var(data)=var(estimate)+var(residuals), shows that the variance of the estimate is ALWAYS less than the variance of
| the data. Since var(data)=var(estimate)+var(residuals) and variances are always positive, the variance of residuals
is less than the variance of data too.
the slope of the regression line is the correlation between the two sets of heights
| multiplied by the ratio of the standard deviations (childrens' to parents' or outcomes to predictors)
sqrt(deviance(fit)/(n-2)) = summary(fit)$sigma = sqrt(sum(fit$residuals^2) / (n-2))
Total Variation = Residual Variation + Regression Variation
Yi-mean(Yi) = sum of squared of this term represent Total Variation ej: sTot <- sum((galton$child-mu)^2)
Which sum of squared term represents Residual Variation? = Yi-Yi_hat ej: sRes <- deviance(fit)
The term R^2 represents the percent of total variation described by the model = 1-residual variation/total variation 
ej: 1- sRes/sTot = summary(fit)$r.squared =  cor(galton$child, galton$parent)^2

## Ejemplo para intervalo de confianza de la regresión
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2))
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
# Es lo mismo que:
fit <- lm(y ~ x)
summary(fit)$coefficients
# obteniendo un intervalo de confianza
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
# interpretamos que.. With 95% confidence, we estimate that a 0.1 carat increase in diamond size results
# in a 355.6 to 388.6 increase in price in (Singapore) dollars.
# Ploteando los intervalos de confianza
plot(x, y, frame=FALSE,xlab="Carat",ylab="Dollars",pch=21,col="black", bg="lightblue", cex=2)
abline(fit, lwd = 2)
xVals <- seq(min(x), max(x), by = .01)
yVals <- beta0 + beta1 * xVals
se1 <- sigma * sqrt(1 / n + (xVals - mean(x))^2/ssx) # linea a X0 (se1)
se2 <- sigma * sqrt(1 + 1 / n + (xVals - mean(x))^2/ssx) # Intervalo de la predicción (se2) 
lines(xVals, yVals + 2 * se1)
lines(xVals, yVals - 2 * se1)
lines(xVals, yVals + 2 * se2)
lines(xVals, yVals - 2 * se2)
# en R
newdata <- data.frame(x = xVals)
p1 <- predict(fit, newdata, interval = ("confidence"))
p2 <- predict(fit, newdata, interval = ("prediction"))
plot(x, y, frame=FALSE,xlab="Carat",ylab="Dollars",pch=21,col="black", bg="lightblue", cex=2)
abline(fit, lwd = 2)
lines(xVals, p1[,2]); lines(xVals, p1[,3])
lines(xVals, p2[,2]); lines(xVals, p2[,3])

### Para el caso de regresión multivariable
# la línea de regresión es la regresión al origen con una variable, eliminando las otras al reducir sus residuos
# en lo que aportan a la línea de regresión
# Ejemplo de aplicacion con dos variables y un intercepto
n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = .1)
e <- function(a, b) a - sum( a * b ) / sum( b ^ 2) * b
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term
coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term
#So that the interpretation of a multivariate regression coefficient is the expected change in the
#response per unit change in the regressor, holding all of the other regressors fixed.
#