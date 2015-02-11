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




