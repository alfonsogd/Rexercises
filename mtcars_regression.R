data(mtcars)
library(MASS)
fit <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
fit2 <- lm(mpg^-1 ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
hpwt <- mtcars$hp/mtcars$wt
fit3 <- lm(mpg^-1 ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb+hpwt, data=mtcars)
step3 <- stepAIC(fit3, direction="both")
vswt <- mtcars$vs/mtcars$wt
amwt <-mtcars$am/mtcars$wt
gearwt <- mtcars$gear/mtcars$wt
carbwt <-mtcars$carb/mtcars$wt
fit4 <- lm(mpg^-1 ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb+hpwt+vswt+amwt+gearwt+carbwt, data=mtcars)
step4 <- stepAIC(fit4, direction="both")
step4$anova
