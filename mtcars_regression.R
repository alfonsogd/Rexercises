data(mtcars)
library(MASS)
#########
require(graphics)
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
coplot(mpg ~ wt | hp, data = mtcars,
       panel = panel.smooth, rows = 1)
coplot(mpg ~ wt | as.factor(gear), data = mtcars,
       panel = panel.smooth, rows = 1)
coplot(mpg ~ disp | as.factor(carb), data = mtcars,
       panel = panel.smooth, rows = 1)
coplot(mpg ~ qsec | as.factor(vs), data = mtcars,
       panel = panel.smooth, rows = 1)
##### primer modelo sin tratamiento alguno
fit <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+am+gear+carb, data=mtcars)
step <- stepAIC(fit, direction="backward")
step$anova
###### segundo modelo, transformando mpg a gpm pero nadamás
fit2 <- lm(mpg^-1 ~ cyl+disp+hp+drat+wt+qsec+am+gear+carb, data=mtcars)
step2 <- stepAIC(fit2, direction="backward")
step2$anova
#### tercer modelo el sugerido por Henderson y Velleman (1981)
hpwt <- mtcars$hp/mtcars$wt
fit3 <- lm(mpg^-1*100 ~ cyl+disp+drat+wt+qsec+am+gear+carb+hpwt+hp, data=mtcars)
step3 <- stepAIC(fit3, direction="both")
step3$anova
#### cuarto modelo con modificaciones para la tarea
hpwt <- mtcars$hp/mtcars$wt
cyldisp <- mtcars$cyl/mtcars$disp
gearwt <- mtcars$gear/mtcars$wt
carbwt <-mtcars$carb/mtcars$wt
fit4 <- lm(mpg^-1*100 ~ cyl+disp+drat+wt+qsec+am+gear+carb+hpwt+hp+cyldisp+gearwt+carbwt, data=mtcars)
step4 <- stepAIC(fit4, direction="both")
step4$anova
