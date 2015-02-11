### intervalo de confianza Z
data=mtcars
mn <- mean(mtcars$mpg)
sd <- sd(mtcars$mpg)
z <- qnorm(0.05)
# intervalo de confianza un solo lado (izquierdo)
mu0 <- mn - z * sd / sqrt(nrow(mtcars))  ## nota que el resultado va a ser mayor que la media de la H_0
mu0

## t.test de diferencia entre medias, varianzas desiguales
cyl4 <- subset(mtcars, mtcars$cyl == 4)
cyl6 <- subset(mtcars, mtcars$cyl == 6)
t.test(cyl6$mpg, cyl4$mpg, paired=TRUE, var.equal=TRUE, alternative="two.sided")

# t.test de diferencia entre medias, pareadas, varianzas
baseline <- c(140, 138, 150, 148, 135) 
week2 <- c(132, 135, 151, 146, 130)
t.test(baseline, week2, paired=TRUE, var.equal=TRUE, alternative="t")

# intervalo t
1100 + c(-1, 1) * qt(.975, 9-1) * 30 / sqrt(9)

## intervalo de confianza z
3+c(-1,1)*qnorm(.975)*1/sqrt(100) # media=3 y sd=1

# test bernoulli
pbinom(54, prob = .5, size = 100, lower.tail = FALSE) # donde 54 es a partir de allí para arriba (aciertos), prob=probabilidad nula

# test poisson
ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE) # 15800 = num de eventos, 520 eventos por día 30 días
, lower.tail no porque buscamos mayores que. TAmbién se puede calcular aproximando a la normal pues x es grande: 
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)

# z test de equivalencia de proporciones
m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))
pv


# poder para Z
pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE) # 10=media, .95=ic, .4=sd, mean=mean_a

# tamaño de muestra con z
n <- (qnorm(.95) + qnorm(.9)) ^ 2 * .04 ^ 2 / .01^2 #.95=nc buscado, .8= poder deseado, .04=sd real, .01=media buscada

## tests para diferencias entre medias de grupos
cyl8 <- subset(mtcars, mtcars$cyl == 8)
cyl6 <- subset(mtcars, mtcars$cyl == 6)
p <- t.test(cyl8$mpg, cyl6$mpg, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
## Hand calculating the T just to check
2 * pt(-abs(z), df = length(cyl8$mpg) + length(cyl6$mpg) - 2)

# sd across?
mixprob <- (length(cyl8$mpg) - 1) / (length(cyl8$mpg) + length(cyl6$mpg) - 2)
s <- sqrt(mixprob * sd(cyl8$mpg) ^ 2  +  (1 - mixprob) * sd(cyl6$mpg) ^ 2)
z <- (mean(cyl8$mpg) - mean(cyl6$mpg)) / (s * sqrt(1 / length(cyl8$mpg) + 1 / length(cyl6$mpg)))
pz <- 2 * pnorm(-abs(z))

