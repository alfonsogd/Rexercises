primera pregunta: t.test (mtcars$mpg)
segunda pregunta qt(.975,8, log=F)
Tercera en dos fases: mtcars2 <- subset(mtcars, cyl %in% c(4,6))
  t.test(mtcars2$mpg ~ mtcars2$cyl, paired=FALSE, var.equal=TRUE)

lambda <- 0.2
n = 40
means <- vector("numeric",1000)
for (i in 1:1000) means[i] <- mean(rexp(n, lambda))
theo_mean <- 1 / lambda
sample_mean <- mean(means)
theo_var <- 1/(lambda^2)/n
sample_var <- var(means)
 
hist(rexp(1000), 20, main="Exponential Distribution", xlab='x', ylab='Probability')
h <- hist(means, 20)
xnorm <- seq(min(means), max(means), length=50)
ynorm <- dnorm(xnorm, mean=theo_mean, sd=sqrt(theo_var))
ynorm <- ynorm * (h$breaks[2] - h$breaks[1]) * length(means) # scale
lines(xnorm, ynorm)

### otra versión
data(ToothGrowth)
nrow(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$dose)
boxplot(len ~ supp + dose, data=ToothGrowth)
t.test(len ~ supp, data=ToothGrowth)
t.test(len ~ dose, data=ToothGrowth[ToothGrowth$dose %in% c(0.5, 1),])
t.test(len ~ dose, data=ToothGrowth[ToothGrowth$dose %in% c(1,2),])
t.test(len ~ dose, data=ToothGrowth[ToothGrowth$dose %in% c(0.5,2),])

###NOTAS
Error estándar de la muestra en distribucion normal: 1/sqrt(n)
Error estándar de la muestra en distribucion uniforme: 1/sqrt(12*n)
Error estándar de la muestra en distribucion poisson: 2/sqrt(n)
Error estándar de la muestra en distribución binomial: 1/(2*sqrt(n))
Varianza de la muestra es la varianza de la población / n
El error estandar de la media de la muestra es la desviación estándar de la muestra / sqrt(n)
Probabilidad de que X=x en una distribución Bernoulli (PMF de la D.Bernoulli): p^x*(1-p)^(1-x)
En una Bernoulli, la esperanza (media) es = p. la E(x^2)=p, varianza = p(1-p) o también p-p^2, cuando se desconoce p se puede usar 1/2 o p'(Wald confidence interval) 
en R: p'+/-qnorm(cuantil)*sqrt(p'(1-p')/n) Recuerda que los cuantiles en la normal son /2 pues representan la mitad del área bajo la curva.
Una alternativa es el intervalo Agresti/Coull se añaden 2 exitos y dos fracasos a los conteos cuando se calcula p'=(x+2)/n+4
La PMF de una variable aleatoria binomial = choose(n,x)*p^x*(1-p)^(n-x). Donde choose(n,x) representa el coeficiente binomial n choose x o el numero de formas de éxito que en n intentos pueden ocurrir.
En R, se obtiene con pbinom(x,n,p,lower.tail=FALSE)
binom.test(x,n)$conf.int da el intervalo de confianza en un experimento binomial. 

En la normal, la funcion de R qnorm(prob) devuelve el valor x(cuantil) en el cual el área bajo la curva normal a la izquierda de x es igual al parámetro prob.
Para valores estándar z, qnorm asume mu=0 y var=1, para no estandar pero normal, qnorm admite los valores de mean y sd.
pnorm en R, arroja la probabilidad asociada a que una variable sea igual o mayor a un valor x. 
Los valores Z=(X-mu)/sigma. pnorm y qnorm son inversas.
Para normalizar datos, se resta la muestra poblacional mu de la media muestral (de la variable aleagtoria) y se divide la diferencia por sigma/sqrt(n)

En la poisson, utilizada para conteos (distribuciones discretas), su PMF tiene un parámetro, lambda, P(X=x)=(lambda^x)e^(-lambda)/x!. Aqui x está en el rango de 0 a infinito.
media y varianza son lambda. En R se usa ppois la cual devuelve una probabilidad de que la variable aleatoria sea menor que un valor (cuantil). se debe especificar la media (mean*t), donde t es el período. Usa los parametros lower.tail=TRUE y log.p=FALSE
ppois(cuantil, lambda)
La poisson aproxima a la binomial en ciertos casos. En una binomial, si n es grande y p pequeña, la poisson con lambda = n*p es una buena aproximación
lambda_hat=x/t para estimar lambda (failure rate o tasa de eventos en t período de tiempo) y la varianza de lambda_hat se estima con lambda_hat/t.
un intervalo de confianza se calcula en R con: est mean+c(-1,1)*qnorm(.975)*sqrt(est var) o también 
poisson.test(x,t)$conf

En la distribución t=(X'-mu)/(s/sqrt(n)) está centrada alrededor de 0 usa la desviación estandar de la muestra (por lo tando depende del tamaño de n)
Confidence interval: Est +/- t-quantile *std error(Est), también: X' +/- t_(n-1)*s/sqrt(n) donde t_(n-1) es el cuantil relevante 
t statistic t=(X'-mu)/(s/sqrt(n)) con 1 gl. Funcion en R para calcular t segun cuantiles y grados de libertad: qt(cuantil, gl)
Para diferencias de medias se usa la t, con media como media de las diferencias y gl=n-1. También se puede usar t.test
#Cuatro formas de calcular el intervalo de confianza
#las despliega como un array de 4 vectores, NOTA: mn y s ya están en la memoria.
rbind(
  mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
  as.vector(t.test(difference)$conf.int),
  as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
  as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)
Para diferencias de grupos no pareados, con varianza igual, la media a usar es la diferencia entre las medias de los dos grupos (no la media de las diferencias 
como en el caso d elas pareadas) y para el cuantil de interés se usa: t_(cuantil,n_x+n_y-2), para la desv estandar de la muestra
o error estándar de la diferencia se usa pooled sample variance / gl totales / standard error: (s_x^2*(n_x-1)+s_y^2*(n_y-1))/sqrt((n_x-1)+(n_y-1))
o tambien funcion t.test(grupo2, grupo1, paired=FALSE, var.equal=TRUE)

Para grupos con distinta varianza, no se calcula la pooled variance, SE = sqrt((s_1)^2/n_1 + (s_2)^2/n_2) Que es la suma del cuadrado
de los errores estándar de ambas medias. La formula para los grados de libertad es compleja, por lo que se usa el t.test.
grados de libertad: ((s_x^2/n+s_y^2/n)^2)/(s_x^4/n^2/(n-1)+s_y^4/n^2/(n-1)) formula: X'_{oc}-X'_{c} +/- t_df * SE, 
sqrt(SE= (s_1)^2/n_1 + (s_2)^2/n_2) o con t.test(grupo2, grupo1, paired=FALSE, var.equal=FALSE)

Para prueba de hipótesis: pt = función de la distribución t. pt(q,df,lower.tail=FALSE) zona de rechazo
pnorm = función de la distribución normal z. pnorm(q,lower.tail=FALSE) zona de rechazo
pbinom = función de la distribución binomial. pbinom(q, size=n, prob=p, lower.tail=FALSE) zona de rechazo
ppois = función de la distribución poisson. ppois(q,lambda=lambda, lower.tail=FALSE)

Power ~ x' + Z_cuantil * (sigma/sqrt(n)) ESto es PODER!!!
En R: pnorm(x'+z_cuantil, mean=muH_a, lower.tail=FALSE)

power.t.test para distribuciones t, permite omitir argumentos. Ejemplo: power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample", alt = "one.sided")$power
size effect en el power test: (mu_a - mu_0) / sigma

Tests múltiples: para reducir la FWER (Family-wise error rate) es decir la probabilidad de dar al menos un falso
positivo, se usa la corrección Bonferroni (alpha=alpha/10), solo pasan los que tengan un p-value mucho mayor que alpha=5% de los tests declarados significativos.
Otro método de control es el Benjamini-Hochberg (BH) que rankea los p-value y solo acepta los que sean significativos 
entre el listado de acuerdo al ranking.
p.adjust es el comando en R para correcciones de este tipo, p.adjust(pValues, method="bonferroni") < 0.5


### Bootstrapping: nonparametric
resam <- matrix(sample(bdoriginal, length(bdoriginal)*n, replace=TRUE), lenth(bdoriginal), n)
meds <- apply(resam, 1, median) ## para calcular medianas de cada muestra

permutación: sample(bdoriginal) # solo permuta
perms <- sapply(1 : 10000, function(i) testStat(BCcounts, sample(group))) # BCcounts= bdoriginal y group=identificadores, nota
la funcion testStat calcula diferencias entre dos medias pareadas.







