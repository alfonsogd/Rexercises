# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
BaseDatos2 <- read.csv("~/Rall/BaseDatos2.csv")
misdatos <- BaseDatos2[,-1]
fit <- princomp(misdatos, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
##################################
# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(misdatos)) # get eigenvalues
ap <- parallel(subject=nrow(misdatos),var=ncol(misdatos),
               rep=100,cent=.3)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
##########################
# Varimax Rotated Principal Components
# retaining 10 components 
library(psych)
fit2 <- principal(misdatos, nfactors=10, rotate="varimax")
fit2 # print results
# para validar con la mitad aleatoriamente obtenida
mitaddemisdatos <- misdatos[sample(nrow(misdatos), 250), ]
fit3 <- princomp(mitaddemisdatos, cor=TRUE)
summary(fit3) # print variance accounted for 
loadings(fit3) # pc loadings 
plot(fit3,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit3)
fit4 <- principal(mitaddemisdatos, nfactors=10, rotate="varimax")
fit4 # print results
############################
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 10 factors, 
# with varimax rotation 
fit5 <- factanal(misdatos, 10, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(misdatos),cex=.7) # add variable names
