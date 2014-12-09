STAKEHOLDERSTRESGRUPOS <- read.csv("~/Documentos/Dropbox/DireccionyTutoria/Carolina Rodriguez/base de datos caro/STAKEHOLDERSTRESGRUPOS.csv")
#Eliminar la primera columna "GRUPO"
BDok <- (STAKEHOLDERSTRESGRUPOS[,-1])
##########ANALISIS DE COMPONENTES PRINCIPALES
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(BDok, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
#######ROTACION VARIMAX
# Varimax Rotated Principal Components
# retaining 5 components 
# Usa el paquete psych y del paquete GPArotation
library(psych)
fit2 <- principal(BDok, nfactors=5, rotate="varimax")
fit2 # print results
############Analisis factorial exploratorio
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit3 <- factanal(BDok, 10, rotation="varimax")
print(fit3, digits=2, cutoff=.3, sort=TRUE)
#bd <- print(fit3, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit3$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(BDok),cex=.7) # add variable names
# Principal Axis Factor Analysis
#library(psych)
#fit4 <- factor.pa(BDok, nfactors=3, rotation="varimax")
#fit4 # print results
#################### Determine Number of Factors to Extract
## Requiere el paquete nFactors
library(nFactors)
ev <- eigen(cor(BDok)) # get eigenvalues
ap <- parallel(subject=nrow(BDok),var=ncol(BDok),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
###############GRAFICAR
# Requiere paquete FactoMineR
# PCA Variable Factor Map 
library(FactoMineR)
result <- PCA(BDok) # graphs generated automatically
