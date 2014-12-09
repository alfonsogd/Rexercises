# Construcción de los datos
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)

# Ajuste del modelo
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())

# Resumen del modelo
anova(glm.D93)
summary(glm.D93)
