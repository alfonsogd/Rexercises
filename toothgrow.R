---
  title: "Statistical Inference Project"
author: "Alfonso Gonzalez Damian"
subtitle: Exponential distribution vs CLT
output: html_document
---

### Exploring dataset
* 
data(ToothGrowth)
summary(ToothGrowth)
dim(ToothGrowth)
n <- length(ToothGrowth$len)
par(mfrow =c(1,2))
plot(ToothGrowth$len ~ ToothGrowth$supp, type="l", xlab="Supp", ylab="Tooth Growth")
plot(ToothGrowth$len ~ as.factor(ToothGrowth$dose), xlab="Dose", ylab="Tooth Growth")

### Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
(Only use the techniques from class, even if there's 
other approaches worth considering)
Did the student perform some relevant confidence intervals and/or tests?
Were the results of the tests and/or intervals interpreted in the context of the 
problem correctly? 

### Conclusions and the assumptions needed
Did the student describe the assumptions needed for their conclusions?