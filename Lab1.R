#Lab 1
source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
dim(cdc)
class(cdc$genhlth)
table(cdc$gender)
summary(cdc$genhlth)/20000
table(cdc$genhlth)/20000
barplot(table(cdc$genhlth))
gender_smokers = table(cdc$gender,cdc$smoke100)
mosaicplot(gender_smokers)
cdc$weight[1:10]
cdc$gender=="m"
cdc$age>30
mdata = subset(cdc, cdc$gender == "m")
m_and_over30 = subset(cdc, cdc$gender == "m" & cdc$age > 30) # masculino y mayor a 30
m_or_over30 = subset(cdc, cdc$gender == "m" | cdc$age > 30) # masculino o mayor a 30
under23_and_smoke <- subset(cdc, cdc$age < 23 & cdc$smoke100 > 0)
boxplot(cdc$height)
summary(cdc$height)
boxplot(cdc$height ~ cdc$gender)
bmi = (cdc$weight / cdc$height^2) * 703 # body mass index (BMI)
boxplot(bmi ~ cdc$genhlth)
boxplot(bmi ~ cdc$smoke100)
boxplot(bmi ~ cdc$gender)
hist(cdc$age)
hist(bmi)
hist(bmi, breaks = 50)
plot(cdc$wtdesire ~ cdc$weight)
plot(cdc$weight ~ cdc$wtdesire)
