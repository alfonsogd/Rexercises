# Load your dataset:
load (url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/nc.Rdata"))

# List the variables in the dataset:
names(nc)
# Compute summaries of the data:
summary(nc)

# Use visualization and summary statistics to view the data for 'gained':
summary(nc$gained)
boxplot(nc$gained)  
# Create a clean version fo your data set:
gained_clean = na.omit(nc$gained)

# Set 'n' to the length of 'gained_clean':
n = length(gained_clean)
# Initialize the 'boot_means' object:
boot_means <- rep(NA,100)

# Insert your for loop:
for (i in 1:100) {
        samp1 <- sample(gained_clean, n, replace = TRUE)
        boot_means[i] <- mean(samp1)
}
# Make a histogram of 'boot_means':
hist(boot_means)
load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/inference.Rdata"))
## inference function
source("http://bit.ly/dasi_inference")
#If the above shortened link doesn’t work for you, try http://d396qusza40orc.cloudfront.net/statistics/lab_resources/inference.R
# Run the inference function:
inference(nc$gained, type="ci", method="simulation", conflevel=0.9, est="mean", boot_method="perc")
# Adapt the inference function to confidence level = 95%
inference(nc$gained, type="ci", method="simulation", conflevel=0.95, est="mean", boot_method="perc")
# Adapt the inference function to boot method to standard error:
inference(nc$gained, type="ci", method="simulation", conflevel=0.95, est="mean", boot_method="se")
# Adapt the inference function to estimate with median:
inference(nc$gained, type="ci", method="simulation", conflevel=0.95, est="median", boot_method="se")
# # Draw your plot here:
plot(nc$weight ~ nc$habit)
#You can use the by function as follows to compare the means of the groups, #You can also use other functions like median, summary, etc.
by(nc$weight, nc$habit, mean)
#ahora comparando length
by(nc$weight, nc$habit, length)
# The code for inference  for evaluating whether there is a difference between the average birth weights of babies born to smoker and non-smoker mothers.
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
# Add the 'order' argument to the 'inference' function: para cambiar el orden de la resta en la H nula.
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical", order=c("smoker","nonsmoker"))
# calcular confidence interval
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci ", null = 0, alternative = "twosided", method = "theoretical", order=c("smoker","nonsmoker"))
# calcular edades minimas y maximas por grupo de edad
by(nc$mage, nc$mature, range)
##############segunda parte
#cargar otra base de datos
load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/gss.Rdata"))
head(gss)
class(gss$wordsum)
class(gss$class)
# Numerical and visual summaries of 'wordsum' and 'class':
summary(gss$wordsum)
summary(gss$class)
hist(gss$wordsum)
plot(gss$wordsum)

# Numerical and visual summaries of their relations:
by(gss$wordsum, gss$class, mean)
boxplot(gss$wordsum ~ gss$class)
#anova
inference(y = gss$wordsum, x = gss$class, est = "mean", method = "theoretical", type = "ht", alternative = "greater")
