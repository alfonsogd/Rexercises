# Load the 'mlb11' data frame into the workspace:
load (url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/mlb11.RData"))
correlation = cor(mlb11$runs, mlb11$at_bats)

# Print the result:
correlation
# Use the 'plot_ss' function to draw an estimated regression line. Two points are given to get you started:
x1 = 5400
y1 = 750

x2 = 5700
y2 = 650
plot_ss(x = mlb11$at_bats, y = mlb11$runs, x1, y1, x2, y2, showSquares = TRUE)
#play around to minimize squares
# Use the 'plot_ss' function to draw an estimated regression line. Two points are given to get you started:
x1 = 5400
y1 = 600

x2 = 5700
y2 = 850
plot_ss(x = mlb11$at_bats, y = mlb11$runs, x1, y1, x2, y2, showSquares = TRUE)
# Adapt the function to plot the best fitting line:
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE, leastSquares = TRUE)
# Use the 'lm' function to make the linear model:
m1 = lm(runs ~ at_bats, data = mlb11)

# Print the model:
m1
summary(m1)
# Create a scatterplot:
plot(mlb11$runs ~ mlb11$at_bats)

# The linear model:
m1  = lm(runs ~ at_bats, data = mlb11)

# Plot the least squares line:
abline(m1)
#residuals to check conditions of reliability : constant variability
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3)
# nerly normal residuals:
qqnorm(m1$residuals)
qqline(m1$residuals)
hist(m1$residuals)