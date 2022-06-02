# Exercise 1

# Read in data
spend = read.delim("C:/Users/austi/OneDrive/Desktop/UTSA/SchoolSummer2022/Income.txt", header = TRUE, sep = "")

# a. scatter plot
plot(spend$Income, spend$Expenditure, main = "Income vs. Expenditure", xlab = "Income", ylab = "Expenditure")

# b. fit slope for the least squares regression line
lm1fit = lm(Expenditure ~ Income, data = spend)
# slope = 0.06894

# c. intercept from least squares line = -151.26509