# Exercise 1

# Read in data
spend = read.delim("C:/Users/austi/OneDrive/Desktop/UTSA/SchoolSummer2022/Income.txt", header = TRUE, sep = "")
spend = spend[order(spend[,3],decreasing=FALSE),]

# a. scatter plot
plot(spend$Income, spend$Expenditure, main = "Income vs. Expenditure", xlab = "Income", ylab = "Expenditure")

# b. fit slope for the least squares regression line
lm1fit = lm(Expenditure ~ Income, data = spend)
# slope = 0.06894

# c. intercept from least squares line = -151.26509

# d. find equation for least squares regression line: Y = 0.06894(X) - 151.265

# e. find R^2, or proportion of the variation that can be explained by the equation
summary(lm1fit)

# f. find sigma^2
lm1_summary = summary(lm1fit)
sigma = lm1_summary$sigma
lm1_var = sigma^2
lm1_var

# g. check for outliers and influential points
lm1.res = resid(lm1fit)
par(mfrow = c(1,2))
plot(spend$Expenditure, lm1.res)
# it does look like there's one outlier way off to the right
boxplot(lm1.res)
# the box plot also shows one extreme outlier

# h. linear model with 10-fold cv. draw scatter plot with fitted line and scatter with observed vs. predicted
set.seed(1)
lm2 = train(Expenditure ~ Income,
            data = spend,
            method = "lm",
            trControl = trainControl(
              method = "cv",
              number = 10
            ))
lm2
summary(lm2)

lm2.obs = spend$Expenditure
lm2.preds = fitted(lm2)

par(mfrow = c(1,2))
plot(spend$Income, spend$Expenditure,
     xlab = "Income",
     ylab = "Expenditure")
lines(spend$Income, fitted(lm2), col = 2, lwd = 2)
plot(lm2.obs, lm2.preds,
     xlab = "Observed",
     ylab = "Predicted")

# i. a)	Fit a quadratic model and conduct 10-fold CV to estimate the error and draw the scatter plot
#       with the fitted line and the scatter plot between the observed and fitted values.
# square the independent variable
spend$Income2 = spend$Income^2
set.seed(1)
# quadratic model
quad = train(Expenditure ~ Income + Income2,
            data = spend,
            method = "lm",
            trControl = trainControl(
              method = "cv",
              number = 10
            ))
quad
summary(quad)

# quality of fit diagnostics
quad.obs = spend$Expenditure
quad.preds = fitted(quad)

par(mfrow = c(1,2))
plot(spend$Income, spend$Expenditure,
     xlab = "Income",
     ylab = "Expenditure")
lines(spend$Income, fitted(quad), col = 2, lwd = 2)
plot(quad.obs, quad.preds,
     xlab = "Observed",
     ylab = "Predicted")


# j. a)	Fit a mars model with optimal tuning parameters that you choose and conduct 10-fold CV
#       to estimate the error and draw the scatter plot with the fitted line and the scatter plot
#       between the observed and fitted values.
set.seed(1)
library(earth)
marsfit = train(Expenditure ~ Income,
                data = spend,
                method = "earth",
                tuneLength = 15,
                trControl = trainControl(
                  method = "cv",
                  number = 10
                ))
marsfit
summary(marsfit)

# determine the best tuning parameter
plot(marsfit)

marsfit = train(Expenditure ~ Income,
                data = spend,
                method = "earth",
                tuneLength = 3,
                trControl = trainControl(
                  method = "cv",
                  number = 10
                ))
marsfit
summary(marsfit)

mars.obs = spend$Expenditure
mars.preds = fitted(marsfit)

par(mfrow = c(1,2))
plot(spend$Income, spend$Expenditure,
     xlab = "Income",
     ylab = "Expenditure")
lines(spend$Income, fitted(marsfit), col = 2, lwd = 2)
plot(mars.obs, mars.preds,
     xlab = "Observed",
     ylab = "Predicted")


# k. Compare the three fitted models in terms of RMSE and R2, and then make a recommendation
#    based on your criteria.
lm2rmse = sqrt(mean((lm2.obs - lm2.preds)^2))
quadrmse = sqrt(mean((quad.obs - quad.preds)^2))
marsrmse = sqrt(mean((mars.obs - mars.preds)^2))

lm2r2 = (summary(lm2))$r.squared
quadr2 = (summary(quad))$r.squared
marsr2 = (summary(marsfit))$r.squared

tab = data.frame("Linear" = c("RMSE" = lm2rmse, "R^2" = lm2r2), "Quadratic" = c(quadrmse, quadr2), "Mars" = c(marsrmse, marsr2))
tab
