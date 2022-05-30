####################################################
#### Section 2.1 Case Study: Predicting Fuel Economy
####################################################

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)

# access the fuel economy data
data(FuelEconomy)
names(cars2010)

# access the predictor EngDispl
cars2010$EngDispl

# format data for plotting against engine displacement

# sort by engine displacement
cars2010 = cars2010[order(cars2010$EngDispl),]
cars2011 = cars2011[order(cars2011$EngDispl),]

# combine data into one data frame
cars2010a = cars2010
cars2010a$Year = "2010 Model Year"
cars2011a = cars2011
cars2011a$Year = "2011 Model Year"

plotData = rbind(cars2010a, cars2011a)
# compactly display the internal structure of data
str(plotData)

library(lattice)
xyplot(FE ~ EngDispl|Year, plotData,
       xlab = "Engine Displacement", 
       ylab = "Fuel Efficiency (MPG)",
       between = list(x = 1.2))


###########################################################################
#### Fit a single linear model and conduct 10-fold CV to estimate the error
###########################################################################

library(caret)
set.seed(1)

# use the cars2010 as training data to build up a linear model
lm1fit = train(FE ~ EngDispl,
               data = cars2010,
               method = "lm",
               trControl = trainControl(method = "cv"))
summary(lm1fit)

# fitted linear regression line
#efficiency = 50.5632 - 4.5209*displacement

# quality of fit diagnostics
par(mfrow = c(1,2))
plot(cars2010$EngDispl, cars2010$FE, 
     xlab = "Engine Displacement",
     ylab = "Fuel Efficiency (MPG)")
lines(cars2010$EngDispl, fitted(lm1fit), col = 2, lwd = 2)

Observed = cars2010$FE
Predicted = fitted(lm1fit)
plot(Observed, Predicted, ylim = c(12, 70))


##############################
#### Fit a quadratic model too
##############################

# create squared terms
displacement = cars2010$EngDispl
cars2010$displacement2 = cars2010$EngDispl^2
cars2011$displacement2 = cars2011$EngDispl^2

set.seed(1)
lm2fit = train(FE ~ EngDispl + displacement2,
               data = cars2010,
               method = "lm",
               trControl = trainControl(method = "cv"))
summary(lm2fit)

# quality of fit diagnostics
par(mfrow = c(1,2))
plot(cars2010$EngDispl, cars2010$FE, 
     xlab = "Engine Displacement",
     ylab = "Fuel Efficiency (MPG)")
lines(cars2010$EngDispl, fitted(lm2fit), col = 2, lwd = 2)

Observed = cars2010$FE
Predicted = fitted(lm2fit)
plot(Observed, Predicted, ylim = c(12, 70))


#####################
#### Fit a MARS model
#####################

install.packages("earth")
library(earth)
set.seed(1)
marsfit = train(FE ~ EngDispl,
                data = cars2010,
                method = "earth",
                tuneLength = 15,
                trControl = trainControl(method = "cv"))
marsfit
summary(marsfit)

# determine the tuning parameter for MARS model
plot(marsfit)

par(mfrow = c(1,2))
# quality of fit diagnostics
plot(cars2010)