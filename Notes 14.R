# Notes Chapter 14

# libraries
library(AppliedPredictiveModeling)
library(caret)
library(ISLR)
library(rpart)
library(ipred)
library(gbm)
library(randomForest)

# access the data
attach(Smarket)

# data splitting
train = which(Smarket$Year < 2005)
Smarket.train = Smarket[train,]
Smarket.test = Smarket[-train,]

# create a control function that will be used across models
set.seed(123)
ctrl = trainControl(method = "LGOCV",
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE,
                    savePredictions = TRUE)


##############################
#### Classification Trees ####
##############################













