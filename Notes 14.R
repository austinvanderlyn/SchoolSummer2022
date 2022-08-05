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

set.seed(123)
rpartTune = train(x = as.matrix(Smarket.train[,1:8]),
                  y = Smarket.train$Direction,
                  method = "rpart",
                  tuneLength = 30,
                  metric = "ROC",
                  trControl = ctrl)
rpartTune
plot(rpartTune)



######################
#### Bagged Trees ####
######################

set.seed(123)
treebagTune = train(x = as.matrix(Smarket.train[,1:8]),
                    y = Smarket.train$Direction,
                    method = "treebag",
                    nbagg = 50,
                    trControl = ctrl)
treebagTune





