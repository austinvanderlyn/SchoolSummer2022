# Notes Chapter 13

library(AppliedPredictiveModeling)
library(caret)
library(ISLR)
library(pROC)
library(MASS)
library(klaR)
library(mda)
library(earth)
library(e1071)
library(kernlab)
data(Smarket)


# Data Splitting
train = which(Smarket$Year<2005)
Smarket.train = Smarket[train,]
Smarket.test = Smarket[-train,]


# create a control function that will be used across models
set.seed(123)
ctrl = trainControl(method = "LGOCV",
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE,
                    savePredictions = TRUE)


#########################################
#### Quadratic Discriminant Analysis ####
















