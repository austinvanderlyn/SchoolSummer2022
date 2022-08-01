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
#########################################

QDATune = train(x = as.matrix(Smarket.train[,1:8]),
                y = Smarket.train$Direction,
                method = "qda",
                metric = "ROC",
                trControl = ctrl)
QDATune


# library
# predict the test set based on the logistic regression
Smarket.test$QDA = predict(QDATune, Smarket.test,
                           type = "prob")[,1]

# ROC for QDA model
QDAROC = roc(Smarket.test$Direction, Smarket.test$QDA)
plot(QDAROC, col=1, lty=1, lwd=2)

# confusion matrix of QDA model
confusionMatrix(data = predict(QDATune, Smarket.test),
                reference = Smarket.test$Direction)











