##########################
#### Notes Chapter 12 ####
##########################

library(AppliedPredictiveModeling)
library(caret)
library(ISLR)
library(pROC)
library(MASS)
library(glmnet)


###############################
#### The Stock Market Data ####
###############################

# data information
data("Smarket")
head(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)

# pairwise correlations for numerical predictors
cor(Smarket[,-9])
attach(Smarket)
plot(Smarket$Volume)

# data splitting
train = which(Year<2005)
Smarket.train = Smarket[train,]
Smarket.test = Smarket[-train,]

# create a control function that will be used across models
set.seed(100)
ctrl = trainControl(method = "LGOCV",
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE,
                    savePredictions = TRUE)


#############################
#### Logistic Regression ####
#############################
set.seed(476)
logisticTune = train(as.matrix(Smarket.train[,1:8]), Smarket.train$Direction,
                     method = "glm",
                     metric = "ROC",
                     trControl = ctrl)
logisticTune

# save the test set results in a data frame
testResults = data.frame(obs = Smarket.test$Direction,
                         logistic = predict(logisticTune, Smarket.test))

# predict the test set based on the logistic regression
Smarket.test$logistic = predict(logisticTune, Smarket.test,
                                type = "prob")[,1]

# ROC for logistic model
logisticROC = roc(Smarket.test$Direction, Smarket.test$logistic)
plot(logisticROC,
     col = 1,
     lty = 1,
     lwd = 2)

# confusion matrix of logistic model
confusionMatrix(data = predict(logisticTune, Smarket.test),
                reference = Smarket.test$Direction)


###################
#### LDA Model ####
###################

set.seed(476)
ldaTune = train(as.matrix(Smarket.train[,1:8]),
                Smarket.train$Direction,
                method = "lda",
                preProc = c("center", "scale"),
                metric = "ROC",
                trControl = ctrl)
ldaTune

# save the test set results in a data frame
testResults$LDA = predict(ldaTune, Smarket.test)


































