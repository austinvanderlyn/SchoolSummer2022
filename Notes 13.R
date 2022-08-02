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

###########################################
#### Regularized Discriminant Analysis ####
###########################################

set.seed(123)
RDATune = train(x = as.matrix(Smarket.train[,1:8]),
                y = Smarket.train$Direction,
                method = "rda",
                preProc = c("center", "scale"),
                metric = "ROC",
                trControl = ctrl)
RDATune


#######################################
#### Mixture Discriminant Analysis ####
#######################################

set.seed(123)
MDATune = train(as.matrix(Smarket.train[,1:8]),
                Smarket.train$Direction,
                method = "mda",
                tuneGrid = expand.grid(.subclasses = 3:10),
                metric = "ROC",
                trControl = ctrl)
MDATune


#####################
#### Naive Bayes ####
#####################

set.seed(123)
NBTune = train(as.matrix(Smarket.train[,1:8]),
               Smarket.train$Direction,
               method = "nb",
               preProc = c("center", "scale"),
               metric = "ROC",
               trControl = ctrl)
NBTune


#############################
#### K-Nearest Neighbors ####
#############################

set.seed(123)
KNNTune = train(as.matrix(Smarket.train[,1:8]),
                Smarket.train$Direction,
                method = "knn",
                preProc = c("center", "scale"),
                metric = "ROC",
                tuneGrid = data.frame(.k = seq(1, 400, by = 10)),
                trControl = ctrl)
KNNTune


#########################
#### Neural Networks ####
#########################

set.seed(123)
nnetGrid = expand.grid(.size = 1:10,
                       .decay = c(0, .1, 0, 2))
maxsize = max(nnetGrid$.size)
numWts = 200
NNTune = train(as.matrix(Smarket.train[,1:8]),
               Smarket.train$Direction,
               method = "nnet",
               preProc = c("center", "scale"),
               metric = "ROC",
               tuneGrid = nnetGrid,
               trace = FALSE,
               maxit = 2000,
               MaxNWts = numWts,
               trControl = ctrl)
NNTune
plot(NNTune)


########################################
#### Flexible Discriminant Analysis #### 
########################################

set.seed(1123)
FDATune = train(as.matrix(Smarket.train[,1:8]),
                Smarket.train$Direction,
                method = "fda",
                preProc = c("center", "scale"),
                metric = "ROC",
                trControl = ctrl)
FDATune


#################################
#### Support Vector Machines ####
#################################

set.seed(123)
sigmaRangeReduced = sigest(as.matrix(Smarket.train[,1:8]))
svmRGridReduced = expand.grid(.sigma = sigmaRangeReduced[1],
                              .C = 2^(seq(-4, 4)))
SVMTune = train(as.matrix(Smarket.train[,1:8]),
                Smarket.train$Direction,
                method = "svmRadial",
                metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = svmRGridReduced,
                fit = FALSE,
                trControl = ctrl)
SVMTune                


####################
#### ROC Curves ####
####################

# predict the test set based on 8 models
# QDA
Smarket.test$QDA = predict(QDATune, Smarket.test,
                           type = "prob")[,1]

# RDA
Smarket.test$RDA = predict(RDATune, Smarket.test,
                           type = "prob")[,1]

# MDA
Smarket.test$MDA = predict(MDATune, Smarket.test,
                           type = "prob")[,1]

# NB
Smarket.test$NB = predict(NBTune, Smarket.test,
                           type = "prob")[,1]

# KNN
Smarket.test$KNN = predict(KNNTune, Smarket.test,
                           type = "prob")[,1]

# NN
Smarket.test$NN = predict(NNTune, Smarket.test,
                           type = "prob")[,1]

# FDA
Smarket.test$FDA = predict(FDATune, Smarket.test,
                           type = "prob")[,1]

# SVM
Smarket.test$SVM = predict(SVMTune, Smarket.test[,1:8],
                           type = "prob")[,1]


# ROC for QDA
QDAROC = roc(Smarket.test$Direction, Smarket.test$QDA)
plot(QDAROC, col = 1, lty = 1, lwd = 2)

# ROC for FDA
FDAROC = roc(Smarket.test$Direction, Smarket.test$FDA)
plot(FDAROC, col = 2, lty = 2, lwd = 2)

# ROC for MDA
MDAROC = roc(Smarket.test$Direction, Smarket.test$MDA)
plot(MDAROC, col = 3, lty = 3, lwd = 2)

# ROC for NB
NBROC = roc(Smarket.test$Direction, Smarket.test$NB)
plot(NBROC, col = 4, lty = 4, lwd = 2)




















