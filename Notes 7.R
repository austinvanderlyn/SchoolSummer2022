# Chapter 7


# required libraries
library(AppliedPredictiveModeling)
library(caret)
library(earth)

# load the data
data(solubility)


# create a control function that will be used across models. We create the fold assignments explicitly instead of relying
# on the random number seed being set to identical values
set.seed(100)
indx = createFolds(solTrainY,
                   returnTrain = TRUE)
ctrl = trainControl(method = "cv",
                    index = indx)


#####################
## Neural Networks ##
#####################

# create a grid for tuning parameters
nnetGrid = expand.grid(decay = c(0, 0.01, 0.1),
                       size = c(1, 3, 5, 7),
                       bag = FALSE)

# it takes time to run
# the following codes take more than 6,000 seconds to run
ptm = proc.time()
set.seed(100)
nnetTune = train(solTrainXtrans, solTrainY,
                 method = "avNNet",
                 tuneGrid = nnetGrid,
                 trControl = ctrl,
                 preProcess = c("center", "scale"),
                 linout = TRUE,
                 trace = FALSE,
                 MaxNWts = 13*(ncol(solTrainXtrans) + 1) + 13 + 1,
                 maxit = 1000,
                 allowParallel = FALSE)
nnetTune
ptm

plot(nnetTune)

# save the predicted values into testResults
testResults3 = data.frame(obs = solTestY,
                          NNet = predict(nnetTune, solTestXtrans))


##############################################
## Multivariate Adaptive Regression Splines ##
##############################################

ptm = proc.time()
set.seed(100)
marsTune = train(solTrainXtrans, solTrainY,
                 method = "earth",
                 tuneGrid = expand.grid(degree = 1,
                                        nprune = 2:38),
                 trControl = ctrl)
marsTune
ptm

plot(marsTune)

# check the importance of each predictor
marsImp = varImp(marsTune,
                 scale = FALSE)
plot(marsImp, top = 10)

# save the predicted values into testResults
testResults3$MARS = predict(marsTune, solTestXtrans)


#############################
## Support Vector Machines ##
#############################

# SVM with the radial basis function
set.seed(100)
svmRTune = train(solTrainXtrans, solTrainY,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 14,
                 trControl = ctrl)
svmRTune

plot(svmRTune,
     scales = list(x = list(log = 2)))

# save the predicted values into testResults
testResults3$SVMr = predict(svmRTune, solTestXtrans)

# svm with the polynomial basis function
svmGrid = expand.grid(degree = 1:2,
                      scale = c(0.01, 0.005, 0.001),
                      C = 2^(-2:5))
set.seed(100)
scmPTune = train(solTrainXtrans, solTrainY,
                 method = "svmPoly",
                 preProc = c("center", "scale"),
                 tuneGrid = svmGrid,
                 trControl = ctrl)
scmPTune























