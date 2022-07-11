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
nnetTune = train(solTrainXtrans, soltrainY,
                 method = "avNNet",
                 tuneGrid = nnetGrid,
                 trControl = ctrl,
                 preProcess = c("center", "scale"),
                 linout = TRUE,
                 trace = FALSE,
                 MaxNWts = 13*(ncol(solTrainXtrans) + 1) + 13 + 1,
                 maxit = 1000,
                 allowParallel = FALSE)
















