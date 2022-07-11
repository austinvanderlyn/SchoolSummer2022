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
