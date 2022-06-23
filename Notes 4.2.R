############
## Notes 4.2
############

data(twoClassData)
str(predictors)
str(classes)

# split the data into training (80%) and testing (20%) sets
set.seed(100)
x = cbind(predictors, classes)
inTrain = createDataPartition(classes, p = 0.8)[[1]]
ClassTrain = x[inTrain, ]
ClassTest = x[-inTrain, ]

# hyperparameter estimation for the Gaussian Radial Basis Kernel
library(kernlab)
set.seed(231)

#frac: fraction of the data to use for estimation
# by default a quarter of the data is used to estimate range of sigma hyperparameter
sigDist = sigest(classes ~ ., 
                 data = ClassTrain,
                 frac = 1)
sigDist
svmTuneGrid = data.frame(sigma = as.vector(sigDist)[1],
                         C = 2^(-2:7))

##########################
## SVM with svmTuneGrid ##
##########################

set.seed(1056)
svmFit = train(classes ~ .,
               data = ClassTrain,
               method = "svmRadial",
               preProc = c("center", "scale"),
               tuneGrid = svmTuneGrid,
               trControl = trainControl(method = "repeatedcv",
                                        repeats = 5,
                                        classProbs = TRUE))
# classProbs = TRUE was addded since the text was written

# print results
svmFit

# a line plot of the average performance
# the 'scales' argument is actually an argument to xy plot that converts the 
# axis to log-2 unnits
plot(svmFit,
     scales = list(x = list(log = 2)))

# test set predictions
predictedClasses = predict(svmFit, ClassTest)
str(predictedClasses)

# use the "type" option to get class probabilities
predictedProbs = predict(svmFit,
                         newdata = ClassTest,
                         type = "prob")
head(predictedProbs)

# obtain the prediction class labels
predictedProbs = predict(svmFit,
                         newdata = ClassTest)
head(predictedProbs)


################################
## 10 - Fold cross validation ##
################################

set.seed(1056)
svmFit10CV = train(classes ~ .,
                   data = ClassTrain,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = svmTuneGrid,
                   trControl = trainControl(method = "cv",
                                            number = 10))
svmFit10CV

# LOOCV
set.seed(1056)
svmFitLOO = train(classes ~ .,
                  data = ClassTrain,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid,
                  trControl = trainControl(method = "LOOCV"))
svmFitLOO


###########
## LOOCV ##
###########

set.seed(1056)
svmFitLOO = train(classes ~ .,
                  data = ClassTrain,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid,
                  trControl = trainControl(method = "LOOCV"))
svmFitLOO


###################
## The Bootstrap ##
###################

set.seed(1056)
svmFitBoot = train(classes ~ .,
                  data = ClassTrain,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid,
                  trControl = trainControl(method = "boot",
                                           number = 50))
svmFitBoot


###############################
## Between Model Comparisons ##
###############################

set.seed(1056)
svmFit = train(classes ~ .,
                  data = ClassTrain,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 5,
                                           classProbs = TRUE))
svmFit


set.seed(1056)
glmProfile = train(classes ~ .,
                  data = ClassTrain,
                  method = "glm",
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 5))
glmProfile

# compare the SVM and logistic regression models using resamples function
resamp = resamples(list(SVM = svmFit, Logistic = glmProfile))
summary(resamp)
modelDifferences = diff(resamp)
summary(modelDifferences)

# the actual pair t-test
modelDifferences$statistics$Accuracy




































