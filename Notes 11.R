##########################
#### Notes Chapter 11 ####
##########################


library(AppliedPredictiveModeling)
library(randomForest)

# simulate some two class data with two predictors
set.seed(975)
training = quadBoundaryFunc(500)
testing = quadBoundaryFunc(1000)
testing$class2 = ifelse(testing$class == "Class1", 1, 0)
testing$ID = 1:nrow(testing)

# scatter plot
xyplot(X1 ~ X2,
       groups = class,
       data = training)
# now going to fit three models: QDA, RM, and logistic


###############################
#### RF, QDA, and Logistic ####
###############################

library(MASS)
set.seed(123)
qdafit = qda(class ~ X1 + X2,
             data = training)

library(randomForest)
set.seed(123)
rfFit = randomForest(class ~ X1 + X2,
                     data = training,
                     ntree = 2000)

glmFit = train(class ~ X1 + X2,
               data = training,
               method = "glm",
               trControl = trainControl(method = "repeatedcv",
                                        repeats = 5))

# predict the test set based on three models
testing$qda = predict(qdafit, testing)$posterior[,1]
testing$rf = predict(rfFit, testing, type = "prob")[,1]
testing$glm = predict(glmFit, testing, type = "prob")[,1]

# create the confusion matrix from the test set

# confusion matrix of qda fit
confusionMatrix(data = predict(qdafit,
                               testing,
                               type = "prob")$class,
                reference = testing$class)

# confusion matrix of rf
confusionMatrix(data = predict(rfFit,
                               testing),
                reference = testing$class)

# confusion matrix of glm
confusionMatrix(data = predict(glmFit,
                               testing),
                reference = testing$class)


#####################################
#### ROC Curves for three models ####
#####################################

library(pROC)
par(mfrow = c(2,2))
# ROC for QDA
qdaROC = roc(testing$class,
             testing$qda)
plot(qdaROC,
     col = 1,
     lty = 1)

# ROC for RF
rfROC = roc(testing$class,
             testing$rf)
plot(rfROC,
     col = 2,
     lty = 2)

# ROC for glm
glmROC = roc(testing$class,
            testing$glm)
plot(glmROC,
     col = 3,
     lty = 3)










