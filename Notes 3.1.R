library(AppliedPredictiveModeling)
data("segmentationOriginal")

# Access cell segmentation data
# retain the original training set
segTrain = subset(segmentationOriginal, Case == "Train")

# remove the first three columns (identifier colomns)
segTrainX = segTrain[, -(1:3)]
segTrainClass = segTrain$Class

# rule of thumbs > 20
max(segTrainX$VarIntenCh3) / min(segTrainX$VarIntenCh3)

# calculate the skewness of a predictor
library(e1071)
skewness(segTrainX$VarIntenCh3)

# Box-cox transformation
library(caret)
BoxCoxTrans(segTrainX$VarIntenCh3)

# apply the transformations for predictor VarIntenCh3
VarIntenCh3BoxCox = BoxCoxTrans(segTrainX$VarIntenCh3)
VarIntenCh3Trans = predict(VarIntenCh3BoxCox, segTrainX$VarIntenCh3)

# histogram comparisons before and after transformation
histogram(segTrainX$VarIntenCh3, xlab = "Natural Units", type = "count", main = "Original")
histogram(VarIntenCh3Trans, xlab = "Log Units", type = )














