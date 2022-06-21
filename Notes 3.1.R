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
histogram(VarIntenCh3Trans, xlab = "Log Units", type = "count", main = "Log-Transformation")

# Box-cox transformation for another predictor Perimch1
BoxCoxTrans(segTrainX$PerimCh1)

# apply the transformations for predictor VarIntenCh3
PerimCh1Box = BoxCoxTrans(segTrainX$PerimCh1)
PerimCh1Trans = predict(PerimCh1Box, segTrainX$PerimCh1)

# histogram comparisons before and after transformation
histogram(segTrainX$PerimCh1, xlab = "Natural Units",
          type = "count",
          main = "Original")
histogram(PerimCh1Trans, xlab = "Inverse Units",
          type = "count",
          main = "Inverse Transformation")



#############################
#### R Demonstration 3.2 ####
#############################

# R codes for PCA
library(caret)

# use caret's preprocess function to transform for skewness
segPP = preProcess(segTrainX, method = "BoxCox")

# apply the transformations
segTrainTrans = predict(segPP, segTrainX)

# R's prcomp is used to conduct PCA
pr = prcomp(~AvgIntenCh1 + EntropyIntenCh1,
            data = segTrainTrans,
            scale. = TRUE)

xyplot(AvgIntenCh1 ~ EntropyIntenCh1, data = segTrainTrans,
       groups = segTrain$Class,
       xlab = "Channel 1 Fiber Width",
       ylab = "Intensity Entropy Channel 1",
       auto.key = list(columns = 2),
       type = c("p", "g"),
       main = "Original Data",
       aspect = 1)

# check if we specify auto.key = FALSE
windows()
xyplot(AvgIntenCh1 ~ EntropyIntenCh1, data = segTrainTrans,
       groups = segTrain$Class,
       xlab = "Channel 1 Fiber Width",
       ylab = "Intensity Entropy Channel 1",
       auto.key = FALSE,
       type = c("p", "g"),
       main = "Original Data",
       aspect = 1)

windows()
xyplot(AvgIntenCh1 ~ EntropyIntenCh1, data = segTrainTrans,
       groups = segTrain$Class,
       xlab = "Channel 1 Fiber Width",
       ylab = "Intensity Entropy Channel 1",
       auto.key = list(columns = 1),
       type = c("p", "g"),
       main = "Original Data",
       aspect = 1)

xyplot(PC2 ~ PC1,
       data = as.data.frame(pr$x),
       groups = segTrain$Class,
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Transformed",
       xlim = extendrange(pr$x),
       ylim = extendrange(pr$x),
       type = c("p", "g"), aspect = 1)

# fit PCA to entire set of segmentation data
# there are a few predictors with only a single value, so we remove these first
# since PCA uses variances, which would be zero
isZV = apply(segTrainX, 2, function(x) length(unique(x)) == 1)

# identify the predictor with a single value
segTrainX = segTrainX[, !isZV]

segPP = preProcess(segTrainX, c("BoxCox", "center", "scale"))
segTrainTrans = predict(segPP, segTrainX)

segPCA = prcomp(segTrainTrans, center = TRUE, scale. = TRUE)

# scree plot
PTotalVariance = (segPCA$sdev^2)/sum(segPCA$sdev^2)*100
ts.plot(PTotalVariance, xlab = "Component", ylab = "Percent of Total Variance")
points(PTotalVariance, col = 2)

# plot a scatterplot matrix of the first three components
panelRange = extendrange(segPCA$x[, 1:3])
splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)

# R codes for removing predictors
# near zero variance predictor
nearZeroVar(segTrainTrans)

# remove the near zero variance predictor
segTrainTrans1 = segTrainTrans[, -nearZeroVar(segTrainTrans)]
nearZeroVar(segTrainTrans1)

# to filter on correlations, we first get the correlation matrix for the predictor set
segCorr = cor(segTrainTrans1)
library(corrplot)
corrplot(segCorr, order = "hclust", tl.cex = .35)

# caret's findcorrelation function is used to identify columns to remove
highCorr = findCorrelation(segCorr, .75)
highCorr

# R codes for removing highly correlated predictors
segCorr1 = cor(segTrainTrans1[, -highCorr])
corrplot(segCorr1, order = "hclust", tl.cex = .35)

# R codes for creating dummy variables
data(cars)
type = c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$type = factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))
carSubset = cars[sample(1:nrow(cars), 20), c(1, 2, 19)]
head(carSubset)
levels(carSubset$type)
simpleMod = dummyVars(~ Mileage + type,
                      data = carSubset,
                      levelsOnly = TRUE)
simpleMod
predict(simpleMod, carSubset)

# R codes for creating dummy variables with interaction
withInteraction = dummyVars(~ Mileage + type + Mileage:type,
                            data = carSubset, 
                            levelsOnly = TRUE)
withInteraction
predict(withInteraction, carSubset)
