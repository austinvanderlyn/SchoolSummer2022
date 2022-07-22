library(AppliedPredictiveModeling)
library(caret)
library(ISLR)
library(tree)
library(rpart)
library(partykit)


# Example: Baseball player salary data (hitters)
data(Hitters)
Hitters1 = na.omit(Hitters)
par(mfrow = c(1,2))
hist(Hitters1$Salary,
     prob = T,
     main = "Salary")
hist(log(Hitters1$Salary),
     prob = T,
     main = "log(Salary)",
     col = 2)
dev.off()


# Tree plot with three terminal nodes
sal.tree = tree(log(Salary) ~ Years + Hits,
                data = Hitters1)
sal.tree3 = prune.tree(sal.tree,
                       best = 3)
plot(sal.tree3)
text(sal.tree3,
     pretty = 0)
title("Baseball Player Salary Data")


# the three-region partition
rbPal = colorRampPalette(c('red', 'green'))
Hitters1$Col = rbPal(20)[as.numeric(cut(log(Hitters1$Salary),
                                        breaks = 10))]
plot(Hitters1$Years,
     Hitters1$Hits,
     pch = 15,
     xlab = "Years",
     ylab = "Hits",
     col = Hitters1$Col)
partition.tree(sal.tree3,
               add = T,
               cex = 1.2,
               col = "blue")


# without pruning
sal.tree = tree(log(Salary) ~ Years + Hits,
                data = Hitters1)
summary(sal.tree)
sal.tree
plot(sal.tree)
text(sal.tree,
     pretty = 0)
title("Baseball Player Salary Data")


# Pruning a tree by cv
set.seed(1)
sal.tree0 = tree(log(Salary) ~ Years + Hits,
                 data = Hitters1)
my.tree.seq = cv.tree(sal.tree0)
plot(my.tree.seq)
opt.trees = which(my.tree.seq$dev == min(my.tree.seq$dev))
# position of optimal (w/r/t error) trees
min(my.tree.seq$size[opt.trees])


# load the data
data(solubility)


# create a control function that will be used across models. We create the fold assignments explicitly instead of
# relying on the random number seed being set to identical values
set.seed(100)
indx = createFolds(solTrainY,
                   returnTrain = TRUE)
ctrl = trainControl(method = "cv",
                    index = indx)


############################
## Basic Regression Trees ##
############################

library(rpart)
library(caret)
ptm = proc.time()
set.seed(100)
cartTune = train(solTrainXtrans, solTrainY,
                 method = "rpart",
                 tuneLength = 25,
                 trControl = ctrl)
cartTune
cartTune$finalModel
proc.time()

# plot the tuning results
# cross-validated RMSE profile for the regression tree
plot(cartTune,
     scales = list(x = list(log = 10)))

# use the partykit package to make 














