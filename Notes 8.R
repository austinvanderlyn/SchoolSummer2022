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





















