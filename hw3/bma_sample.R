# Bayesian model averaging via BIC using BMA package

prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")

library(BMA)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

# Model averaging a linear model is done with bicreg().  
# Other functions can handle other regression models
#  Calculations are based on leaps() all-subsets regression with BIC on each model
#  Probabilities are found as shown in lecture notes
# 
# OR is a number that determines how many models are considered in calculating the model probabilities.
#  If the best model has probability Pmax, then any model with probability < Pmax/OR is not considered.
# strict = TRUE enforces greater parsimony by eliminating models that have submodels with higher probability.


mod.avg1 <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 80)
mod.avg2 <- bicreg(x = x.2, y = y.2, strict = FALSE, OR = 80)

summary(mod.avg1)
summary(mod.avg2)

quartz(h=7, w=10, pointsize=12)
plot(mod.avg1)
quartz(h=7, w=10, pointsize=12)
plot(mod.avg2)

quartz(h=5, w=10, pointsize=12)
par(mfrow=c(1,2))
imageplot.bma(mod.avg1)
imageplot.bma(mod.avg2)