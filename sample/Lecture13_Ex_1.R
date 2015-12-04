# random Forests for Prostate Data

prostate <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
#prostate <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)


y.1 <- as.matrix(prostate[which(set==1),10])
x.1 <- as.matrix(prostate[which(set==1),c(2:9)])

y.2 <- as.matrix(prostate[which(set==2),10])
x.2 <- as.matrix(prostate[which(set==2),c(2:9)])

x.all <- as.matrix(prostate[,c(2:9)])
y.all <- as.matrix(prostate[,10])

library(gbm)
####################################################################
## Gradient boosting through gbm() allows different distributions to be used for boosting 
## Tuning parameters and defaults include 
# n.trees=100: number of trees (=M)
# interaction.depth=1: implies additive model.  USUALLY need to increase this (HTF suggest 5)
# shrinkage = .001: The learning rate parameter (HTF: 0.1; author says .001 is better)
# bag.fraction=0.5: The subsampling fraction (HTF: 0.5)
## Crossvalidation using cv.folds= allows you to estimate the number of trees for your current
##   parameter settings to avoid overfitting
####################################################################


pro.boost.1.001 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=1, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(pro.boost.1.001, method="cv" ) #Make plot of training error and CV MSPE
cv.1.001 <- pro.boost.1.001$cv.error[gbm.perf(pro.boost.1.001, method="cv" )]
tr.1.001 <- pro.boost.1.001$train.error[gbm.perf(pro.boost.1.001, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.2.001 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=2, shrinkage=0.001, 
                 bag.fraction=0.4, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(2,2))
gbm.perf(pro.boost.2.001, method="cv" ) #Make plot of training error and CV MSPE 
cv.2.001 <-pro.boost.2.001$cv.error[gbm.perf(pro.boost.2.001, method="cv" )]
tr.2.001 <- pro.boost.2.001$train.error[gbm.perf(pro.boost.2.001, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.3.001 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=3, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.3.001, method="cv" ) #Make plot of training error and CV MSPE
cv.3.001 <-pro.boost.3.001$cv.error[gbm.perf(pro.boost.3.001, method="cv" )]
tr.3.001 <- pro.boost.3.001$train.error[gbm.perf(pro.boost.3.001, method="cv" )]

# Run these two functions several times to see change in CV suggestion
pro.boost.4.001 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=4, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.4.001, method="cv" ) #Make plot of training error and CV MSPE
cv.4.001 <-pro.boost.4.001$cv.error[gbm.perf(pro.boost.4.001, method="cv" )]
tr.4.001 <- pro.boost.4.001$train.error[gbm.perf(pro.boost.4.001, method="cv" )]

# Run these two functions several times to see change in CV suggestion
pro.boost.5.001 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.5.001, method="cv" ) #Make plot of training error and CV MSPE
cv.5.001 <-pro.boost.4.001$cv.error[gbm.perf(pro.boost.5.001, method="cv" )]
tr.5.001 <- pro.boost.5.001$train.error[gbm.perf(pro.boost.5.001, method="cv" )]

#################CHANGE SHRINKAGE

pro.boost.1.01 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=1, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(pro.boost.1.01, method="cv" ) #Make plot of training error and CV MSPE
cv.1.01 <-pro.boost.1.01$cv.error[gbm.perf(pro.boost.1.01, method="cv" )]
tr.1.01 <- pro.boost.1.01$train.error[gbm.perf(pro.boost.1.01, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.2.01 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=2, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.2.01, method="cv" ) #Make plot of training error and CV MSPE 
cv.2.01 <-pro.boost.2.01$cv.error[gbm.perf(pro.boost.2.01, method="cv" )]
tr.2.01 <- pro.boost.2.01$train.error[gbm.perf(pro.boost.2.01, method="cv" )]



# Run these two functions several times to see change in CV suggestion
pro.boost.3.01 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=3, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.3.01, method="cv" ) #Make plot of training error and CV MSPE
cv.3.01 <-pro.boost.3.01$cv.error[gbm.perf(pro.boost.3.01, method="cv" )]
tr.3.01 <- pro.boost.3.01$train.error[gbm.perf(pro.boost.3.01, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.4.01 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=500, interaction.depth=4, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.4.01, method="cv" ) #Make plot of training error and CV MSPE
cv.4.01 <-pro.boost.4.01$cv.error[gbm.perf(pro.boost.4.01, method="cv" )]
tr.4.01 <- pro.boost.4.01$train.error[gbm.perf(pro.boost.4.01, method="cv" )]



# Run these two functions several times to see change in CV suggestion
pro.boost.5.01 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=500, interaction.depth=5, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.5.01, method="cv" ) #Make plot of training error and CV MSPE
cv.5.01 <-pro.boost.5.01$cv.error[gbm.perf(pro.boost.5.01, method="cv" )]
tr.5.01 <- pro.boost.5.01$train.error[gbm.perf(pro.boost.5.01, method="cv" )]


#################CHANGE SHRINKAGE

pro.boost.1.1 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=1, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(pro.boost.1.1, method="cv" ) #Make plot of training error and CV MSPE
cv.1.1 <-pro.boost.1.1$cv.error[gbm.perf(pro.boost.1.1, method="cv" )]
tr.1.1 <- pro.boost.1.1$train.error[gbm.perf(pro.boost.1.1, method="cv" )]



# Run these two functions several times to see change in CV suggestion
pro.boost.2.1 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=2, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.2.1, method="cv" ) #Make plot of training error and CV MSPE 
cv.2.1 <-pro.boost.2.1$cv.error[gbm.perf(pro.boost.2.1, method="cv" )]
tr.2.1 <- pro.boost.2.1$train.error[gbm.perf(pro.boost.2.1, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.3.1 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=3, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.3.1, method="cv" ) #Make plot of training error and CV MSPE
cv.3.1 <-pro.boost.3.1$cv.error[gbm.perf(pro.boost.3.1, method="cv" )]
tr.3.1 <- pro.boost.3.1$train.error[gbm.perf(pro.boost.3.1, method="cv" )]

# Run these two functions several times to see change in CV suggestion
pro.boost.4.1 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=500, interaction.depth=4, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.4.1, method="cv" ) #Make plot of training error and CV MSPE
cv.4.1 <-pro.boost.4.1$cv.error[gbm.perf(pro.boost.4.1, method="cv" )]
tr.4.1 <- pro.boost.4.1$train.error[gbm.perf(pro.boost.4.1, method="cv" )]


# Run these two functions several times to see change in CV suggestion
pro.boost.5.1 <- gbm(data=prostate[,-c(1,11)], lpsa ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=500, interaction.depth=5, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)
gbm.perf(pro.boost.5.1, method="cv" ) #Make plot of training error and CV MSPE
cv.5.1 <-pro.boost.5.1$cv.error[gbm.perf(pro.boost.5.1, method="cv" )]
tr.5.1 <- pro.boost.5.1$train.error[gbm.perf(pro.boost.5.1, method="cv" )]


win.graph(h=7, w=6)
summary(pro.boost.3.001, n.trees=gbm.perf(pro.boost.5.1, method="cv" ))

win.graph(h=7,w=12,pointsize=12)
par(mfcol=c(2,3))
plot(pro.boost.3.001, i.var=c(1), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
plot(pro.boost.3.001, i.var=c(2), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
plot(pro.boost.3.001, i.var=c(3), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
win.graph(h=7,w=12,pointsize=12)
par(mfcol=c(2,3))
plot(pro.boost.3.001, i.var=c(4), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
plot(pro.boost.3.001, i.var=c(5), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
plot(pro.boost.3.001, i.var=c(6), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
win.graph(h=7,w=8,pointsize=12)
par(mfcol=c(2,2))
plot(pro.boost.3.001, i.var=c(7), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))
plot(pro.boost.3.001, i.var=c(8), n.trees=gbm.perf(pro.boost.5.1, method="cv" ))


