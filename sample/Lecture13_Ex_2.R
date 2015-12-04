# Gradient Boosting for Abelone Data
###### Warning: Takes HOURS to run!

abelone <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\abelone.csv", header=TRUE, sep=",", na.strings=" ")

#abelone$sex.m = ifelse(abelone$Sex==1, yes=1, no=0)
#abelone$sex.f = ifelse(abelone$Sex==2, yes=1, no=0)


abelone$Sex <- as.factor(abelone$Sex)
head(abelone)

#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test

set.seed(29003092)
U <- runif(n=nrow(abelone))
set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))
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


abe.boost.1.001.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=100000, interaction.depth=1, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.001.5, method="cv" ) #Make plot of training error and CV MSPE
cv.1.001.5 <- abe.boost.1.001.5$cv.error[gbm.perf(abe.boost.1.001.5, method="cv" )]
val.1.001.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.001.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.1.001.5.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=50000))^2)
val.1.001.5.70 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=70000))^2)
val.1.001.5.90 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=90000))^2)
opt.val.1.001.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.001.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.01.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=20000, interaction.depth=1, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.01.5, method="cv" ) #Make plot of training error and CV MSPE
cv.1.01.5 <- abe.boost.1.01.5$cv.error[gbm.perf(abe.boost.1.01.5, method="cv" )]
val.1.01.5.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.01.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.01.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.1.01.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.1.01.5.40 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=40000))^2)
val.1.01.5.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=50000))^2)
opt.val.1.01.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.01.5, method="cv" )))^2)

# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.02.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=1, shrinkage=0.02, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.02.5, method="cv" ) #Make plot of training error and CV MSPE
cv.1.02.5 <- abe.boost.1.02.5$cv.error[gbm.perf(abe.boost.1.02.5, method="cv" )]
val.1.02.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.02.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.02.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.1.02.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.1.02.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.02.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.05.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=1, shrinkage=0.05, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.05.5, method="cv" ) #Make plot of training error and CV MSPE
cv.1.05.5 <- abe.boost.1.05.5$cv.error[gbm.perf(abe.boost.1.05.5, method="cv" )]
val.1.05.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.05.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
opt.val.1.05.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.05.5, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.1.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=1, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.1.5, method="cv" ) #Make plot of training error and CV MSPE
cv.1.1.5 <- abe.boost.1.1.5$cv.error[gbm.perf(abe.boost.1.1.5, method="cv" )]
val.1.1.5.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.5,newdata=abelone[which(set==2),], n.trees=1000))^2)
val.1.1.5.02 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.5,newdata=abelone[which(set==2),], n.trees=2000))^2)
val.1.1.5.03 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.5,newdata=abelone[which(set==2),], n.trees=3000))^2)
opt.val.1.1.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.1.5, method="cv" )))^2)

#############################################
# Increase interaction depth to 3
#############################################

abe.boost.3.001.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=50000, interaction.depth=3, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.001.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.001.5 <- abe.boost.3.001.5$cv.error[gbm.perf(abe.boost.3.001.5, method="cv" )]
val.3.001.5.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.3.001.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.3.001.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.3.001.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.3.001.5.40 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=40000))^2)
val.3.001.5.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=50000))^2)
opt.val.3.001.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.001.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.01.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=3, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.01.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.01.5 <- abe.boost.3.01.5$cv.error[gbm.perf(abe.boost.3.01.5, method="cv" )]
val.3.01.5.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.3.01.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.3.01.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.3.01.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.3.01.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.01.5, method="cv" )))^2)

# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.02.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=3, shrinkage=0.02, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.02.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.02.5 <- abe.boost.3.02.5$cv.error[gbm.perf(abe.boost.3.02.5, method="cv" )]
val.3.02.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.02.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
opt.val.3.02.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.02.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.02.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.05.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=2000, interaction.depth=3, shrinkage=0.05, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.05.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.05.5 <- abe.boost.3.05.5$cv.error[gbm.perf(abe.boost.3.05.5, method="cv" )]
val.3.05.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.05.5,newdata=abelone[which(set==2),], n.trees=000))^2)
opt.val.3.05.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.05.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.05.5, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.1.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=3, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.1.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.1.5 <- abe.boost.3.1.5$cv.error[gbm.perf(abe.boost.3.1.5, method="cv" )]
val.3.1.5.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.1.5,newdata=abelone[which(set==2),], n.trees=1000))^2)
opt.val.3.1.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.1.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.1.5, method="cv" )))^2)


#############################################
# Increase interaction depth to 5
#############################################

abe.boost.5.001.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=50000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.001.5, method="cv" ) #Make plot of training error and CV MSPE
cv.5.001.5 <- abe.boost.5.001.5$cv.error[gbm.perf(abe.boost.5.001.5, method="cv" )]
val.5.001.5.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.001.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.5.001.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.5.001.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.5.001.5.40 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=40000))^2)
val.5.001.5.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=50000))^2)
opt.val.5.001.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.001.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.01.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=5, shrinkage=0.01, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.01.5, method="cv" ) #Make plot of training error and CV MSPE
cv.5.01.5 <- abe.boost.5.01.5$cv.error[gbm.perf(abe.boost.5.01.5, method="cv" )]
val.5.01.5.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.01.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.5.01.5.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.5,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.5.01.5.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.5,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.5.01.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.01.5, method="cv" )))^2)

# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.02.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=5, shrinkage=0.02, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.02.5, method="cv" ) #Make plot of training error and CV MSPE
cv.5.02.5 <- abe.boost.5.02.5$cv.error[gbm.perf(abe.boost.5.02.5, method="cv" )]
val.5.02.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.02.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
opt.val.5.02.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.02.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.02.5, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.05.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=5, shrinkage=0.05, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.05.5, method="cv" ) #Make plot of training error and CV MSPE
cv.5.05.5 <- abe.boost.5.05.5$cv.error[gbm.perf(abe.boost.5.05.5, method="cv" )]
val.5.05.5.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.05.5,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.05.5.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.05.5,newdata=abelone[which(set==2),], n.trees=10000))^2)
opt.val.5.05.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.05.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.05.5, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.1.5 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=5, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.1.5, method="cv" ) #Make plot of training error and CV MSPE
cv.5.1.5 <- abe.boost.5.1.5$cv.error[gbm.perf(abe.boost.5.1.5, method="cv" )]
val.5.1.5.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.5,newdata=abelone[which(set==2),], n.trees=1000))^2)
val.5.1.5.02 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.5,newdata=abelone[which(set==2),], n.trees=2000))^2)
val.5.1.5.03 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.5,newdata=abelone[which(set==2),], n.trees=3000))^2)
opt.val.5.1.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.5,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.1.5, method="cv" )))^2)

####################################################################
####################################################################
####################################################################
## Lower bagging fraction to .25
####################################################################
####################################################################
####################################################################


abe.boost.1.001.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=100000, interaction.depth=1, shrinkage=0.001, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.001.25, method="cv" ) #Make plot of training error and CV MSPE
cv.1.001.25 <- abe.boost.1.001.25$cv.error[gbm.perf(abe.boost.1.001.25, method="cv" )]
val.1.001.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.001.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.1.001.25.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=50000))^2)
val.1.001.25.70 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=70000))^2)
val.1.001.25.90 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=90000))^2)
opt.val.1.001.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.001.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.001.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.01.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=20000, interaction.depth=1, shrinkage=0.01, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.01.25, method="cv" ) #Make plot of training error and CV MSPE
cv.1.01.25 <- abe.boost.1.01.25$cv.error[gbm.perf(abe.boost.1.01.25, method="cv" )]
val.1.01.25.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.01.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.01.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.1.01.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.1.01.25.40 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=40000))^2)
val.1.01.25.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=50000))^2)
opt.val.1.01.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.01.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.01.25, method="cv" )))^2)

# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.02.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=1, shrinkage=0.02, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.02.25, method="cv" ) #Make plot of training error and CV MSPE
cv.1.02.25 <- abe.boost.1.02.25$cv.error[gbm.perf(abe.boost.1.02.25, method="cv" )]
val.1.02.25.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.02.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.1.02.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.1.02.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.1.02.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.02.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.02.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.05.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=10000, interaction.depth=1, shrinkage=0.05, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.05.25, method="cv" ) #Make plot of training error and CV MSPE
cv.1.05.25 <- abe.boost.1.05.25$cv.error[gbm.perf(abe.boost.1.05.25, method="cv" )]
val.1.05.25.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.1.05.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
opt.val.1.05.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.05.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.05.25, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.1.1.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=1, shrinkage=0.1, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.1.1.25, method="cv" ) #Make plot of training error and CV MSPE
cv.1.1.25 <- abe.boost.1.1.25$cv.error[gbm.perf(abe.boost.1.1.25, method="cv" )]
val.1.1.25.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.25,newdata=abelone[which(set==2),], n.trees=1000))^2)
val.1.1.25.02 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.25,newdata=abelone[which(set==2),], n.trees=2000))^2)
val.1.1.25.03 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.25,newdata=abelone[which(set==2),], n.trees=3000))^2)
opt.val.1.1.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.1.1.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.1.1.25, method="cv" )))^2)

#############################################
# Increase interaction depth to 3
#############################################

abe.boost.3.001.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=50000, interaction.depth=3, shrinkage=0.001, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.001.25, method="cv" ) #Make plot of training error and CV MSPE
cv.3.001.25 <- abe.boost.3.001.25$cv.error[gbm.perf(abe.boost.3.001.25, method="cv" )]
val.3.001.25.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.3.001.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.3.001.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.3.001.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
val.3.001.25.40 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=40000))^2)
val.3.001.25.50 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=50000))^2)
opt.val.3.001.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.001.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.001.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.01.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=3, shrinkage=0.01, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.01.5, method="cv" ) #Make plot of training error and CV MSPE
cv.3.01.25 <- abe.boost.3.01.25$cv.error[gbm.perf(abe.boost.3.01.25, method="cv" )]
val.3.01.25.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.3.01.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.3.01.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.3.01.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.3.01.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.01.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.01.25, method="cv" )))^2)


### Edited to here


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.02.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=3, shrinkage=0.02, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.02.25, method="cv" ) #Make plot of training error and CV MSPE
cv.3.02.25 <- abe.boost.3.02.25$cv.error[gbm.perf(abe.boost.3.02.25, method="cv" )]
val.3.02.25.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.02.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
opt.val.3.02.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.02.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.02.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.05.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=2000, interaction.depth=3, shrinkage=0.05, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.05.25, method="cv" ) #Make plot of training error and CV MSPE
cv.3.05.25 <- abe.boost.3.05.25$cv.error[gbm.perf(abe.boost.3.05.25, method="cv" )]
val.3.05.25.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.05.25,newdata=abelone[which(set==2),], n.trees=000))^2)
opt.val.3.05.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.05.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.05.25, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.3.1.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=3, shrinkage=0.1, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.3.1.25, method="cv" ) #Make plot of training error and CV MSPE
cv.3.1.25 <- abe.boost.3.1.25$cv.error[gbm.perf(abe.boost.3.1.25, method="cv" )]
val.3.1.25.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.1.25,newdata=abelone[which(set==2),], n.trees=1000))^2)
opt.val.3.1.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.3.1.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.3.1.25, method="cv" )))^2)


#############################################
# Increase interaction depth to 5
#############################################

abe.boost.5.001.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.001.25, method="cv" ) #Make plot of training error and CV MSPE
cv.5.001.25 <- abe.boost.5.001.25$cv.error[gbm.perf(abe.boost.5.001.5, method="cv" )]
val.5.001.25.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.001.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.5.001.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.5.001.25.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.25,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.5.001.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.001.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.01.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=25000, interaction.depth=5, shrinkage=0.01, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.01.25, method="cv" ) #Make plot of training error and CV MSPE
cv.5.01.25 <- abe.boost.5.01.25$cv.error[gbm.perf(abe.boost.5.01.25, method="cv" )]
val.5.01.25.05 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.01.25.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.25,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.5.01.25.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.25,newdata=abelone[which(set==2),], n.trees=20000))^2)
opt.val.5.01.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.01.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.01.25, method="cv" )))^2)

# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.02.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=5000, interaction.depth=5, shrinkage=0.02, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.02.25, method="cv" ) #Make plot of training error and CV MSPE
cv.5.02.25 <- abe.boost.5.02.25$cv.error[gbm.perf(abe.boost.5.02.25, method="cv" )]
val.5.02.25.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.02.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
opt.val.5.02.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.02.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.02.25, method="cv" )))^2)


# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.05.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=2000, interaction.depth=5, shrinkage=0.05, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.05.25, method="cv" ) #Make plot of training error and CV MSPE
cv.5.05.25 <- abe.boost.5.05.25$cv.error[gbm.perf(abe.boost.5.05.25, method="cv" )]
val.5.05.25.0 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.05.25,newdata=abelone[which(set==2),], n.trees=5000))^2)
opt.val.5.05.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.05.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.05.25, method="cv" )))^2)



# Shrinkage=.001 takes too long.  Try increasing shrinkage a little bit
abe.boost.5.1.25 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=1000, interaction.depth=5, shrinkage=0.1, 
                 bag.fraction=0.25, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.1.25, method="cv" ) #Make plot of training error and CV MSPE
cv.5.1.25 <- abe.boost.5.1.25$cv.error[gbm.perf(abe.boost.5.1.5, method="cv" )]
val.5.1.25.01 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.25,newdata=abelone[which(set==2),], n.trees=1000))^2)
opt.val.5.1.25 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.1.25,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.1.25, method="cv" )))^2)


######################################################
## From above: 
# Interaction depts of 1 is not good.
# Shrinkage above .01 is not good.
# Bagging fraction of .25 is better than .5
#  So try a few more runs
#####################################################


abe.boost.7.001.15 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=7, shrinkage=0.001, 
                 bag.fraction=0.15, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.7.001.15, method="cv" ) #Make plot of training error and CV MSPE
cv.7.001.15 <- abe.boost.7.001.15$cv.error[gbm.perf(abe.boost.7.001.15, method="cv" )]
val.7.001.15.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.7.001.15,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.7.001.15.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.7.001.15,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.7.001.15.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.7.001.15,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.7.001.15.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.7.001.15,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.7.001.15 <- mean((abelone[which(set==2),9]-predict(abe.boost.7.001.15,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.7.001.15, method="cv" )))^2)

abe.boost.2.001.3 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=50000, interaction.depth=2, shrinkage=0.001, 
                 bag.fraction=0.3, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.2.001.3, method="cv" ) #Make plot of training error and CV MSPE
cv.2.001.3 <- abe.boost.2.001.3$cv.error[gbm.perf(abe.boost.2.001.3, method="cv" )]
opt.val.2.001.3 <- mean((abelone[which(set==2),9]-predict(abe.boost.2.001.3,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.2.001.3, method="cv" )))^2)
opt.test.2.001.3 <- mean((abelone[which(set==3),9]-predict(abe.boost.2.001.3,newdata=abelone[which(set==3),], n.trees=gbm.perf(abe.boost.2.001.3, method="cv" )))^2)
opt.test.2.001.3a <- mean((abelone[which(set==3),9]-predict(abe.boost.2.001.3,newdata=abelone[which(set==3),], n.trees=20000))^2)

abe.boost.5.001.15 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.15, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.001.15, method="cv" ) #Make plot of training error and CV MSPE
cv.5.001.15 <- abe.boost.5.001.15$cv.error[gbm.perf(abe.boost.5.001.15, method="cv" )]
val.5.001.15.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.15,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.001.15.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.15,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.5.001.15.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.15,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.5.001.15.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.15,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.5.001.15 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.15,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.001.15, method="cv" )))^2)



abe.boost.5.001.10 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=18000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.10, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.5.001.10, method="cv" ) #Make plot of training error and CV MSPE
cv.5.001.10 <- abe.boost.5.001.10$cv.error[gbm.perf(abe.boost.5.001.10, method="cv" )]
val.5.001.10.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.10,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.5.001.10.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.10,newdata=abelone[which(set==2),], n.trees=10000))^2)
opt.val.5.001.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.5.001.10,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.5.001.10, method="cv" )))^2)

abe.boost.4.001.15 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=30000, interaction.depth=4, shrinkage=0.001, 
                 bag.fraction=0.15, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.4.001.15, method="cv" ) #Make plot of training error and CV MSPE
cv.4.001.15 <- abe.boost.4.001.15$cv.error[gbm.perf(abe.boost.4.001.15, method="cv" )]
val.4.001.15.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.4.001.15.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=10000))^2)
val.4.001.15.20 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=20000))^2)
val.4.001.15.30 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=30000))^2)
opt.val.4.001.15 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.4.001.15, method="cv" )))^2)


abe.boost.4.01.15 <- gbm(data=abelone[which(set==1),], Rings ~ ., 
                 distribution="gaussian", verbose=FALSE,
                 n.trees=15000, interaction.depth=4, shrinkage=0.01, 
                 bag.fraction=0.15, cv.folds=10)

win.graph(h=12, w=12, pointsize=12)
par(mfrow=c(3,2))
gbm.perf(abe.boost.4.001.15, method="cv" ) #Make plot of training error and CV MSPE
cv.4.01.15 <- abe.boost.4.01.15$cv.error[gbm.perf(abe.boost.4.01.15, method="cv" )]
val.4.01.15.5 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.01.15,newdata=abelone[which(set==2),], n.trees=5000))^2)
val.4.01.15.10 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.01.15,newdata=abelone[which(set==2),], n.trees=10000))^2)
opt.val.4.01.15 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.01.15,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.4.01.15, method="cv" )))^2)



win.graph(h=8, w=6, pointsize=12)
summary(abe.boost.4.001.15, n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))

win.graph(h=7,w=12,pointsize=12)
par(mfcol=c(2,3))
plot(abe.boost.4.001.15, i.var=c(1), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
plot(abe.boost.4.001.15, i.var=c(2), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
plot(abe.boost.4.001.15, i.var=c(3), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
win.graph(h=7,w=12,pointsize=12)
par(mfcol=c(2,3))
plot(abe.boost.4.001.15, i.var=c(4), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
plot(abe.boost.4.001.15, i.var=c(5), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
plot(abe.boost.4.001.15, i.var=c(6), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
win.graph(h=7,w=8,pointsize=12)
par(mfcol=c(2,2))
plot(abe.boost.4.001.15, i.var=c(7), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))
plot(abe.boost.4.001.15, i.var=c(8), n.trees=gbm.perf(abe.boost.4.001.15, method="cv" ))

opt.val.4.001.15 <- mean((abelone[which(set==2),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==2),], n.trees=gbm.perf(abe.boost.4.001.15, method="cv" )))^2)
opt.test.4.001.15 <- mean((abelone[which(set==3),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set==3),], n.trees=gbm.perf(abe.boost.4.001.15, method="cv" )))^2)
opt.tv.4.001.15 <- mean((abelone[which(set>1),9]-predict(abe.boost.4.001.15,newdata=abelone[which(set>1),], n.trees=gbm.perf(abe.boost.4.001.15, method="cv" )))^2)
