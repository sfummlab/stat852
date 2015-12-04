# Regression trees for Abelone Data
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

library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
####################################################################

abe.rf1 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=1, keep.forest=TRUE)
OOB.MSE1 <- mean((abelone[which(set==1),9]-abe.rf1$predicted)^2)
abe.rf2 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=2, keep.forest=TRUE)
OOB.MSE2 <- mean((abelone[which(set==1),9]-abe.rf2$predicted)^2)
abe.rf3 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=3, keep.forest=TRUE)
OOB.MSE3 <- mean((abelone[which(set==1),9]-abe.rf3$predicted)^2)
abe.rf4 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=4, keep.forest=TRUE)
OOB.MSE4 <- mean((abelone[which(set==1),9]-abe.rf4$predicted)^2)
abe.rf5 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=5, keep.forest=TRUE)
OOB.MSE5 <- mean((abelone[which(set==1),9]-abe.rf5$predicted)^2)
abe.rf6 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=6, keep.forest=TRUE)
OOB.MSE6 <- mean((abelone[which(set==1),9]-abe.rf6$predicted)^2)
abe.rf7 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=7, keep.forest=TRUE)
OOB.MSE7 <- mean((abelone[which(set==1),9]-abe.rf7$predicted)^2)
abe.rf8 <- randomForest(x=abelone[which(set==1),-9], y=abelone[which(set==1),9], importance=TRUE, ntree=1000, mtry=8, keep.forest=TRUE)
OOB.MSE8 <- mean((abelone[which(set==1),9]-abe.rf8$predicted)^2)

win.graph(h=15,w=15,pointsize=12)
par(mfrow=c(4,2))
plot(abe.rf1) # Plot of importance measures; more interesting with more variables
plot(abe.rf2) # Plot of importance measures; more interesting with more variables
plot(abe.rf3) # Plot of importance measures; more interesting with more variables
plot(abe.rf4) # Plot of importance measures; more interesting with more variables
plot(abe.rf5) # Plot of importance measures; more interesting with more variables
plot(abe.rf6) # Plot of importance measures; more interesting with more variables
plot(abe.rf7) # Plot of importance measures; more interesting with more variables
plot(abe.rf8) # Plot of importance measures; more interesting with more variables

win.graph(h=7,w=6)
varImpPlot(abe.rf3)
importance(abe.rf3)
# Default plot method shows OOB error vs. number of trees.

pred.rf1 <- predict(abe.rf1, newdata=abelone[which(set>1),-9])
pred.rf2 <- predict(abe.rf2, newdata=abelone[which(set>1),-9])
pred.rf3 <- predict(abe.rf3, newdata=abelone[which(set>1),-9])
pred.rf4 <- predict(abe.rf4, newdata=abelone[which(set>1),-9])
pred.rf5 <- predict(abe.rf5, newdata=abelone[which(set>1),-9])
pred.rf6 <- predict(abe.rf6, newdata=abelone[which(set>1),-9])
pred.rf7 <- predict(abe.rf7, newdata=abelone[which(set>1),-9])
pred.rf8 <- predict(abe.rf8, newdata=abelone[which(set>1),-9])
MSPE1 <- mean((abelone[which(set>1),9]-pred.rf1)^2)
MSPE2 <- mean((abelone[which(set>1),9]-pred.rf2)^2)
MSPE3a <- mean((abelone[which(set>1),9]-pred.rf3)^2)
MSPE4 <- mean((abelone[which(set>1),9]-pred.rf4)^2)
MSPE5 <- mean((abelone[which(set>1),9]-pred.rf5)^2)
MSPE6 <- mean((abelone[which(set>1),9]-pred.rf6)^2)
MSPE7 <- mean((abelone[which(set>1),9]-pred.rf7)^2)
MSPE8 <- mean((abelone[which(set>1),9]-pred.rf8)^2)
pred.rf1.3 <- predict(abe.rf1, newdata=abelone[which(set==3),-9])
pred.rf2.3 <- predict(abe.rf2, newdata=abelone[which(set==3),-9])
pred.rf3.3 <- predict(abe.rf3, newdata=abelone[which(set==3),-9])
pred.rf4.3 <- predict(abe.rf4, newdata=abelone[which(set==3),-9])
pred.rf5.3 <- predict(abe.rf5, newdata=abelone[which(set==3),-9])
pred.rf6.3 <- predict(abe.rf6, newdata=abelone[which(set==3),-9])
pred.rf7.3 <- predict(abe.rf7, newdata=abelone[which(set==3),-9])
pred.rf8.3 <- predict(abe.rf8, newdata=abelone[which(set==3),-9])
MSPE1.3 <- mean((abelone[which(set==3),9]-pred.rf1.3)^2)
MSPE2.3 <- mean((abelone[which(set==3),9]-pred.rf2.3)^2)
MSPE3.3 <- mean((abelone[which(set==3),9]-pred.rf3.3)^2)
MSPE4.3 <- mean((abelone[which(set==3),9]-pred.rf4.3)^2)
MSPE5.3 <- mean((abelone[which(set==3),9]-pred.rf5.3)^2)
MSPE6.3 <- mean((abelone[which(set==3),9]-pred.rf6.3)^2)
MSPE7.3 <- mean((abelone[which(set==3),9]-pred.rf7.3)^2)
MSPE8.3 <- mean((abelone[which(set==3),9]-pred.rf8.3)^2)
