# Gradient Boosting using gbm, Classification 
# Vehicle image data

vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

# only does two-class problems, so changing response to pick out bus
vehdata$type <- ifelse(vehdata$class==3 , yes=1, no=0)

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-c(19,20)]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-c(19,20)]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-c(19,20)]

library(gbm)
####################################################################
## Gradient boosting through gbm() does only 2-class classification problems. 
##    Requires 0/1 response values
##  Allow different loss functions through distribution= :
## Exponential ("adaboost") and binomial deviance ("bernoulli")
## 
## As before, tuning parameters and defaults include 
# n.trees=100: number of trees (=M)
# interaction.depth=1: implies additive model.  USUALLY need to increase this (HTF suggest 5)
# shrinkage = .001: The learning rate parameter (HTF: 0.1; author says .001 is better)
# bag.fraction=0.5: The subsampling fraction (HTF: 0.5)
## Crossvalidation using cv.folds= allows you to estimate the number of trees for your current
##   parameter settings to avoid overfitting
####################################################################

# Run these two functions several times to see change in suggested #trees
veh.boost.ada.2 <- gbm(data=set1, type~., 
                 distribution="adaboost", verbose=FALSE, 
                 n.trees=15000, interaction.depth=2, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)
# 10000 seems optimal following tuning effort
win.graph(h=7, w=12, pointsize=12)
par(mfrow=c(1,2))
ntrees.2 <- gbm.perf(veh.boost.ada.2, method="cv" )
summary(veh.boost.ada.2, n.trees=ntrees.2, las=1) #"las"" rotates labels
# From Help: 'For distribution="gaussian" this returns exactly the reduction 
#   of squared error attributable to each variable. For other loss functions 
#   this returns the reduction attributeable to each variable in sum of 
#   squared error in predicting the gradient on each iteration. It describes 
#   the relative influence of each variable in reducing the loss function.'

win.graph(h=7, w=6, pointsize=12)
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=6) # Var 6 is most important
# From Help: 'plot.gbm produces low dimensional projections of the gbm.object 
#   by integrating out the variables not included in the i.var argument. 
#   The function selects a grid of points and uses the weighted tree 
#   traversal method described in Friedman (2001) to do the integration.
# I don't know exactly what the Y axis is, but the X axis is for the variable,

# Plots of marginal effects of individual variables
win.graph(h=15, w=15, pointsize=12)
par(mfrow=c(3,3))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=1, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=2, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=3, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=4, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=5, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=6, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=7, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=8, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=9, ylim=c(-4,0))
win.graph(h=15, w=15, pointsize=12)
par(mfrow=c(3,3))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=10, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=11, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=12, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=13, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=14, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=15, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=16, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=17, ylim=c(-4,0))
plot(veh.boost.ada.2, n.trees=ntrees.2, i.var=18, ylim=c(-4,0))

# showing trees of size 5+1=6
veh.boost.ada.5 <- gbm(data=set1, type~., 
                 distribution="adaboost", verbose=FALSE, 
                 n.trees=10000, interaction.depth=5, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

win.graph(h=7, w=12, pointsize=12)
par(mfrow=c(1,2))
ntrees.5 <- gbm.perf(veh.boost.ada.5, method="cv" )
summary(veh.boost.ada.5, n.trees=ntrees.5, las=1) #"las"" rotates labels




## Calculate test misclassification rates
### predict() returns values of f(x), not class values.
pred.boost.train.2 <- predict(veh.boost.ada.2, newdata=set1, n.trees=ntrees.2)
pred.boost.val.2 <- predict(veh.boost.ada.2, newdata=set2, n.trees=ntrees.2)
pred.boost.test.2 <- predict(veh.boost.ada.2, newdata=set3, n.trees=ntrees.2)

pred.boost.train.5 <- predict(veh.boost.ada.5, newdata=set1, n.trees=ntrees.5)
pred.boost.val.5 <- predict(veh.boost.ada.5, newdata=set2, n.trees=ntrees.5)
pred.boost.test.5 <- predict(veh.boost.ada.5, newdata=set3, n.trees=ntrees.5)

#Predictions are (-1,1).  Need to convert predictions into 0/1
misclass.boost.train.2 <- mean(ifelse(sign(pred.boost.train.2) == 2*set1$type-1, yes=0, no=1))
misclass.boost.val.2 <- mean(ifelse(sign(pred.boost.val.2) == 2*set2$type-1, yes=0, no=1))
misclass.boost.test.2 <- mean(ifelse(sign(pred.boost.test.2) == 2*set3$type-1, yes=0, no=1))

misclass.boost.train.5 <- mean(ifelse(sign(pred.boost.train.5) == 2*set1$type-1, yes=0, no=1))
misclass.boost.val.5 <- mean(ifelse(sign(pred.boost.val.5) == 2*set2$type-1, yes=0, no=1))
misclass.boost.test.5 <- mean(ifelse(sign(pred.boost.test.5) == 2*set3$type-1, yes=0, no=1))

table(set1$type,(sign(pred.boost.train.2) + 1)/2,dnn=c("Obs","Pred"))
table(set2$type,(sign(pred.boost.val.2) + 1)/2,dnn=c("Obs","Pred"))
table(set3$type,(sign(pred.boost.test.2) + 1)/2,dnn=c("Obs","Pred"))
table(set1$type,(sign(pred.boost.train.5) + 1)/2,dnn=c("Obs","Pred"))
table(set2$type,(sign(pred.boost.val.5) + 1)/2,dnn=c("Obs","Pred"))
table(set3$type,(sign(pred.boost.test.5) + 1)/2,dnn=c("Obs","Pred"))


####################################################################
## 4-class classification using adabag::boosting
##   boos:  if TRUE (by default), a bootstrap sample of the training set is drawn using the
##          weights for each observation on that iteration. If FALSE, every observation is
##          used with its weights.
##   mfinal: an integer, the number of iterations for which boosting is run or the number of
##           trees to use. Defaults to mfinal=100 iterations.
##  coeflearn:  if 'Breiman'(by default), alpha=1/2log((1-err)/err) is used. If 'Freund'
##               alpha=log((1-err)/err) is used. Where alpha is the weight updating coefficient.
##   control:  options that control details of the rpart algorithm. 
##             See rpart.control for more details.
####################################################################

library(adabag)

# Need to recreate data sets with original class

vehdata$class <- as.factor(vehdata$class)

set1 <- vehdata[which(perm<=nrow(vehdata)/2),-c(20,21)]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-c(20,21)]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-c(20,21)]

## Function to train boosting on set1, compute error on set2, at numbers of trees given in mlist 
aa <- function(set1,set2,mlist){
  err.list <- matrix(0,nrow=length(mlist),ncol=2)
  row=0
  for(m in mlist){
     row <- row + 1
     ada.m <- boosting(data=set1, formula=class~., mfinal=m)
     val.ada.m <- predict(ada.m, newdata=set2)
     val.err.m <- mean(ifelse(val.ada.m$class == set2$class, yes=0, no=1))
     err.list[row,] <- c(m,val.err.m)
}
  err.list
}

#  Running this function multiple times, because the boosting error rates are rather variable.
#  Maybe should run it a few more times, and try out to 1000?  Not as in-class demo...

trainerr <- aa(set1,set1,c(10,30,50,70,100,150,200,250,300,400,500))
valerr1 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500))


# code below was used to make plot in notes.  DO NOT RUN IN CLASS!

valerr2 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr3 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr4 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr5 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr6 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr7 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr8 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr9 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))
valerr10 <- aa(set1,set2,c(10,30,50,70,100,150,200,250,300,400,500, 750, 1000))

valerr <- (valerr2+valerr3+valerr4+valerr5+valerr6+valerr7+valerr8+valerr9+valerr10)/9


win.graph(h=7, w=12, pointsize=12)
par(mfrow=c(1,2))

plot(x=trainerr[,1],y=trainerr[,2])
plot(x=valerr[,1],y=valerr[,2])





