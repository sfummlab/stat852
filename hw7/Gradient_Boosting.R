# Gradient Boosting using gbm 
# Air Quality data

library(gbm)
####################################################################
## Gradient boosting through gbm() allows different distributions to be used for boosting 
## Tuning parameters and defaults include 
# n.trees=100: number of trees (=M). Usually need to increase this
# interaction.depth = 1: is J-1. Default implies additive model.  
#    USUALLY need to increase this (HTF suggest 5)
# shrinkage = .001: The learning rate parameter "nu" 
#    (HTF: 0.1; author says .001 is better)
# bag.fraction=0.5: The subsampling fraction, eta (HTF: 0.5)
## Crossvalidation using cv.folds= allows you to estimate the number 
#    of trees for your current parameter settings to avoid overfitting
####################################################################
AQ <- na.omit(airquality[,c(1,3,4)])

# Only 2 variables, so trying small tree size, lots of trees
# Run these two functions several times to see change in CV suggestion
air.boost <- gbm(data=AQ, Ozone~Wind+Temp, distribution="gaussian", 
                 n.trees=10000, interaction.depth=2, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

gbm.perf(air.boost, method="cv" ) #Make plot of training error and CV MSPE 


win.graph(h=7, w=12, pointsize=12)
par(mfrow=c(1,2))
gbm.perf(air.boost, method="cv" ) #Make plot of training error and CV MSPE 


### NOTE: Can add trees using gbm.more(), in case you have a long-running function and need more trees.


# Refitting, since fast, but don't have to.  See predict.gbm() below'
air.boost.5000 <- gbm(data=AQ, Ozone~Wind+Temp, distribution="gaussian", 
                 n.trees=5000, interaction.depth=2, shrinkage=0.001, 
                 bag.fraction=0.5, cv.folds=10)

gbm.perf(air.boost.5000, method="cv" ) #Make plot of training error and CV MSPE 

# Variable Importance
win.graph(h=7, w=6)
summary(air.boost.5000)

#Profiles of variable effects
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(air.boost.5000, i.var=1)
plot(air.boost.5000, i.var=2)


# 3D plot of surface from aggregated tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
# predict.gbm() needs to know how many of thr trees to use 
### Convenient! (you can fit too many but base predictions on a smaller number)
surface.boost.5000 = matrix(predict(air.boost.5000, newdata=xy1, n.trees=4300, 
                                    type="response"),nrow=length(x1))


library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.boost.5000 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")

####################################################################
#Repeat, increasing the tree size to 6, 
#        increasing bag.fraction to allow larger trees
####################################################################

air.boost.6 <- gbm(data=AQ, Ozone~Wind+Temp, distribution="gaussian", 
                 n.trees=15000, interaction.depth=6, shrinkage=0.001, 
                 bag.fraction=.8, cv.folds=10)

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
gbm.perf(air.boost.6, method="cv" ) #Make plot of training error and CV MSPE 
summary(air.boost.6)

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(air.boost.6, i.var=1)
plot(air.boost.6, i.var=2)

# 3D plot of surface from aggregated tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
# Using only 9000 trees in the prediction
surface.boost.6 = matrix(predict(air.boost.6, newdata=xy1, n.trees=9000, type="response")
                         ,nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.boost.6 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")

####################################################################
#Repeat, showing bad things from using too little shrinkage and too many trees
####################################################################

air.boost.over <- gbm(data=AQ, Ozone~Wind+Temp, distribution="gaussian", 
                 n.trees=10000, interaction.depth=2, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=5)

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
gbm.perf(air.boost.over, method="cv" ) #Make plot of training error and CV MSPE 

air.boost.over2 <- gbm(data=AQ, Ozone~Wind+Temp, distribution="gaussian", 
                 n.trees=150, interaction.depth=2, shrinkage=0.1, 
                 bag.fraction=0.5, cv.folds=5)

gbm.perf(air.boost.over2, method="cv" ) #Make plot of training error and CV MSPE 
 

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(air.boost.over, i.var=1)
plot(air.boost.over, i.var=2)

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(air.boost.over, i.var=1, n.trees=75)
plot(air.boost.over, i.var=2, n.trees=75)

# 3D plot of surface from aggregated tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
# Using only 75 trees in the prediction
surface.boost.over = matrix(predict(air.boost.over, newdata=xy1, n.trees=75, type="response"),
                            nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.boost.over ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")

