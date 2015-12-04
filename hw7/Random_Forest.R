# Random Forests using randomForest 
# Air Quality data

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

air.rf <- randomForest(data=airquality[which(airquality$Ozone>0),], Ozone~Wind+Temp, 
                       importance=TRUE, ntree=1000, mtry=1, keep.forest=TRUE)
air.rf             # Barely useful here
summary(air.rf)    # Not useful here.
importance(air.rf) # Print out importance measures
varImpPlot(air.rf) # Plot of importance measures; more interesting with more variables

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
partialPlot(air.rf, pred.data=airquality[which(airquality$Ozone>0),], x.var=Temp)
partialPlot(air.rf, pred.data=airquality[which(airquality$Ozone>0),], x.var=Wind)

# Default plot method shows OOB error vs. number of trees.

win.graph(h=7,w=6,pointsize=12)
plot(air.rf)

# Histogram of tree sizes
win.graph(h=7,w=6,pointsize=12)
hist(treesize(air.rf))

# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(air.rf)

### 3d plot of prediction
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
pred.rf <- predict(air.rf,newdata=xy1)
surface.rf = matrix(predict(air.rf, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.rf ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

#################################################################
## Rerun, limiting trees to no more than 20 nodes
#################################################################

air.rf2 <- randomForest(data=airquality[which(airquality$Ozone>0),], 
                       Ozone~Wind+Temp, importance=TRUE, ntree=1000, 
                       mtry=1, keep.forest=TRUE, maxnodes=20)
air.rf2
summary(air.rf2)
importance(air.rf2)
varImpPlot(air.rf2)
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
partialPlot(air.rf2, pred.data=airquality[which(airquality$Ozone>0),], x.var=Temp)
partialPlot(air.rf2, pred.data=airquality[which(airquality$Ozone>0),], x.var=Wind)
win.graph(h=7,w=6,pointsize=12)
plot(air.rf2)
win.graph(h=7,w=6,pointsize=12)
hist(treesize(air.rf2), breaks=c(15:21))
varUsed(air.rf2)


x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
pred.rf2 <- predict(air.rf2,newdata=xy1)
surface.rf2 = matrix(predict(air.rf2, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.rf2 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")
