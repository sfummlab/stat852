# Regression trees using rpart 
# Air Quality data

library(rpart)
####################################################################
## Default tree
####################################################################

air.tree <- rpart(data=airquality, Ozone~Wind+Temp, method="anova")
air.tree
air.tree$cptable
summary(air.tree)
x11()
meanvar(air.tree) # Print out plot of variances vs means for terminal nodes


## Plotting results twice just to show different options.
## First plot uses all defaults. 
## Second plot uses diagonal branches, uniform distances between levels,
##   and prints the node sizes and limits the number of digits displayed.
## I kind of like the unequal distances in the first plot, but like everything 
##   else about the second.  You can decide what you like.
#x11(h=9, w=12, pointsize=11)
#par(mfrow=c(1,2))
#plot(air.tree)
#text(air.tree)
#
#plot(air.tree, branch=0, uniform=TRUE)
#text(air.tree, use.n=TRUE, digits=3, all=TRUE, xpd=TRUE)
#

#############################
# CHECK OUT rpart.plot PACKAGE to make nicer trees!
#   See the package vignette for examples
#############################

library(rpart.plot)
x11(h=7, w=6, pointsize=11)
prp(air.tree, type=1, extra=1, main="Original full tree")


# Plot of the cross-validation for the complexity parameter.
x11(h=7, w=8)
plotcp(air.tree)

# Creating a pruned tree using optimum of the CP.
air.prune <- prune(air.tree, cp=0.19)

win.graph(h=7, w=6, pointsize=11)
prp(air.prune, type=1, extra=1, main="Final pruned tree")



# 3D plot of surface from full unpruned tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
pred.full <- predict(air.tree,newdata=xy1)
surface.full = matrix(predict(air.tree, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.full ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


pred.prune <- predict(air.prune,newdata=xy1)
surface.prune = matrix(predict(air.prune, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.prune ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

# Sometimes the cp stopping rule causes trees to terminate too early.
#   Can set it to 0 to see what a "full" tree looks like.


air.tree2 <- rpart(data=airquality, Ozone~Wind+Temp, method="anova", cp=0)
air.tree2
(cpt <- air.tree2$cptable)

x11(h=7, w=8)
plotcp(air.tree2)

# The code below shows how to select the tuning parameter using
#   either the +1SE or the true min CV error

# Find location of minimum error
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- cpt[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)

# Do pruning each way
air2.prune.min <- prune(air.tree2, cp=cp.min)
air2.prune.1se <- prune(air.tree2, cp=cp.1se)

library(rpart.plot)

x11(h=10, w=12, pointsize=11)
par(mfrow=c(1,2))
prp(air2.prune.min, type=1, extra=1, main="Larger tree pruned to Min CV Error")

prp(air2.prune.1se, type=1, extra=1, main="Larger tree pruned to +1SE CV Error")
