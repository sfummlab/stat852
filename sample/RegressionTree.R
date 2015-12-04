# Regression trees using rpart 
# Air Quality data

library(rpart)
####################################################################
## Default tree
####################################################################

air.tree <- rpart(data=airquality, Ozone~Wind+Temp, method="anova")
air.tree
summary(air.tree)
meanvar(air.tree) # Print out plot of variances vs means for terminal nodes

# Plotting results twice just to show different options.
# First plot uses all defaults. 
# Second plot uses diagonal branches, uniform distances between levels,
#   and prints the node sizes and limits the number of digits displayed.
# I kind of like the unequal distances in the first plot, but like everything 
#   else about the second.  You can decide what you like.
x11(h=9, w=12, pointsize=11)
par(mfrow=c(1,2))
plot(air.tree)
text(air.tree)

#############################
# CHECK OUT rpart.plot PACKAGE to make nicer trees!
#############################


plot(air.tree, branch=0, uniform=TRUE)
text(air.tree, use.n=TRUE, digits=3, all=TRUE, xpd=TRUE)

# Plot of the cross-validation for the complexity parameter.
x11(h=7, w=6)
plotcp(air.tree)

# Creating a pruned tree using optimum of the CP.
air.prune <- prune(air.tree, cp=0.19)

win.graph(h=7, w=6, pointsize=11)
plot(air.prune, branch=0, main="Final pruned tree")
text(air.prune, use.n=TRUE, digits=3, xpd=TRUE)



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