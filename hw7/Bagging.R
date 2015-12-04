# Bagging Regression trees using ipred 
# Air Quality data

library(ipred)
####################################################################
## Default Bagging uses B="nbagg"=25.  THIS IS TOO SMALL.  
##    If your computer can stand it, MAKE IT BIGGER
## (Try rerunning this several times with nbagg=25 and look at the OOB error 
##    -- it is very variable!
## Try with 500.  Doesn't take too long, and slightly lower error and less variable!
####################################################################

air.bag <- bagging(data=airquality, Ozone~Wind+Temp, method="anova", coob=TRUE)
air.bag

air.bag <- bagging(data=airquality, Ozone~Wind+Temp, method="anova", nbagg=500, coob=TRUE)
air.bag


air.bag$err
# Show the first few trees, just for fun
#  Note that $mtrees is a list of objects, so access $btree from object in element [[1]]
air.bag$mtrees[[1]]$btree 
air.bag$mtrees[[2]]$btree 
air.bag$mtrees[[3]]$btree 


# 3D plot of surface from aggregated tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))

surface.bag = matrix(predict(air.bag, newdata=xy1),nrow=length(x1))
library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.bag ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

surface.tree1 = matrix(predict(air.bag$mtrees[[1]]$btree, newdata=xy1),nrow=length(x1))
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.tree1 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

surface.tree2 = matrix(predict(air.bag$mtrees[[2]]$btree, newdata=xy1),nrow=length(x1))
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.tree2 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

surface.tree3 = matrix(predict(air.bag$mtrees[[3]]$btree, newdata=xy1),nrow=length(x1))
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.tree3 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")



#Repeat, relaxing the complexity parameter and the splitting criteria.  
#  Defaults are rpart defaults, cp=0.01, minsplit=20.
##  (Doesn't matter much in this example)
library(rpart)

air.bag.0 <- bagging(data=airquality, Ozone~Wind+Temp, method="anova", nbagg=100, 
                     coob=TRUE, control=rpart.control(cp=0, minsplit=10))
air.bag.0

air.bag.0$err
air.bag.0$mtrees[[1]]$btree # Show the first few trees, just for fun
air.bag.0$mtrees[[2]]$btree #  Note that $mtrees is a list, so access $btree in element [[1]]
air.bag.0$mtrees[[3]]$btree 

# 3D plot of surface from aggregated tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  

surface.bag.0 = matrix(predict(air.bag.0, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.bag.0 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

