# Multivariate Adamtive Regression Splines using mars{mda} 
# Air Quality data

##########
# NOTE: e1071 package has a "tune" function
##########


# NOTE TO TOM: SKIP mars() in class

library(mda)
library(earth)
library(datasets)
####################################################################
## Default fit
#  Does not handle missing values, so must get rid of them.
####################################################################

x1 <- as.matrix(airquality[which(airquality[,1]>0),c(3:4)])
y1 <- as.vector(airquality[which(airquality[,1]>0),1])

air.mars <- mars(x=x1, y=y1, trace=1)

# Predicted values
#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
pred.mars <- predict(air.mars,newdata=xy1)
surface.mars = matrix(pred.mars,nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.mars ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

####################################################################
## Adding additional degree of interaction.  Good idea to allow AT LEAST 2 and maybe a little more
####################################################################
                  
air.mars.2 <- mars(x=as.matrix(airquality[which(airquality[,1]>0),c(3:4)],ncol=2),
                 y=as.vector(airquality[which(airquality[,1]>0),1]), trace.mars=TRUE, degree=2)

# Predicted values
#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
pred.mars.2 <- predict(air.mars.2,newdata=xy1)
surface.mars.2 = matrix(pred.mars.2,nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.mars.2 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


####################################################################
## Adding increased penalty.  Doesn't make a difference in this case, but can lead to smaller models.
####################################################################
                  
air.mars.23 <- mars(x=as.matrix(airquality[which(airquality[,1]>0),c(3:4)],ncol=2),
                 y=as.vector(airquality[which(airquality[,1]>0),1]), trace.mars=TRUE, degree=2, penalty=3)

# Predicted values
#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
pred.mars.23 <- predict(air.mars.23,newdata=xy1)
surface.mars.23 = matrix(pred.mars.23,nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.mars.23 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

######################################################################
# Now using earth() from package earth. ("MARS" is trademarked software)
# Default parameters, with added trace=3 to get information printed out about forward and backward steps.
#  Does not handle missing values, so must get rid of them.
#  NOTE: earth() has a LOT of arguments that can be specified.  
######################################################################

library(earth)

air.earth <- earth(Ozone~Temp + Wind, data=airquality[which(airquality[,1]>0),], trace=3)
summary(air.earth)
win.graph(h=7, w=6, pointsize=12)
plot(air.earth)

library(rgl)  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=98, by=.1)))
pred.earth <- predict(air.earth,newdata=xy1)
surface.earth = matrix(pred.earth,nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.earth ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

######################################################################
# Now use degree=2 to allow interactions.  
# This automatically increases penalty to 3 from default of 2.  (good)
######################################################################

air.earth.2 <- earth(Ozone~Temp + Wind, data=airquality[which(airquality[,1]>0),], 
                     trace=3, degree=2)
summary(air.earth.2)
win.graph(h=7, w=6, pointsize=12)
plot(air.earth.2)

pred.earth.2 <- predict(air.earth.2,newdata=xy1)
surface.earth.2 = matrix(pred.earth.2,nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.earth.2 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

#########################################################################
# earth() can now do CV pruning!  
# "This selects the number of terms that gives the 
#    maximum mean out-of-fold RSq on the fold models." 
#  Still uses backward elimination to create sequence of models
#  I think I prefer GCV from "backward" --- CV is vary variable!
# pmethod = c("backward", "none", "exhaustive", "forward", "seqrep", "cv")
#########################################################################

air.earth.2cv <- earth(Ozone~Temp + Wind, data=airquality[which(airquality[,1]>0),], 
                     trace=3, degree=2, pmethod="cv", nfold=10)
summary(air.earth.2cv)
win.graph(h=7, w=6, pointsize=12)
plot(air.earth.2cv)

pred.earth.2cv <- predict(air.earth.2cv,newdata=xy1)
surface.earth.2cv = matrix(pred.earth.2cv,nrow=length(x1))

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=98, by=.1), 
        z = surface.earth.2cv ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

