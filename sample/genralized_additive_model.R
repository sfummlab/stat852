# Multivariate Splines using the gam function of mgcv.
# Air Quality data

# 3D plot to show surface more clearly.

library(mgcv)

#  Generalized additive model as alternative to multivariate splines
gam1 <- gam(data=airquality, Ozone~s(Wind)+s(Temp), family=gaussian(link=identity)) 
summary(gam1)
#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1)))
pred <- predict(gam1,newdata=xy1)
surface = matrix(predict(gam1, newdata=xy1),nrow=length(x1))

# Plots of results
quartz(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(gam1, main="Generalized Additive Model marginal splines")

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


####################################################################
## Adding interaction effect
####################################################################
gam2 <- gam(data=airquality, Ozone~s(Wind) + s(Temp) + s(Wind,Temp)) 
#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  
summary(gam2)

x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1)))
pred2 <- predict(gam2,newdata=xy1)
surface2 = matrix(predict(gam2, newdata=xy1),nrow=length(x1))

# Plots of results
win.graph(h=8,w=16,pointsize=12)
par(mfrow=c(1,3))
plot(gam2, main="Generalized Additive Model marginal splines")

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface2,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


##################################################
# Compare residuals of models

# Combine residuals with data
aqres <- data.frame(airquality[which(airquality$Ozone > 0),], 
                    res1=residuals(gam1), res2=residuals(gam2))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z =  matrix(0,nrow=nrow(surface),ncol=ncol(surface)),col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Model 1 residual", zlim=c(-100,100))
points3d(aqres$res1 ~ aqres$Wind + aqres$Temp, col="blue")

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z =  matrix(0,nrow=nrow(surface),ncol=ncol(surface)),col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Model 2 residual", zlim=c(-100,100))
points3d(aqres$res2 ~ aqres$Wind + aqres$Temp, col="blue")
