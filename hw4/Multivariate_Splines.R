# Multivariate Splines using the gam function of mgcv.
# Air Quality data

# 3D plot to show surface more clearly.
library(rgl)  
  open3d()
  plot3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

library(mgcv)

## isotropic thin plate spline smoother 
# s() uses regularized model fitting (smoothing spline)
# bs=  lists basis functions to be used.  
#   Default is "tp" = thin plate.
#   Likely alternative is "cr" = cubic regression
thin.plate <- gam(data=airquality, Ozone~s(Wind,Temp, bs="tp")) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1)))
pred <- predict(thin.plate,newdata=xy1)
surface = matrix(predict(thin.plate, newdata=xy1),nrow=length(x1))

# Plots of results
x11(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(thin.plate, main="Thin Plate Default Contours")
plot(thin.plate, pers=TRUE, main="Thin Plate Default Surface")

# Plots of results
x11(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(thin.plate, main="Thin Plate Default Surface", scheme=1)
plot(thin.plate, main="Thin Plate Default Surface", scheme=2)

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


## Tensor Product spline
# te() creates tensor products, Basis set is bs="cr" for cubic reg.
tensor <- gam(data=airquality, Ozone~te(Wind,Temp, bs="cr")) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1)))
pred <- predict(tensor,newdata=xy1)
surface = matrix(predict(tensor, newdata=xy1),nrow=length(x1))

# Plots of results 
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(tensor, main="Tensor Product Default Contours")
plot(tensor, pers=TRUE, main="Tensor Product Default Surface")

# Plots of results
win.graph(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(tensor, main="Thin Plate Default Surface", scheme=1)
plot(tensor, main="Thin Plate Default Surface", scheme=2)

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")
