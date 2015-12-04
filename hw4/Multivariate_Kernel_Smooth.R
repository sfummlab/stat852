#############################################################################
# Multivariate Splines using the gam function of mgcv.
# Air Quality data
#############################################################################

# Loess smoother, degree 1, span=.75
lo.1.75 <- loess(data=airquality, Ozone~Wind + Temp, degree=1) 

#  Then creating matrix of estimated means by direct calculation.  
#   Matrix form required by contour()  

x1 <- seq(from=0, to=22, by=.1)
xy1 <- expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1))
surface <- predict(lo.1.75, newdata=xy1)
                              
# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", main="Loess, Degree 1, Span=.75 Contours")
contour(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
         main="Loess, Degree 1, Span=.75 Contours")

# 3D plot.
library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")
        

###############################################################################
#Repeat with longer span
#############################################################################

# Loess smoother, degree 1, span=1
lo.1.1 <- loess(data=airquality, Ozone~Wind + Temp, span=1, degree=1) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=0, to=22, by=.1)
xy1 <- expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1))
surface <- predict(lo.1.1, newdata=xy1)
                              
# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", main="Loess, Degree 1, Span=1 Contours")
contour(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
         main="Loess, Degree 1, Span=1 Contours")

# 3D plot.
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

###############################################################################
#Repeat with higher degree
#############################################################################

# Loess smoother, degree 2, span=0.75
lo.2.1 <- loess(data=airquality, Ozone~Wind + Temp, degree=2) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=0, to=22, by=.1)
xy1 <- expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1))
surface <- predict(lo.2.1, newdata=xy1)
                              
# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", main="Loess, Degree 2, Span=1 Contours")
contour(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface, xlab="Wind", ylab="Temp", 
         main="Loess, Degree 2, Span=1 Contours")

# 3D plot. 
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")
