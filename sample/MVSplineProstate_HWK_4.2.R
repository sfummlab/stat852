# Multivariate Splines using the gam function of mgcv.
# Prostate Data

prostate <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# 3D plot to show surface more clearly.
library(rgl)  
  open3d()
  plot3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

library(mgcv)

## isotropic thin plate spline smoother 
# s() uses regularixed model fitting (smoothing spline)
# bs=  lists basis function s to be used.  
#   Default is "tp" = thin plate.
#   Likely alternative is "cr" = cubic regression
thin.plate <- gam(data=prostate, lpsa~s(lcavol,lweight, bs="tp")) 
summary(thin.plate)

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=-1.4, to=4, by=.01)
xy1 <- data.frame(expand.grid(lcavol=seq(from=-1.4, to=4, by=.01), 
                              lweight=seq(from=2.4, to=4.7, by=.01)))
pred <- predict(thin.plate,newdata=xy1)
surface = matrix(predict(thin.plate, newdata=xy1),nrow=length(x1))

# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(thin.plate, main="Thin Plate Default Contours")
plot(thin.plate, pers=TRUE, main="Thin Plate Default Surface")

open3d()
persp3d(x = seq(from=-1.4, to=4, by=.01), y = seq(from=2.4, to=4.7, by=.01), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="Predicted lpsa", zlim=c(-1,5.5))
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


## Tensor Product spline
# te() creates tensor products
tensor <- gam(data=prostate, lpsa~te(lcavol,lweight, bs="cr")) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

pred <- predict(tensor,newdata=xy1)
surface = matrix(predict(tensor, newdata=xy1),nrow=length(x1))

# Plots of results 
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(tensor, main="Tensor Product Default Contours")
plot(tensor, pers=TRUE, main="Tensor Product Default Surface")

open3d()
persp3d(x = seq(from=-1.4, to=4, by=.01), y = seq(from=2.4, to=4.7, by=.01), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="Predicted lpsa", zlim=c(-1,5.5))
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


lo.1 <- loess(data=prostate, lpsa ~ lcavol + lweight, span=1)
surface = matrix(predict(lo.1, newdata=xy1),nrow=length(x1))

open3d()
persp3d(x = seq(from=-1.4, to=4, by=.01), y = seq(from=2.4, to=4.7, by=.01), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="Predicted lpsa", zlim=c(-1,5.5), main="LOESS, Span=1")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

lo.5 <- loess(data=prostate, lpsa ~ lcavol + lweight, span=.5)
surface = matrix(predict(lo.5, newdata=xy1),nrow=length(x1))

open3d()
persp3d(x = seq(from=-1.4, to=4, by=.01), y = seq(from=2.4, to=4.7, by=.01), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="Predicted lpsa", zlim=c(-1,5.5), main="LOESS, Span=.5")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")




