# Multivariate Splines using the gam function of mgcv.
# Air Quality data


prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)



library(leaps)
library(rgl)
# 3D plot to show surface more clearly.
#library(plot3D)  
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

library(mgcv)

## isotropic thin plate spline smoother 
# s() uses regularized model fitting (smoothing spline)
# bs=  lists basis functions to be used.  
#   Default is "tp" = thin plate.
#   Likely alternative is "cr" = cubic regression
thin.plate <- gam(data=prostate, lpsa~s(lcavol,lweight, bs="tp")) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=-2, to=5, by=.1)
xy1 <- data.frame(expand.grid(lcavol=seq(from=-2, to=5, by=.1), 
                              lweight=seq(from=2, to=5, by=.1)))
pred <- predict(thin.plate,newdata=xy1)
surface = matrix(predict(thin.plate, newdata=xy1),nrow=length(x1))

# Plots of results
quartz(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(thin.plate, main="Thin Plate Default Contours")
plot(thin.plate, pers=TRUE, main="Thin Plate Default Surface")

# Plots of results
quartz(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(thin.plate, main="Thin Plate Default Surface", scheme=1)
plot(thin.plate, main="Thin Plate Default Surface", scheme=2)

open3d()
persp3d(x = seq(from=-2, to=5, by= .1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


## Tensor Product spline
# te() creates tensor products, Basis set is bs="cr" for cubic reg.
tensor <- gam(data=prostate, lpsa~te(lcavol,lweight, bs="cr")) 
#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

xy1 <- data.frame(expand.grid(lcavol=seq(from=-2, to=5, by=.1), 
                              lweight=seq(from=2, to=5, by=.1)))
pred <- predict(tensor,newdata=xy1)
surface = matrix(predict(tensor, newdata=xy1),nrow=length(x1))

# Plots of results 
quartz(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(tensor, main="Tensor Product Default Contours")
plot(tensor, pers=TRUE, main="Tensor Product Default Surface")

# Plots of results
quartz(h=7,w=12,pointsize=15)
par(mfrow=c(1,2))
plot(tensor, main="Tensor Default Surface", scheme=1)
plot(tensor, main="Tensor Default Surface", scheme=2)

open3d()
persp3d(x = seq(from=-2, to=5, by= .1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")
