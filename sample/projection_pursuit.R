# Projection Pursuit using ppr 
# Air Quality data

library(datasets)
####################################################################
## One terms
####################################################################
#  Using default smoother, increasing quality of optimizer.
ppr1 <- ppr(data=airquality, Ozone~scale(Wind)+scale(Temp), nterms=1, optlevel=3) 
summary(ppr1)

# The term weights and the coefficients on the terms. 
ppr1$alpha
ppr1$beta

# Plots of results
win.graph(h=7,w=6,pointsize=12)
plot(ppr1, main="Projection Pursuit Regression 1 term", col="orange")

terms <- cbind(as.matrix(cbind(scale(airquality$Wind),scale(airquality$Temp))) %*% ppr1s$alpha,
               (airquality$Ozone-mean(airquality$Ozone, na.rm=TRUE ))/sd(airquality$Ozone, na.rm=TRUE))
points(x=terms[,2], y=terms[,1], pch=23, col="blue")

#  Creating matrix of estimated means by direct calculation.  Matrix form required by contour()  
x1 <- seq(from=0, to=22, by=.1)
xy1 <- data.frame(expand.grid(Wind=seq(from=0, to=22, by=.1), 
                              Temp=seq(from=55, to=95, by=.1)))
pred <- predict(ppr1,newdata=xy1)
surface1 = matrix(predict(ppr1, newdata=xy1),nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface1 ,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

##########################
# Using smoothing splines instead
ppr1s <- ppr(data=airquality, Ozone~Wind+Temp, nterms=1, optlevel=3, sm.method="gcvspline") 
summary(ppr1s)

pred <- predict(ppr1s,newdata=xy1)
surface1s = matrix(predict(ppr1s, newdata=xy1),nrow=length(x1))

# Plots of results
win.graph(h=7,w=6,pointsize=12)
plot(ppr1s, main="Projection Pursuit Regression 1 term", col="orange")

terms <- cbind(as.matrix(cbind(airquality$Wind,airquality$Temp)) %*% ppr1s$alpha,
               (airquality$Ozone-mean(airquality$Ozone, na.rm=TRUE ))/sd(airquality$Ozone, na.rm=TRUE))
points(x=terms[,1], y=terms[,2], pch=23, col="blue")

library(rgl)  
open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface1s, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")


####################################################################
## Two terms
####################################################################
ppr2 <- ppr(data=airquality, Ozone~Wind+Temp, nterms=2, optlevel=3, sm.method="gcvspline") 
summary(ppr2)

pred2 <- predict(ppr2,newdata=xy1)
surface2 = matrix(predict(ppr2, newdata=xy1),nrow=length(x1))


# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
plot(ppr2, main="Projection Pursuit Regression 2 terms", col="orange")

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface2,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")

####################################################################
## Runnung through various sizes to select of number of terms
##    (Picking max.terms arbitrarily.)
## Note: "Goodness of fit" is supposed to be 
##     "the overall residual (weighted) sum of squares"
##   But somehow it increases with adding new variables???
####################################################################

ppr.all <- ppr(data=airquality, Ozone~Wind+Temp, nterms=1, max.terms=6, 
               optlevel=3, sm.method="gcvspline") 
summary(ppr.all)

## Suggests 2 or 3 terms might be good.

####################################################################
## Three terms
####################################################################
ppr3 <- ppr(data=airquality, Ozone~Wind+Temp, nterms=3, optlevel=3, sm.method="gcvspline") 
summary(ppr3)

pred3 <- predict(ppr3,newdata=xy1)
surface3 = matrix(predict(ppr3, newdata=xy1),nrow=length(x1))

# Plots of results
win.graph(h=7,w=16,pointsize=12)
par(mfrow=c(1,3))
plot(ppr3, main="Projection Pursuit Regression 3 terms")

open3d()
persp3d(x = seq(from=0, to=22, by=.1), y = seq(from=55, to=95, by=.1), 
        z = surface3,col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone", zlim=c(0,200))
points3d(airquality$Ozone ~ airquality$Wind + airquality$Temp, col="blue")
