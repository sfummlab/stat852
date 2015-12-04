# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

pro.test$X21 <- as.numeric(pro.test$X21)


pro$X9.1 <- pro$X9 * pro$X1
pro$X9.2 <- pro$X9 * pro$X2
pro$X9.3 <- pro$X9 * pro$X3
pro$X9.4 <- pro$X9 * pro$X4
pro$X9.5 <- pro$X9 * pro$X5
pro$X9.6 <- pro$X9 * pro$X6
pro$X9.7 <- pro$X9 * pro$X7
pro$X9.8 <- pro$X9 * pro$X8
pro$X9.10<- pro$X9 * pro$X10
pro$X9.11 <- pro$X9 * pro$X11
pro$X9.12<- pro$X9 * pro$X12
pro$X9.13 <- pro$X9 * pro$X13
pro$X9.14 <- pro$X9 * pro$X14
pro$X9.15 <- pro$X9 * pro$X15
pro$X9.16 <- pro$X9 * pro$X16
pro$X9.17 <- pro$X9 * pro$X17
pro$X9.18 <- pro$X9 * pro$X18
pro$X9.19 <- pro$X9 * pro$X19
pro$X9.20 <- pro$X9 * pro$X20

#pro$X21.1 <- pro$X21 * pro$X1
#pro$X21.2 <- pro$X21 * pro$X2
#pro$X21.3 <- pro$X21 * pro$X3
#pro$X21.4 <- pro$X21 * pro$X4
#pro$X21.5 <- pro$X21 * pro$X5
#pro$X21.6 <- pro$X21 * pro$X6
#pro$X21.7 <- pro$X21 * pro$X7
#pro$X21.8 <- pro$X21 * pro$X8
#pro$X21.10 <- pro$X21 * pro$X10
#pro$X21.11 <- pro$X21 * pro$X11
#pro$X21.12 <- pro$X21* pro$X12
#pro$X21.13 <- pro$X21 * pro$X13
#pro$X21.14 <- pro$X21 * pro$X14
#pro$X21.15 <- pro$X21 * pro$X15
#pro$X21.16 <- pro$X21 * pro$X16
#pro$X21.17 <- pro$X21 * pro$X17
#pro$X21.18 <- pro$X21 * pro$X18
#pro$X21.19 <- pro$X21 * pro$X19
#pro$X21.20 <- pro$X21 * pro$X20




pro$logX1  <- log(pro$X1 )
pro$logX2  <- log(pro$X2 )
pro$logX3  <- log(pro$X3 )
pro$logX4  <- log(pro$X4 )
pro$logX5  <- log(pro$X5 )
pro$logX6  <- log(pro$X6 )
pro$logX7  <- log(pro$X7 )
pro$logX8  <- log(pro$X8 )
pro$logX10 <- log(pro$X10)
pro$logX11 <- log(pro$X11)
#pro$logX12 <- log(pro$X12)
pro$logX13 <- log(pro$X13)
pro$logX14 <- log(pro$X14)
pro$logX15 <- log(pro$X15)
pro$logX16 <- log(pro$X16)
pro$logX17 <- log(pro$X17)
pro$logX18 <- log(pro$X18)
#pro$logX19 <- log(pro$X19)
pro$logX20 <- log(pro$X20)





#pro <- pro[,-1] # Dump old Sex variable
#pro <- pro[is.finite(rowSums(pro)),]


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


library(rgl)
# 3D plot to show surface more clearly.
library(plot3D)  
open3d()
plot3d(pro$Y ~ pro$X20 + pro$X6, col="blue")


library(plot3D)  
open3d()
plot3d(pro$Y ~ pro$X10 + pro$X6, col="blue")

library(plot3D)  
open3d()
plot3d(pro$Y ~ pro$X17 + pro$X18, col="blue")

library(plot3D)  
open3d()
plot3d(pro$Y ~ pro$X9 + pro$X20, col="blue")

library(plot3D)  
open3d()
plot3d(pro$Y ~ pro$X21 + pro$X20, col="blue")


library(flexmix)


mat_pro <-  read.table("~/stat852/data/mat2015.csv", header=TRUE, sep=",", na.strings=" ")




