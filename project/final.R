# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)
pro.test$X21 <- as.numeric(pro.test$X21)

test <-  read.table("~/Desktop/rui_wang_predict.csv", header=TRUE, sep=",", na.strings=" ")

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


pro.test$logX1   <- log(pro.test$X1 )
pro.test$logX2   <- log(pro.test$X2 )
pro.test$logX3   <- log(pro.test$X3 )
pro.test$logX4   <- log(pro.test$X4 )
pro.test$logX5   <- log(pro.test$X5 )
pro.test$logX6   <- log(pro.test$X6 )
pro.test$logX7   <- log(pro.test$X7 )
pro.test$logX8   <- log(pro.test$X8 )
pro.test$logX10  <- log(pro.test$X10)
pro.test$logX11  <- log(pro.test$X11)

pro.test$logX13  <- log(pro.test$X13)
pro.test$logX14  <- log(pro.test$X14)
pro.test$logX15  <- log(pro.test$X15)
pro.test$logX16  <- log(pro.test$X16)
pro.test$logX17  <- log(pro.test$X17)
pro.test$logX18  <- log(pro.test$X18)
pro.test$logX20  <- log(pro.test$X20)


x.1 <- as.matrix(pro[,-22])
y.1 <- as.matrix(pro[,22])
x.2 <- as.matrix(pro[,1:21])
y.2 <- as.matrix(pro[,22])

x.3 <- as.matrix(pro.test)

x.4 <- as.matrix(pro.test[,1:21])

#----------------All Subset BIC ---------------#
library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub.bic <- regsubsets(x=x.1, y=y.1, nbest=1, nvmax=6,really.big=TRUE)
order.bic <- order(summary(allsub.bic)$bic)
allsub.cols <- which(summary(allsub.bic)$which[order.bic[1],]==TRUE)
allsub.cols1 <- allsub.cols[-1]-1
bic.x1 <- x.1[,allsub.cols1]
bic.x3 <- x.3[,allsub.cols1]
beta <- coef(allsub.bic,id=order.bic[1])
smse.bic <- cbind(1,bic.x1)%*%beta 
mspe.bic<- cbind(1,bic.x3)%*%beta 


sMSE.allsub.bic <- mean((y.1 - smse.bic)^2)
#MSPE.allsub.bic <- mean((y.2 - mspe.bic)^2)



#-----------------------Extreme Gradient Boosting Machine----------------------#
require(xgboost)
pro.sparse <- xgboost(silent = 1, data = x.2, label = y.2, max.depth = 10, eta = 0.1, nround = 1000, alpha=0.2,lambda=0.1,num_parallel_tree=10,
                      colsample_bytree = 0.7, subsample = 0.9,
                      objective ="reg:linear",maximize=TRUE)

pro.smse <- mean((y.2 - predict(pro.sparse, newdata = x.2))^2)

#pro.mspe <- mean((y.4- predict(pro.sparse, newdata = x.4))^2)


print("Extreme Gradient Boosting Working: ")
#-----------------------------Gradient Boosting Machine----------------------#
library(gbm)


pro.boost <- gbm(data=data.frame(x.2,Y=y.2), Y ~ ., distribution="gaussian", 
                 n.trees=3000, interaction.depth=10,verbose=FALSE, shrinkage=0.005,
                 bag.fraction=0.75, cv.folds=5,n.cores=16)

min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 

pro.smse.boost <- mean((y.2- predict(pro.boost,newdata=data.frame(x.2),n.trees=3000))^2)

#pro.mspe.boost <- mean((y.4- predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))^2)





ensemble <- (1/3) * ( mspe.bic + predict(pro.sparse, newdata = x.4) + predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))


write.csv(ensemble, "~/Desktop/rui_wang_predict.csv", row.names=FALSE)
