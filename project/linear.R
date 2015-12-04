# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)


#pro <- pro[,c(20,1:19,21,22)]
pro.test$X21 <- as.numeric(pro.test$X21)




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



pro$X6.2 <- (pro$logX6)^(1/4)

pro$X17.2 <- (pro$logX17)^(1/4)

pro$X4.2 <-  (pro$logX4)^(1/4)

pro$X10.2 <- (pro$logX10)^(1/4)

pro$X13.2 <- (pro$logX13)^(1/2)

pro$X10.6 <- pro$logX10 * pro$logX6

pro$X10.17  <- pro$logX10 * pro$logX17


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}









sMSE.sum.ols <- rep(0,20)
sMSE.sum.lasso.1se <- rep(0,20)
sMSE.sum.lasso.min <- rep(0,20)

sMSE.sum.ridge <- rep(0,20)
sMSE.sum.step <- rep(0,20)
sMSE.sum.allsub.bic <- rep(0,20)
sMSE.sum.allsub.aic <- rep(0,20)
sMSE.sum.bma <- rep(0,20)




MSPE.sum.ols <- rep(0,20)
MSPE.sum.lasso.1se <- rep(0,20)
MSPE.sum.lasso.min <- rep(0,20)

MSPE.sum.ridge <- rep(0,20)
MSPE.sum.step <- rep(0,20)
MSPE.sum.allsub.bic <- rep(0,20)
MSPE.sum.allsub.aic <- rep(0,20)
MSPE.sum.bma <- rep(0,20)

MSPE.sum.ols.re <- rep(0,20)
MSPE.sum.lasso.1se.re <- rep(0,20)
MSPE.sum.lasso.min.re <- rep(0,20)

MSPE.sum.ridge.re <- rep(0,20)
MSPE.sum.step.re <- rep(0,20)
MSPE.sum.allsub.bic.re <- rep(0,20)
MSPE.sum.allsub.aic.re <- rep(0,20)
MSPE.sum.bma.re <- rep(0,20)





for(i in 1:20)
{
  

rice <- runif(1,0,1)
set.seed(rice * 10000000) 
pro$set <- ifelse(runif(n=nrow(pro))>0.75, yes=2, no=1)


x.1 <- as.matrix(pro[which(pro$set==1),-c(22,47)])
y.1 <- as.matrix(pro[which(pro$set==1),22])
x.2 <- as.matrix(pro[which(pro$set==2),-c(22,47)])
y.2 <- as.matrix(pro[which(pro$set==2),22])

x.3 <- as.matrix(pro[which(pro$set==1),1:21])
y.3 <- log(as.matrix(pro[which(pro$set==1),22]))
y.3.o <- as.matrix(pro[which(pro$set==1),22])
x.4 <- as.matrix(pro[which(pro$set==2),1:21])
y.4 <- log(as.matrix(pro[which(pro$set==2),22]))
y.4.o <- as.matrix(pro[which(pro$set==2),22])


#x.3.scaled <- rescale(x.3, x.3)
#x.4.scaled <- rescale(x.4, x.3)


# ----------------------OLS-------------------------#
ols.fit <- function(x,y){
  lsfit(x,y)
}
ols.predict <- function(fit,x){
  cbind(1,x) %*% fit$coef
}



olson <- ols.fit(x.1,y.1)

predict.ols.1 <- ols.predict(olson, x.1)

predict.ols.2 <- ols.predict(olson, x.2)

sMSE.ols<- mean((y.1 - predict.ols.1)^2)
MSPE.ols <- mean((y.2 - predict.ols.2)^2)

sMSE.sum.ols[i] <-  sMSE.ols

MSPE.sum.ols[i] <-  MSPE.ols






#------------------------LASSO--------------------#
library(glmnet)
lasson <- cv.glmnet(x = x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 10, alpha=1)
#lasson <- glmnet(x = x.1, y= y.1, family="gaussian", alpha=1)
# plot(lasson) # Plots coefficient path
#  coef(lasson) # Lists out coefficients for each lambda
predict.min.1 <- predict(lasson, newx=x.1, s = lasson$lambda.min)
predict.min.2 <- predict(lasson, newx=x.2, s = lasson$lambda.min)
predict.1se.1 <- predict(lasson, newx=x.1, s = lasson$lambda.1se)
predict.1se.2 <- predict(lasson, newx=x.2, s = lasson$lambda.1se)
sMSE.lasson.min <- mean((y.1 - predict.min.1)^2)
MSPE.lasson.min <- mean((y.2 - predict.min.2)^2)

sMSE.lasson.1se <- mean((y.1 - predict.1se.1)^2)
MSPE.lasson.1se <- mean((y.2 - predict.1se.2)^2)

sMSE.sum.lasso.min[i] <- sMSE.lasson.min

MSPE.sum.lasso.min[i] <- MSPE.lasson.min


sMSE.sum.lasso.1se[i] <- sMSE.lasson.1se

MSPE.sum.lasso.1se[i] <- MSPE.lasson.1se



#---------------------RIDGE----------------------#
library(glmnet)
ridged <- cv.glmnet(x=x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 5,alpha=0)
#ridged <- glmnet(x=x.1, y= y.1, family="gaussian",alpha=0)

# plot(ridged) # Plots coefficient path
#  coef(ridged) # Lists out coefficients for each lambda
#  predict.ridged.1 <- predict(ridged, newx=x.1, s=ridged$lambda.min)
#  predict.ridged.2 <- predict(ridged, newx=x.2, s=ridged$lambda.min)

predict.ridged.1 <- predict(ridged, newx=x.1, s=ridged$lambda.min)
predict.ridged.2 <- predict(ridged, newx=x.2, s=ridged$lambda.min)
sMSE.ridged <- mean((y.1 - predict.ridged.1)^2)
MSPE.ridged <- mean((y.2 - predict.ridged.2)^2)

sMSE.sum.ridge[i] <-  sMSE.ridged

MSPE.sum.ridge[i] <-  MSPE.ridged



#----------------All Subset BIC ---------------#
library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub.bic <- regsubsets(x=x.1, y=y.1, nbest=1, nvmax=6,really.big=TRUE)
order.bic <- order(summary(allsub.bic)$bic)
allsub.cols <- which(summary(allsub.bic)$which[order.bic[1],]==TRUE)
allsub.cols1 <- allsub.cols[-1]-1
bic.x1 <- x.1[,allsub.cols1]
bic.x2 <- x.2[,allsub.cols1]
beta <- coef(allsub.bic,id=order.bic[1])
smse.bic <- cbind(1,bic.x1)%*%beta 
mspe.bic<- cbind(1,bic.x2)%*%beta 


sMSE.allsub.bic <- mean((y.1 - smse.bic)^2)
MSPE.allsub.bic <- mean((y.2 - mspe.bic)^2)



sMSE.sum.allsub.bic[i] <-  sMSE.allsub.bic
MSPE.sum.allsub.bic[i] <-  MSPE.allsub.bic


#--------------Step Wise BIC -----------------#



step.fit <- function(x,y) {
  data=data.frame(y,x)
  initial.1 <- lm(data=data,formula=y~ 1)
  final.1 <- lm(data=data, formula=y~.)
  step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,
                k = log(nrow(data)))
  step1
}

step.pred <- function(fit,x){
  predict(fit,as.data.frame(x))
}


stepon <- step.fit(x.1,y.1);
predict.step.1 <- step.pred(stepon, x.1)
predict.step.2 <- step.pred(stepon, x.2)



sMSE.step <- mean((y.1 - predict.step.1)^2)
MSPE.step <- mean((y.2 - predict.step.2)^2)

sMSE.sum.step[i] <- sMSE.step
MSPE.sum.step[i] <- MSPE.step



split_MSPE <- cbind(MSPE.ols, MSPE.lasson.min, MSPE.lasson.1se,  MSPE.allsub.bic, # MSPE.allsub.aic,
                    MSPE.step)


min_MSPE <- min(split_MSPE)


min_MSPE




MSPE.sum.lasso.min.re[i] <- MSPE.lasson.min /min_MSPE



MSPE.sum.lasso.1se.re[i] <- MSPE.lasson.1se / min_MSPE


MSPE.sum.allsub.bic.re[i] <-  MSPE.allsub.bic / min_MSPE


#MSPE.sum.allsub.aic.re[i] <-  MSPE.allsub.aic / min_MSPE

MSPE.sum.step.re[i] <- MSPE.step / min_MSPE


#MSPE.sum.bma.re[i] <- MSPE.bma / min_MSPE



MSPE.sum.ols.re[i] <-  MSPE.ols / min_MSPE


}



MSE <- data.frame(ols = MSPE.sum.ols, lasso.min = MSPE.sum.lasso.min,  lasso.1se = MSPE.sum.lasso.1se, allsub.bic = MSPE.sum.allsub.bic,
                   step = MSPE.sum.step)#, bma = MSPE.sum.bma)

MSE.re <- data.frame(ols = MSPE.sum.ols.re, lasso.min = MSPE.sum.lasso.min.re,  lasso.1se = MSPE.sum.lasso.1se.re, allsub.bic = MSPE.sum.allsub.bic.re,
                     step = MSPE.sum.step.re)#, bma = MSPE.sum.bma.re)
MSE <- sqrt(MSE)

MSE.re <- sqrt(MSE.re)

boxplot(MSE)

boxplot(MSE.re)
