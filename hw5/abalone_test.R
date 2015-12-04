# Bayesian model averaging via BIC using BMA package
abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)

abelone <- abelone[,-1] # Dump old Sex variable
abelone <- abelone[is.finite(rowSums(abelone)),]



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



MSPE.gam<- rep(0,20)
MSPE.ppr <- rep(0,20)
MSPE.gam.re <- rep(0,20)
MSPE.ppr.re <- rep(0,20)

library(mgcv)


for(i in 1:20)
{
  
  
  #-----------------Randomize on Data Selection-------------#
  rice <- runif(1,0,1)
  set.seed(rice * 10000000) 
  abelone$set <- ifelse(runif(n=nrow(abelone))>0.50, yes=2, no=1)
  
  
  x.1 <- as.matrix(abelone[which(abelone$set==1),-c(8,32)])
  y.1 <- as.matrix(abelone[which(abelone$set==1),8])
  x.2 <- as.matrix(abelone[which(abelone$set==2),-c(8,32)])
  y.2 <- as.matrix(abelone[which(abelone$set==2),8])
  
  
  
  
  
  
  
  
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
  lasson <- cv.glmnet(x = x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 5, alpha=1)
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
  allsub.bic <- regsubsets(x=x.1[,-31], y=y.1, nbest=1, nvmax=30)
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
  
  
  
  
  #----------------All Subset AIC ---------------#
  
  allsub.aic <- regsubsets(x=x.1[,-31], y=y.1, nbest=1, nvmax=30)
  
  
  allsub.aic.bic <- summary(allsub.aic)$bic
  
  
  n <- nrow(x.1)
  allsub.aic.aicc <- allsub.aic.bic
  for(ind in c(1:30)){
    allsub.aic.aicc[ind] <- allsub.aic.bic[ind] +(2*n/(n-ind) - log(n))*(ind+1)
  }
  
  
  fit.cols <- which(summary(allsub.aic)$which[order.bic[1],]==TRUE)
  fit.cols1 <- fit.cols[-1]-1
  fit.x2 <- x.2[,fit.cols1]
  
  
  
  order.aicc <- order(allsub.aic.aicc)
  fit.cols <- which(summary(allsub.aic)$which[order.aicc[1],]==TRUE)
  fit.cols1 <- fit.cols[-1]-1
  fit.x1 <- x.1[,fit.cols1]
  beta <- coef(allsub.aic,id=order.aicc[1])
  aicc.pred1 <- cbind(1,fit.x1)%*%beta
  sMSE.allsub.aic <- mean((y.1-aicc.pred1)^2)
  fit.x2 <- x.2[,fit.cols1]
  aicc.pred2 <- cbind(1,fit.x2)%*%beta
  MSPE.allsub.aic<- mean((y.2-aicc.pred2)^2)
  
  
  
  sMSE.sum.allsub.aic[i] <-  sMSE.allsub.aic
  MSPE.sum.allsub.aic[i] <-  MSPE.allsub.aic
  
  
  
  
  
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
  
  
  
  
  #-----------------Bayesian Model Averaging--------------#
  library(MASS)
  library(BMA)
  mod.avg <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 60)
  
  
  #coef.bma <-  summary(mod.avg)[,4]
  
  #coef.bma <- as.numeric(coef.bma)
  
  #coef.bma[is.na(coef.bma)] <- 0.0
  
  #coef.bma <- coef.bma[1:31]
  
  
  pred.bma.1 <- predict( mod.avg, newdata = x.1)$mean
  pred.bma.2 <- predict( mod.avg, newdata = x.2)$mean
  
  
  sMSE.bma <- mean((y.1 - pred.bma.1)^2)
  MSPE.bma <- mean((y.2 - pred.bma.2)^2)
  
  sMSE.sum.bma[i] <- sMSE.bma
  MSPE.sum.bma[i] <- MSPE.bma
  
  
  
  
  
  gam.iter<- gam(data=abelone[which(abelone$set==1),],  Rings ~ s(Length) + s(Diameter) + s(Height)+ s(Whole) + 
                   s(Shucked) + s(Viscera) + s(Shell) + sex.m + sex.f,
                 family=gaussian(link=identity)) 
  
  
  pred.gam <- predict(gam.iter,newdata=data.frame(x.2))
  
  gam.mspe <- mean((data.frame(y.2)-pred.gam)^2)
  
  MSPE.gam[i] <- gam.mspe
  
  ppr.iter <- ppr(data=abelone[which(abelone$set==1),], Rings ~ Length + Height +  Diameter + Whole + 
                    Shucked + Viscera + Shell + sex.m + sex.f, nterms=2, optlevel=3, sm.method="gcv")
  
  
  pred.ppr <- predict(ppr.iter, newdata=abelone[which(abelone$set==2),])
  
  ppr.mspe <- mean((data.frame(y.2)-pred.ppr)^2)
  
  MSPE.ppr[i] <- ppr.mspe
  
  
  
  
  
  
  
  
  
  
  
  
  split_MSPE <- cbind(MSPE.ols, MSPE.lasson.min, MSPE.lasson.1se,  MSPE.allsub.bic,  MSPE.allsub.aic, MSPE.step, MSPE.bma,gam.mspe, ppr.mspe)
  
  
  min_MSPE <- min(split_MSPE)
  
  
  min_MSPE
  
  
 
  
  MSPE.sum.lasso.min.re[i] <- MSPE.lasson.min /min_MSPE
  
  
  
  MSPE.sum.lasso.1se.re[i] <- MSPE.lasson.1se / min_MSPE
  

  MSPE.sum.allsub.bic.re[i] <-  MSPE.allsub.bic / min_MSPE
  

  MSPE.sum.allsub.aic.re[i] <-  MSPE.allsub.aic / min_MSPE

  MSPE.sum.step.re[i] <- MSPE.step / min_MSPE
  

  MSPE.sum.bma.re[i] <- MSPE.bma / min_MSPE
  
  
  
  MSPE.sum.ols.re[i] <-  MSPE.ols / min_MSPE

  
  
  MSPE.gam.re[i] <- gam.mspe / min_MSPE
  
  MSPE.ppr.re[i] <- ppr.mspe / min_MSPE
  
  
}

MSE <- data.frame(ols = MSPE.sum.ols, lasso.min = MSPE.sum.lasso.min,  lasso.1se = MSPE.sum.lasso.1se, allsub.bic = MSPE.sum.allsub.bic,
                  allsub.aic = MSPE.sum.allsub.aic, step = MSPE.sum.step, bma = MSPE.sum.bma, gam= MSPE.gam, ppr = MSPE.ppr)

MSE.re <- data.frame(ols = MSPE.sum.ols.re, lasso.min = MSPE.sum.lasso.min.re,  lasso.1se = MSPE.sum.lasso.1se.re, allsub.bic = MSPE.sum.allsub.bic.re,
                  allsub.aic = MSPE.sum.allsub.aic.re, step = MSPE.sum.step.re, bma = MSPE.sum.bma.re, gam= MSPE.gam.re, ppr = MSPE.ppr.re)
MSE <- sqrt(MSE)

MSE.re <- sqrt(MSE.re)

boxplot(MSE)

boxplot(MSE.re)

quartz(h=12, w=12)
pairs(x=abelone[,-1])
