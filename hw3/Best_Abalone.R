# Bayesian model averaging via BIC using BMA package
abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)

# Scatterplot matrix of all but nominal variable

#quartz(h=12, w=12)
#pairs(x=abelone[,-1])

# Add interactions with sex and the log of each variable
abelone$sexm.len  <- abelone$sex.m * abelone$Length
abelone$sexm.diam <- abelone$sex.m * abelone$Diameter
abelone$sexm.hgt  <- abelone$sex.m * abelone$Height
abelone$sexm.whol <- abelone$sex.m * abelone$Whole
abelone$sexm.shuc <- abelone$sex.m * abelone$Shucked
abelone$sexm.vic  <- abelone$sex.m * abelone$Viscera
abelone$sexm.shel <- abelone$sex.m * abelone$Shell
abelone$sexf.len  <- abelone$sex.f * abelone$Length
abelone$sexf.diam <- abelone$sex.f * abelone$Diameter
abelone$sexf.hgt  <- abelone$sex.f * abelone$Height
abelone$sexf.whol <- abelone$sex.f * abelone$Whole
abelone$sexf.shuc <- abelone$sex.f * abelone$Shucked
abelone$sexf.vic  <- abelone$sex.f * abelone$Viscera
abelone$sexf.shel <- abelone$sex.f * abelone$Shell
abelone$loglen  <- log(abelone$Length)
abelone$logdiam <- log(abelone$Diameter)
abelone$loghgt <- log(abelone$Height)
abelone$logwhol<- log(abelone$Whole)
abelone$logshuc<- log(abelone$Shucked)
abelone$logvic <- log(abelone$Viscera)
abelone$logshel<- log(abelone$Shell)

#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test


abelone <- abelone[,-1] # Dump old Sex variable
abelone <- abelone[is.finite(rowSums(abelone)),]


abelone <- scale(abelone[,1:31])

head(abelone)

abelone <- as.data.frame(abelone)



# Model averaging a linear model is done with bicreg().  
# Other functions can handle other regression models
#  Calculations are based on leaps() all-subsets regression with BIC on each model
#  Probabilities are found as shown in lecture notes
# 


library(leaps)



  
  #-----------------Randomize on Data Selection-------------#
   set.seed(29003092)
   
   U <- runif(n=nrow(abelone))
   abelone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))

  
  
  
  x.1 <- as.matrix(abelone[which(abelone$set==1),-c(8,32)])
  y.1 <- as.matrix(abelone[which(abelone$set==1),8])
  x.2 <- as.matrix(abelone[which(abelone$set==2),-c(8,32)])
  y.2 <- as.matrix(abelone[which(abelone$set==2),8])
  x.3 <- as.matrix(abelone[which(abelone$set==3),-c(8,32)])
  y.3 <- as.matrix(abelone[which(abelone$set==3),8])
  
  
  
  #------------------------LASSO--------------------#
  library(glmnet)
  lasson <- cv.glmnet(x = x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 5, alpha=1)
  #lasson <- glmnet(x = x.1, y= y.1, family="gaussian", alpha=1)
  # plot(lasson) # Plots coefficient path
  #  coef(lasson) # Lists out coefficients for each lambda

  predict.1se.1 <- predict(lasson, newx=x.1, s = lasson$lambda.1se)
  predict.1se.2 <- predict(lasson, newx=x.2, s = lasson$lambda.1se)
  predict.1se.3 <- predict(lasson, newx=x.3, s = lasson$lambda.1se)

  
  sMSE.lasson.1se <- mean((y.1 - predict.1se.1)^2)
  valid.lasson.1se <- mean((y.2 - predict.1se.2)^2)
  MSPE.lasson.1se <- mean((y.3 - predict.1se.3)^2)
  
  

  
  
  #---------------------RIDGE----------------------#
  library(glmnet)
  ridged <- cv.glmnet(x=x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 5,alpha=0)
  #ridged <- glmnet(x=x.1, y= y.1, family="gaussian",alpha=0)
  
  # plot(ridged) # Plots coefficient path
  #  coef(ridged) # Lists out coefficients for each lambda
  #  predict.ridged.1 <- predict(ridged, newx=x.1, s=ridged$lambda.min)
  #  predict.ridged.2 <- predict(ridged, newx=x.2, s=ridged$lambda.min)
  
  predict.ridged.1 <- predict(ridged, newx=x.1, s=ridged$lambda.1se)
  predict.ridged.2 <- predict(ridged, newx=x.2, s=ridged$lambda.1se)
  predict.ridged.3 <- predict(ridged, newx=x.3, s=ridged$lambda.1se)
  
  sMSE.ridged <- mean((y.1 - predict.ridged.1)^2)
  valid.ridged <- mean((y.2 - predict.ridged.2)^2)
  MSPE.ridged <- mean((y.3 - predict.ridged.3)^2)
  
  
  sMSE.sum.ridge[loo] <-  sMSE.ridged
  
  MSPE.sum.ridge[loo] <-  MSPE.ridged
  
  
  
  #----------------All Subset BIC ---------------#
  library(leaps)
  # All subsets regression using the "regsubsets" function from "leaps"
  allsub.bic <- regsubsets(x=x.1[,-31], y=y.1, nbest=1, nvmax=30)
  order.bic <- order(summary(allsub.bic)$bic)
  allsub.cols <- which(summary(allsub.bic)$which[order.bic[1],]==TRUE)
  allsub.cols1 <- allsub.cols[-1]-1
  bic.x1 <- x.1[,allsub.cols1]
  bic.x2 <- x.2[,allsub.cols1]
  bic.x3 <- x.3[,allsub.cols1]
  beta <- coef(allsub.bic,id=order.bic[1])
  smse.bic <- cbind(1,bic.x1)%*%beta 
  valid.bic<- cbind(1,bic.x2)%*%beta 
  mspe.bic<- cbind(1,bic.x2)%*%beta 
  
  
  sMSE.allsub.bic <- mean((y.1 - smse.bic)^2)
  valid.allsub.bic <- mean((y.2 - valid.bic)^2)
  MSPe.allsub.bic <- mean((y.3 - mspe.bic)^2)

  
  
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
  predict.step.3 <- step.pred(stepon, x.3)
  
  
  sMSE.step <- mean((y.1 - predict.step.1)^2)
  valid.step <- mean((y.2 - predict.step.2)^2)
  MSPE.step <- mean((y.3 - predict.step.3)^2)
  
  
  #--------------Step Wise BIC Backward-----------------#
  
  back.fit <- function(x,y) {
    data=data.frame(y,x)
    initial.1 <- lm(data=data,formula=y~ 1)
    final.1 <- lm(data=data, formula=y~.)
    step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,direction = "backward",
                  k = log(nrow(data)))
    step1
  }
  
  back.pred <- function(fit,x){
    predict(fit,as.data.frame(x))
  }
  
  
  
  backon <- back.fit(x.1,y.1);
  predict.back.1 <- step.pred(backon, x.1)
  predict.back.2 <- step.pred(backon, x.2)
  predict.back.3 <- step.pred(backon, x.3)
  
  
  sMSE.back <- mean((y.1 - predict.back.1)^2)
  valid.back <- mean((y.2 - predict.back.2)^2)
  MSPE.back <- mean((y.3 - predict.back.3)^2)
  

  
  
  
  
  #-----------------Bayesian Model Averaging--------------#
  library(BMA)
  
  
  
  mod.avg <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 80)
  bma.coef <- mod.avg$postmean
  
  
  bma.pred1 <- cbind(1,x.1)%*%bma.coef
  mspe.bma1 <- mean((y.1-bma.pred1)^2)
  bma.pred3 <- cbind(1,x.3)%*%bma.coef
  mspe.bma3 <- mean((y.3-bma.pred3)^2)
  bma.pred2 <- cbind(1,x.2)%*%bma.coef
  mspe.bma2 <- mean((y.2-bma.pred2)^2)
  bma.pred3 <- cbind(1,x.3)%*%bma.coef
  mspe.bma3 <- mean((y.3-bma.pred3)^2)
  

