# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)


#pro <- pro[,c(20,1:19,21,22)]
pro.test$X21 <- as.numeric(pro.test$X21)


#pro$X9.1 <- pro$X9 * pro$X1
#pro$X9.2 <- pro$X9 * pro$X2
#pro$X9.3 <- pro$X9 * pro$X3
#pro$X9.4 <- pro$X9 * pro$X4
#pro$X9.5 <- pro$X9 * pro$X5
#pro$X9.6 <- pro$X9 * pro$X6
#pro$X9.7 <- pro$X9 * pro$X7
#pro$X9.8 <- pro$X9 * pro$X8
#pro$X9.10<- pro$X9 * pro$X10
#pro$X9.11 <- pro$X9 * pro$X11
#pro$X9.12<- pro$X9 * pro$X12
#pro$X9.13 <- pro$X9 * pro$X13
#pro$X9.14 <- pro$X9 * pro$X14
#pro$X9.15 <- pro$X9 * pro$X15
#pro$X9.16 <- pro$X9 * pro$X16
#pro$X9.17 <- pro$X9 * pro$X17
#pro$X9.18 <- pro$X9 * pro$X18
#pro$X9.19 <- pro$X9 * pro$X19
#pro$X9.20 <- pro$X9 * pro$X20

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

#pro$X20.6 <- pro$X20 * pro$X6




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
  
  
  x.1 <- as.matrix(pro[which(pro$set==1),-c(22,39)])
  y.1 <- as.matrix(pro[which(pro$set==1),22])
  x.2 <- as.matrix(pro[which(pro$set==2),-c(22,39)])
  y.2 <- as.matrix(pro[which(pro$set==2),22])
  
  x.3 <- as.matrix(pro[which(pro$set==1),1:21])
  y.3 <- as.matrix(pro[which(pro$set==1),22])
  y.3.o <- as.matrix(pro[which(pro$set==1),22])
  x.4 <- as.matrix(pro[which(pro$set==2),1:21])
  y.4 <- as.matrix(pro[which(pro$set==2),22])
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
  
  
  
  quartz(h=12, w=12)
  plot(density(y.2),ylim = c(0,0.05))
  lines(density(predict(lasson, newx=x.1, s = lasson$lambda.min)),col="blue")
  lines(density(predict(lasson, newx=x.2, s = lasson$lambda.min)),col="orange")
  
  
  quartz(h=12, w=12)
  plot(density(y.1),ylim = c(0,0.05))
  lines(density(predict(lasson, newx=x.1, s = lasson$lambda.min)),col="blue")
  lines(density(predict(lasson, newx=x.2, s = lasson$lambda.min)),col="orange")
  
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
  
  
  quartz(h=12, w=12)
  plot(density(y.1),ylim = c(0,0.05))
  lines(density(smse.bic),col="blue")
  
  
  quartz(h=12, w=12)
  plot(density(y.2),ylim = c(0,0.05))
  lines(density(mspe.bic),col="orange")
  
  
  sMSE.sum.allsub.bic[i] <-  sMSE.allsub.bic
  MSPE.sum.allsub.bic[i] <-  MSPE.allsub.bic
  
  
  
  
  
  
  require(xgboost)
  pro.sparse <- xgboost(data = x.3, label = y.3, max.depth = 10, eta = 0.05, nround = 1000, alpha=0.05,lambda=0.05,num_parallel_tree=10,
                        objective ="reg:linear",maximize=TRUE)
  
  pro.smse <- mean((y.3 - predict(pro.sparse, newdata = x.3))^2)
  pro.mspe <- mean((y.4- predict(pro.sparse, newdata = x.4))^2)
  
  
  
  library(gbm)
  
  
  pro.boost <- gbm(data=data.frame(x.3,Y=y.3), Y ~ ., distribution="gaussian", 
                   n.trees=5000, interaction.depth=15,verbose=FALSE, shrinkage=0.005,
                   bag.fraction=0.75, cv.folds=5,n.cores=8)
  
  min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 
  
  pro.smse.boost <- mean((y.3 - predict(pro.boost,newdata=data.frame(x.3),n.trees=10000))^2)
  
  pro.mspe.boost <- mean((y.4 - predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))^2)
  
  
  
  
  
  
  #-----------------------------------------Regression Tree----------------------------------------#
  
  library(rpart)
  ####################################################################
  ## Default tree
  ####################################################################
  
  control1 <- rpart.control(xval = 10)
  pro.tree <- rpart(data=pro[which(pro$set==1),-23],Y ~ ., method="anova", 
                    control = control1, cp = 0)
  
  
  pred.regtree <- predict(pro.tree, newdata = pro[which(pro$set==2),-23])
  
  MSPE.rpart <-  mean((data.frame(y.4)-pred.regtree)^2)
  
  cpt <- pro.tree$cptable
  # Find location of minimum error
  minrow <- which.min(cpt[,4])
  # Take geometric mean of cp values at min error and one step up 
  cplow.min <- cpt[minrow,1]
  cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
  cp.min <- sqrt(cplow.min*cpup.min)
  
  # Find smallest row where error is below +1SE
  se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
  # Take geometric mean of cp values at min error and one step up 
  cplow.1se <- cpt[se.row,1]
  cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
  cp.1se <- sqrt(cplow.1se*cpup.1se)
  
  # Do pruning each way
  pro.prune.min <- prune(pro.tree, cp=cp.min)
  pro.prune.1se <- prune(pro.tree, cp=cp.1se)
  
  pred.prune <- predict(pro.prune.1se, newdata = pro[which(pro$set==2),-23])
  
  
  MSPE.rpart.prune <- mean((data.frame(y.4)-pred.prune)^2)
  
  
  
  
  
  
  
  
  
  
  library(earth)
  prostate.earth <- earth(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X10 + X11 + X12+ X13 + X14 + X15 + X16+ X17
                          + X18 + X19 + X20,
                          data=pro[which(pro$set==1),-23], trace=3, degree = 1)
  
  
  
  
  pred.earth.3 <- predict(prostate.earth, newdata = pro[which(pro$set==1),])
  
  pred.earth.4 <- predict(prostate.earth, newdata = pro[which(pro$set==2),])
  
  
  sMSE.earth <- mean((y.1 - pred.earth.3)^2)
  MSPE.earth <- mean((y.2- pred.earth.4)^2)
  
  
  
  
  
  #------------------Random Forest----------------------------------#
  
  
  library(randomForest)
  
  pro.randf <- randomForest(data=cbind(data.frame(x.3),Y=y.3), Y ~ ., 
                            importance=TRUE, ntree=10000, mtry= 15,  keep.forest=TRUE)
  
  
  pro.randf.smse <- mean((predict(pro.randf,x.3)-y.3)^2)
  
  pro.randf.mspe <- mean((predict(pro.randf,x.4)-y.4)^2)
  
  
  
  
  
  quartz(h=12, w=12)
  plot(density(y.4))
  lines(density(predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree)),col="red")
  lines(density(predict(pro.sparse,newdata=x.4)),col="green")
  lines(density(pred.earth.4),col="red")
  
  
  quartz(h=12, w=12)
  plot(density(y.3),ylim = c(0,0.05))
  lines(density(predict(pro.boost,newdata=data.frame(x.3),n.trees=3000)),col="blue")
  lines(density(predict(pro.sparse,newdata=x.3)),col="orange")
  lines(density(pred.earth.3),col="red")
  
  
  points(predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree), col = "blue")
  
  
  mean.3 <- (1/2) * (predict.min.2 + predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree) ) 
  
  
  ensemble <-  mean((y.4 - mean.3)^2)
  
  
  
}

