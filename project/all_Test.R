# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
#pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

#pro.test$X21 <- as.numeric(pro.test$X21)




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




sMSE.gam <- rep(0,20)
sMSE.ppr <- rep(0,20)
sMSE.earth<- rep(0,20)

sMSE.extrem <- rep(0,20)
sMSE.boost <- rep(0,20)
sMSE.randf <- rep(0,20)
sMSE.nn <- rep(0,20)



MSPE.gam <- rep(0,20)
MSPE.ppr <- rep(0,20)
MSPE.earth<- rep(0,20)
MSPE.rpart <- rep(0,20) 
MSPE.rpart.prune <- rep(0,20) 
MSPE.extrem <- rep(0,20)
MSPE.boost <- rep(0,20)
MSPE.randf <- rep(0,20)
MSPE.nn <- rep(0,20)



MSPE.gam.re <- rep(0,20)

MSPE.ppr.re <- rep(0,20)

MSPE.rpart.re <- rep(0,20) 

MSPE.rpart.prune.re <- rep(0,20) 

MSPE.earth.re<- rep(0,20)

MSPE.extrem.re <- rep(0,20)

MSPE.boost.re <- rep(0,20)

MSPE.nn.re <- rep(0,20)

MSPE.randf.re <- rep(0,20)


MSPE.ensemble <- rep(0,20)


MSPE.ensemble.re <- rep(0,20)


MSPE.ensemble1 <- rep(0,20)

MSPE.ensemble1.re <- rep(0,20)


MSPE.ensemble2 <- rep(0,20)

MSPE.ensemble2.re <- rep(0,20)


MSPE.ensemble3 <- rep(0,20)

MSPE.ensemble3.re <- rep(0,20)


MSPE.ensemble4 <- rep(0,20)

MSPE.ensemble4.re <- rep(0,20)


for(i in 1:20)
{
  
  
  rice <- runif(1,0,1)
  set.seed(rice * 10000000) 
  pro$set <- ifelse(runif(n=nrow(pro))>0.75, yes=2, no=1)
  
  
  x.1 <- as.matrix(pro[which(pro$set==1),-c(22,41)])
  y.1 <- as.matrix(pro[which(pro$set==1),22])
  x.2 <- as.matrix(pro[which(pro$set==2),-c(22,41)])
  y.2 <- as.matrix(pro[which(pro$set==2),22])
  
  x.3 <- as.matrix(pro[which(pro$set==1),1:21])
  y.3 <- as.matrix(pro[which(pro$set==1),22])
  
  x.4 <- as.matrix(pro[which(pro$set==2),1:21])
  y.4 <- as.matrix(pro[which(pro$set==2),22])
  
  y.21.1 <- as.matrix(pro[which(pro$X21<2),22])
  y.21.2 <- as.matrix(pro[which(pro$X21>=2),22])
  
  y.21.1 <- as.matrix(pro[which(pro$X21<2),22])
  y.21.2 <- as.matrix(pro[which(pro$X21>=2),22])
  
  x.3.scaled <- rescale(x.3, x.3)
  x.4.scaled <- rescale(x.4, x.3)
  
  
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
  
  
  print("Lasso Working: ")
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
  
  print("All subset BIC Working: ")
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
  
  
  
  
  print("Stepwise Working: ")
  
  #--------------------------------GAM-----------------------------------------#
  
  library(mgcv)
  gam.iter<- gam(data=pro[which(pro$set==1),-23],  Y ~ s(X6) + s(X17) + s(X18) + X1 + X2 + X3 + X4 + X11 + X12  + s(X10),
                 family=gaussian(link=identity)) 
  
  
  pred.gam.3 <- predict(gam.iter,newdata=data.frame(x.3))
  pred.gam.4 <- predict(gam.iter,newdata=data.frame(x.4))
  
  gam.smse <- mean((data.frame(y.3)-pred.gam.3)^2)
  gam.mspe <- mean((data.frame(y.4)-pred.gam.4)^2)
  
  
  
  sMSE.gam[i] <- mean((data.frame(y.3)-pred.gam.3)^2)
  MSPE.gam[i] <- mean((data.frame(y.4)-pred.gam.4)^2)
  
  
  print("GAM Working: ")
  
  #---------------------------PPR---------------------------------#
  
  ppr.iter <- ppr(data=pro[which(pro$set==1),-23], Y ~ ., nterms=2, optlevel=3)
  
  pred.ppr.3 <-  predict(ppr.iter, newdata=pro[which(pro$set==1),])
  pred.ppr.4 <- predict(ppr.iter, newdata=pro[which(pro$set==2),])
  
  
  ppr.smse <- mean((data.frame(y.3)-pred.ppr.3)^2)
  ppr.mspe <- mean((data.frame(y.4)-pred.ppr.4)^2)
  
  sMSE.ppr[i] <- ppr.smse
  MSPE.ppr[i] <- ppr.mspe
  
  print("PPR  Working: ")
  #------------------Random Forest----------------------------------#
  
  
  library(randomForest)
  
  pro.randf <- randomForest(data=cbind(data.frame(x.3),Y=y.3), Y ~ ., 
                            importance=TRUE, ntree=3000, mtry= 10,  keep.forest=TRUE)
  
  
  
  pro.randf.smse <- mean((predict(pro.randf,x.3)-y.3)^2)
  
  pro.randf.mspe <- mean((predict(pro.randf,x.4)-y.4)^2)
  
  
  sMSE.randf[i] <- pro.randf.smse 
  MSPE.randf[i] <- pro.randf.mspe
  
  
  print("Random Forest Working: ")
  #--------------------MARS--------------------#
  
  library(earth)
  prostate.earth <- earth(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12+ X13 + X14 + X15 + X16+ X17
                          + X18 + X19 + X20 + X21,
                          data=pro[which(pro$set==1),-23], trace=3, degree = 2)
  
  
  
  
  pred.earth.3 <- predict(prostate.earth, newdata = pro[which(pro$set==1),])
  
  pred.earth.4 <- predict(prostate.earth, newdata = pro[which(pro$set==2),])
  
  sMSE.earth[i] <- mean((y.3 - pred.earth.3)^2)
  MSPE.earth[i] <- mean((y.4 - pred.earth.4)^2)
  
  print("Regression Earth Working: ")
  #-----------------------Extreme Gradient Boosting Machine----------------------#
  require(xgboost)
  pro.sparse <- xgboost(silent = 1, data = x.3, label = y.3, max.depth = 10, eta = 0.1, nround = 1000, alpha=0.2,lambda=0.1,num_parallel_tree=10,
                        colsample_bytree = 0.7, subsample = 0.9,
                        objective ="reg:linear",maximize=TRUE)
  
  pro.smse <- mean((y.3 - predict(pro.sparse, newdata = x.3))^2)
  pro.mspe <- mean((y.4- predict(pro.sparse, newdata = x.4))^2)
  
  
  
  sMSE.extrem[i] <- pro.smse
  MSPE.extrem[i] <- pro.mspe
  print("Extreme Gradient Boosting Working: ")
  #-----------------------------Gradient Boosting Machine----------------------#
  library(gbm)
  
  
  pro.boost <- gbm(data=data.frame(x.3,Y=y.3), Y ~ ., distribution="gaussian", 
                   n.trees=3000, interaction.depth=10,verbose=FALSE, shrinkage=0.005,
                   bag.fraction=0.75, cv.folds=5,n.cores=16)
  
  min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 
  
  pro.smse.boost <- mean((y.3- predict(pro.boost,newdata=data.frame(x.3),n.trees=3000))^2)
  
  pro.mspe.boost <- mean((y.4- predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))^2)
  
  
  sMSE.boost[i] <- pro.smse.boost
  MSPE.boost[i] <- pro.mspe.boost
  
  
  print("Gradient Boosting Working: ")
  #----------------------------------Neural Network--------------------------------#
  library(nnet)
  nn <- nnet(y=y.3, x=x.3.scaled, linout=TRUE, size=2, decay=0.01, maxit=500, trace=FALSE)
  
  pred.nn.3 <- predict(nn,newdata = x.3.scaled)
  
  pred.nn.4 <- predict(nn,newdata = x.4.scaled)
  
  pro.nn.smse <-  mean((y.3 - pred.nn.3)^2)
  pro.nn.mspe <-  mean((y.4 - pred.nn.4)^2)
  
  sMSE.nn[i] <- pro.nn.smse
  MSPE.nn[i] <- pro.nn.mspe
  
  
  
  print("Neural Net Working: ")
  #-----------------------------------------Regression Tree----------------------------------------#
  
  library(rpart)
  ####################################################################
  ## Default tree
  ####################################################################
  
  control1 <- rpart.control(xval = 10)
  pro.tree <- rpart(data=pro[which(pro$set==1),-23],Y ~ ., method="anova", 
                    control = control1, cp = 0)
  
  
  pred.regtree <- predict(pro.tree, newdata = pro[which(pro$set==2),-23])
  
  MSPE.rpart[i] <-  mean((data.frame(y.4)-pred.regtree)^2)
  
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
  
  pred.prune <- predict(pro.prune.min, newdata = pro[which(pro$set==2),-23])
  
  
  MSPE.rpart.prune[i] <- mean((data.frame(y.4)-pred.prune)^2)
  
  
  print("Regression Tree Rpart Working: ")
  #------------------------Ensemble Method---------------------#
  
  
  # 0. boosting, extreme, allsubset bic ; 1. boosting, extreme, lasso; 2. boosting, lasso  earth ; 3. random forest, boosting; 4. boosting, extreme
  
  # 0. boosting, extreme, allsubset bic ; 1. boosting, allsubset bic; 3. random forest bic; 4. boosting, extreme
  
  
  mean.ensemble.4 <- 0.5 * (mspe.bic + predict(pro.sparse, newdata = x.4))
  MSPE.ensemble4[i] <- mean((y.4 - mean.ensemble.4)^2)
  
  
  mean.ensemble <- (1/4) * (predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree) + 2 * mspe.bic + predict(pro.sparse, newdata = x.4))
  
  
  MSPE.ensemble[i] <- mean((y.4 - mean.ensemble)^2)
  
  
  mean.ensemble.1 <- 0.5 * (predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree) + mspe.bic)
  
  MSPE.ensemble1[i] <- mean((y.4 - mean.ensemble.1)^2)
  
  
  mean.ensemble.2 <- 0.5 * (predict.min.2 + pred.earth.4 )
  
  MSPE.ensemble2[i] <- mean((y.4 - mean.ensemble.2)^2)
  
  
  mean.ensemble.3 <- (1/2) * (mspe.bic + predict(pro.randf,x.4))
  
  MSPE.ensemble3[i] <- mean((y.4 - mean.ensemble.3)^2)
                            
                            
  #----------------------------------Data Calcualtion-----------------------------#
  
  
  split_sMSE <- cbind(sMSE.gam[i], sMSE.ppr[i], sMSE.nn[i], sMSE.extrem[i], sMSE.boost[i], sMSE.earth[i],sMSE.randf[i], sMSE.sum.ols[i],
                      sMSE.sum.lasso.1se[i],
                      sMSE.sum.lasso.min[i],
                      sMSE.sum.ridge[i],
                      sMSE.sum.step[i],
                      sMSE.sum.allsub.bic[i])

  
  split_MSPE <- cbind(MSPE.ols, MSPE.lasson.min, MSPE.lasson.1se,  MSPE.allsub.bic, # MSPE.allsub.aic,
                      MSPE.step, MSPE.gam[i], MSPE.ppr[i], MSPE.nn[i], MSPE.extrem[i], MSPE.boost[i], 
                      MSPE.earth[i],MSPE.randf[i],MSPE.rpart[i], MSPE.ensemble[i],
                      MSPE.ensemble4[i], MSPE.ensemble3[i] , MSPE.ensemble2[i] ,MSPE.ensemble1[i]  
                      )
  
  
  min_MSPE <- min(split_MSPE)
  
  
  
  MSPE.gam.re[i] <- gam.mspe / min_MSPE
  
  MSPE.ppr.re[i] <- ppr.mspe / min_MSPE
  
  
  
  MSPE.earth.re[i] <- MSPE.earth[i] / min_MSPE
  
  MSPE.extrem.re[i] <- MSPE.extrem[i] / min_MSPE
  
  MSPE.boost.re[i] <- MSPE.boost[i] / min_MSPE
  
  MSPE.nn.re[i] <- MSPE.nn[i] / min_MSPE
  
  MSPE.randf.re[i] <- MSPE.randf[i] / min_MSPE
  
  MSPE.rpart.re[i] <- MSPE.rpart[i] / min_MSPE
  
  
  
  
  MSPE.sum.lasso.min.re[i] <- MSPE.lasson.min /min_MSPE
  
  
  
  MSPE.sum.lasso.1se.re[i] <- MSPE.lasson.1se / min_MSPE
  
  
  MSPE.sum.allsub.bic.re[i] <-  MSPE.allsub.bic / min_MSPE
  
  
  #MSPE.sum.allsub.aic.re[i] <-  MSPE.allsub.aic / min_MSPE
  
  MSPE.sum.step.re[i] <- MSPE.step / min_MSPE
  
  
  #MSPE.sum.bma.re[i] <- MSPE.bma / min_MSPE
  
  
  
  MSPE.sum.ols.re[i] <-  MSPE.ols / min_MSPE
  
  MSPE.ensemble.re[i] <- MSPE.ensemble[i] / min_MSPE
  
  
  MSPE.ensemble1.re[i] <- MSPE.ensemble1[i] /min_MSPE
  
  MSPE.ensemble2.re[i] <- MSPE.ensemble2[i] /min_MSPE
  
  MSPE.ensemble3.re[i] <- MSPE.ensemble3[i] /min_MSPE
  
  MSPE.ensemble4.re[i] <- MSPE.ensemble4[i] /min_MSPE
  
  
  
  cat("ensemble result: ", MSPE.ensemble[i])
  
}



MSE <- data.frame(ols = MSPE.sum.ols, lasso.min = MSPE.sum.lasso.min,  lasso.1se = MSPE.sum.lasso.1se, allsub.bic = MSPE.sum.allsub.bic,
                  step = MSPE.sum.step, random = MSPE.randf, extrem = MSPE.extrem, boost = MSPE.boost, gam = MSPE.gam, ppr <- MSPE.ppr,
                  earth = MSPE.earth, nnet = MSPE.nn, rpart = MSPE.rpart, ensemble = MSPE.ensemble, ensemble1 = MSPE.ensemble1
                  , ensemble2 = MSPE.ensemble2 , ensemble3 = MSPE.ensemble3 , ensemble4 = MSPE.ensemble4)

MSE.re <- data.frame(ols = MSPE.sum.ols.re, lasso.min = MSPE.sum.lasso.min.re,  lasso.1se = MSPE.sum.lasso.1se.re, allsub.bic = MSPE.sum.allsub.bic.re,
                     step = MSPE.sum.step.re, random = MSPE.randf.re, extrem = MSPE.extrem.re, boost = MSPE.boost.re, gam = MSPE.gam.re, ppr <- MSPE.ppr.re,
                     earth = MSPE.earth.re, nnet = MSPE.nn.re, rpart = MSPE.rpart.re, ensemble = MSPE.ensemble.re
                     , ensemble1 = MSPE.ensemble1.re
                     , ensemble2 = MSPE.ensemble2.re , ensemble3 = MSPE.ensemble3.re , ensemble4 = MSPE.ensemble4.re)
MSE <- sqrt(MSE)

MSE.re <- sqrt(MSE.re)

boxplot(MSE,ylim = c(3.8,5.0))

boxplot(MSE.re, ylim = c(1.0,1.2))
