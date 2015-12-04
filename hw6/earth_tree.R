library(earth)
prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)




set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)


y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

 


x.1.scaled <- rescale(x.1, x.1)

#Prove that it worked
apply(X=x.1.scaled, MARGIN=2, FUN=min)
apply(X=x.1.scaled, MARGIN=2, FUN=max)
x.2.scaled <- rescale(x.2, x.1)
#Prove that it worked, but does not perfectly scale test set
apply(X=x.2.scaled, MARGIN=2, FUN=min)
apply(X=x.2.scaled, MARGIN=2, FUN=max)



####################### MARS Based on Earth ##############################

prostate.earth <- earth(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                        data=prostate[which(prostate$set==1),], trace=3, degree = 1)




pred.earth <- predict(prostate.earth, newdata = prostate[which(prostate$set==2),])

pred.earth.in <- predict(prostate.earth, newdata = prostate[which(prostate$set==1),])

sMSE.earth <- mean((y.1 - pred.earth.in)^2)
MSPE.earth <- mean((y.2 - pred.earth)^2)





#################### Regresssion Tree  ########################


library(rpart)
control1 <- rpart.control(xval = 5)
prostate.tree <- rpart(data=prostate[which(prostate$set==1),-12],lpsa ~ lcavol + lweight + age + lbph 
                       + svi + lcp + gleason + pgg45, method="anova", 
                      control = control1,cp=0)

pred.regtree.in <- predict(prostate.tree, newdata = prostate[which(prostate$set==1),])
pred.regtree <- predict(prostate.tree, newdata = prostate[which(prostate$set==2),])

sMSE.rpart <- mean((data.frame(y.1)-pred.regtree.in)^2)

MSPE.rpart <-  mean((data.frame(y.2)-pred.regtree)^2)


##########################    GAM PPR   ############################


library(mgcv)


gam.iter<- gam(data=prostate[which(prostate$set==1),],  lpsa ~ s(lcavol) + s(lweight) + age + lbph 
               + svi + lcp + gleason,
               family=gaussian(link=identity)) 


pred.gam <- predict(gam.iter,newdata=data.frame(x.2))

pred.gam.in <- predict(gam.iter,newdata=data.frame(x.1))

gam.mspe <- mean((data.frame(y.2)-pred.gam)^2)

gam.smse<- mean((data.frame(y.1)-pred.gam.in)^2)



ppr.iter <- ppr(data=prostate[which(prostate$set==1),],lpsa ~ lcavol + lweight + age + lbph 
                + svi + lcp + gleason, nterms=2, optlevel=3, sm.method="gcv")

pred.ppr <- predict(ppr.iter, newdata=prostate[which(prostate$set==1),])
pred.ppr.in <- predict(ppr.iter, newdata=prostate[which(prostate$set==2),])

ppr.mspe <- mean((data.frame(y.2)-pred.ppr)^2)

ppr.smse <- mean((data.frame(y.1)-pred.ppr.in)^2)

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



#----------------All Subset BIC ---------------#
library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub.bic <- regsubsets(x=x.1, y=y.1, nbest=1, nvmax=30)
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





#----------------All Subset AIC ---------------#

allsub.aic <- regsubsets(x=x.1, y=y.1, nbest=1, nvmax=30)


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





#-----------------Bayesian Model Averaging--------------#
library(MASS)
library(BMA)
mod.avg <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 60)



pred.bma.1 <- predict( mod.avg, newdata = x.1)$mean
pred.bma.2 <- predict( mod.avg, newdata = x.2)$mean


sMSE.bma <- mean((y.1 - pred.bma.1)^2)
MSPE.bma <- mean((y.2 - pred.bma.2)^2)







#-------------------------Neural Network----------------------#

library(nnet)
nn <- nnet(y=y.1, x=x.1.scaled, linout=TRUE, size=8, decay=1.0, maxit=500, trace=FALSE)




pred.nn <- predict(nn,newdata = x.2.scaled)

MSPE.nn <- mean((y.2 - pred.nn)^2)

sMSE.nn <-  nn$value/nrow(x.1.scaled)




#-------------------------Random Forest----------------------------#


prostate.rf.1 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=700, mtry=1, keep.forest=TRUE)

prostate.mspe.1 <- mean((predict(prostate.rf.1,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)



prostate.rf.2 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=600, mtry= ncol(x.1) / 3, keep.forest=TRUE)


prostate.mspe.2<- mean((predict(prostate.rf.2,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)

prostate.rf.3 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=600, mtry=2 * ncol(x.1) / 3, keep.forest=TRUE)


prostate.mspe.3<- mean((predict(prostate.rf.3,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)



prostate.rf.p <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=600, mtry= ncol(x.1) / 3, keep.forest=TRUE)



prostate.mspe.p <- mean((predict(prostate.rf.p,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)

#-------------------------------Gradient Boosting-----------------------------#


prostate.boost.final <- gbm(data=data.frame(x.1,lpsa=y.1), lpsa ~ ., distribution="gaussian", 
                            n.trees=3000, interaction.depth=3, shrinkage=0.001,
                            bag.fraction=0.75, cv.folds=0,n.cores=8)

boost.smse <- mean((y.1- predict(prostate.boost.final,newdata=data.frame(x.1),n.trees=3000))^2)
boost.mspe <- mean((y.2- predict(prostate.boost.final,newdata=data.frame(x.2),n.trees=3000))^2)

#------------------------Data Collection-----------------------#

MSPE <- data.frame(ols = MSPE.ols, ridge = sMSE.ridged, lasso.min = MSPE.lasson.min,  lasso.1se = MSPE.lasson.1se, allsub.bic = MSPE.allsub.bic,
           allsub.aic = MSPE.allsub.aic, step = MSPE.step, bma = MSPE.bma, gam= gam.mspe, ppr = ppr.mspe, tree = MSPE.rpart,
           earth = MSPE.earth, nnet = MSPE.nn,randf.1 = prostate.mspe.1,randf.2 = prostate.mspe.2,
           randf.3 = prostate.mspe.3, randf.p = prostate.mspe.p,boost=boost.mspe)


sMSE <- data.frame(ols = sMSE.ols, ridge = MSPE.ridged, lasso.min = sMSE.lasson.min,  lasso.1se = sMSE.lasson.1se, allsub.bic = sMSE.allsub.bic, 
                   allsub.aic = sMSE.allsub.aic, step = sMSE.step, bma = sMSE.bma, gam= gam.smse, ppr = ppr.smse, tree = sMSE.rpart,
               earth = sMSE.earth,nnet = sMSE.nn)

boxplot(sMSE[,1:12])

quartz(h=7,w=12)
boxplot(MSPE)

