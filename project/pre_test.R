# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

pro.test$X21 <- as.numeric(pro.test$X21)





rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}



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


for(i in 1:20)

{

rice <- runif(1,0,1)
set.seed(rice * 10000000) 
pro$set <- ifelse(runif(n=nrow(pro))>0.75, yes=2, no=1)



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




#---------------------------PPR---------------------------------#

ppr.iter <- ppr(data=pro[which(pro$set==1),-23], Y ~ ., nterms=2, optlevel=3)

pred.ppr.3 <-  predict(ppr.iter, newdata=pro[which(pro$set==1),])
pred.ppr.4 <- predict(ppr.iter, newdata=pro[which(pro$set==2),])


ppr.smse <- mean((data.frame(y.3)-pred.ppr.3)^2)
ppr.mspe <- mean((data.frame(y.4)-pred.ppr.4)^2)

sMSE.ppr[i] <- ppr.smse
MSPE.ppr[i] <- ppr.mspe

#------------------Random Forest----------------------------------#


library(randomForest)

pro.randf <- randomForest(data=cbind(data.frame(x.3),Y=y.3), Y ~ ., 
                          importance=TRUE, ntree=10000, mtry= 15,  keep.forest=TRUE)



pro.randf.smse <- mean((predict(pro.randf,x.3)-y.3)^2)

pro.randf.mspe <- mean((predict(pro.randf,x.4)-y.4)^2)


sMSE.randf[i] <- pro.randf.smse 
MSPE.randf[i] <- pro.randf.mspe
#--------------------MARS--------------------#

library(earth)
prostate.earth <- earth(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12+ X13 + X14 + X15 + X16+ X17
                        + X18 + X19 + X20 + X21,
                        data=pro[which(pro$set==1),-23], trace=3, degree = 1)




pred.earth.3 <- predict(prostate.earth, newdata = pro[which(pro$set==1),])

pred.earth.4 <- predict(prostate.earth, newdata = pro[which(pro$set==2),])

sMSE.earth[i] <- mean((y.3 - pred.earth.3)^2)
MSPE.earth[i] <- mean((y.4 - pred.earth.4)^2)


#-----------------------Extreme Gradient Boosting Machine----------------------#
require(xgboost)
pro.sparse <- xgboost(data = x.3, label = y.3, max.depth = 10, eta = 0.05, alpha=0.05,lambda=0.05,num_parallel_tree=10,
                      nround = 1000, 
                      objective ="reg:linear", maximize=TRUE)

pro.smse <- mean((y.3 - predict(pro.sparse, newdata = x.3))^2)
pro.mspe <- mean((y.4 - predict(pro.sparse, newdata = x.4))^2)


sMSE.extrem[i] <- pro.smse
MSPE.extrem[i] <- pro.mspe

#-----------------------------Gradient Boosting Machine----------------------#
library(gbm)


pro.boost <- gbm(data=data.frame(x.3,Y=y.3), Y ~ ., distribution="gaussian", 
                 n.trees=5000, interaction.depth=10,verbose=FALSE, shrinkage=0.005,
                 bag.fraction=0.75, cv.folds=5,n.cores=8)

min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 

pro.smse.boost <- mean((y.3- predict(pro.boost,newdata=data.frame(x.3),n.trees=12000))^2)

pro.mspe.boost <- mean((y.4- predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))^2)


sMSE.boost[i] <- pro.smse.boost
MSPE.boost[i] <- pro.mspe.boost


#----------------------------------Neural Network--------------------------------#
library(nnet)
nn <- nnet(y=y.3, x=x.3.scaled, linout=TRUE, size=2, decay=0.01, maxit=500, trace=FALSE)

pred.nn.3 <- predict(nn,newdata = x.3.scaled)

pred.nn.4 <- predict(nn,newdata = x.4.scaled)

pro.nn.smse <-  mean((y.3 - pred.nn.3)^2)
pro.nn.mspe <-  mean((y.4 - pred.nn.4)^2)

sMSE.nn[i] <- pro.nn.smse
MSPE.nn[i] <- pro.nn.mspe




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



#----------------------------------Data Calcualtion-----------------------------#


split_sMSE <- cbind(sMSE.gam[i], sMSE.ppr[i], sMSE.nn[i], sMSE.extrem[i], sMSE.boost[i], sMSE.earth[i],sMSE.randf[i])
split_MSPE <- cbind(MSPE.gam[i], MSPE.ppr[i], MSPE.nn[i], MSPE.extrem[i], MSPE.boost[i], MSPE.earth[i],MSPE.randf[i],MSPE.rpart[i])

min_MSPE <- min(split_MSPE)


MSPE.gam.re[i] <- gam.mspe / min_MSPE

MSPE.ppr.re[i] <- ppr.mspe / min_MSPE



MSPE.earth.re[i] <- MSPE.earth[i] / min_MSPE

MSPE.extrem.re[i] <- MSPE.extrem[i] / min_MSPE

MSPE.boost.re[i] <- MSPE.boost[i] / min_MSPE

MSPE.nn.re[i] <- MSPE.nn[i] / min_MSPE

MSPE.randf.re[i] <- MSPE.randf[i] / min_MSPE

MSPE.rpart.re[i] <- MSPE.rpart[i] / min_MSPE
}


MSE <- data.frame(random = MSPE.randf, extrem = MSPE.extrem, boost = MSPE.boost, gam = MSPE.gam, ppr <- MSPE.ppr,
                  earth = MSPE.earth, nnet = MSPE.nn, rpart = MSPE.rpart)

MSE.re <- data.frame(random = MSPE.randf.re, extrem = MSPE.extrem.re, boost = MSPE.boost.re, gam = MSPE.gam.re, ppr <- MSPE.ppr.re,
                     earth = MSPE.earth.re, nnet = MSPE.nn.re, rpart = MSPE.rpart.re )
MSE <- sqrt(MSE)

MSE.re <- sqrt(MSE.re)

boxplot(MSE)

boxplot(MSE.re)



