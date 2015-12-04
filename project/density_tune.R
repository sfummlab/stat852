

# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

pro.test$X21 <- as.numeric(pro.test$X21)



rice <- runif(1,0,1)
set.seed(rice * 10000000) 
pro$set <- ifelse(runif(n=nrow(pro))>0.7, yes=2, no=1)



x.3 <- as.matrix(pro[which(pro$set==1),1:21])
y.3 <- as.matrix(pro[which(pro$set==1),22])

x.4 <- as.matrix(pro[which(pro$set==2),1:21])
y.4 <- as.matrix(pro[which(pro$set==2),22])


x.3.scaled <- rescale(x.3, x.3)
x.4.scaled <- rescale(x.4, x.3)

require(xgboost)
#pro.sparse <- xgboost(booster = 'gblinear', data = x.3, label = y.3, lambda = 1, alpha = 1,  nrounds = 1000,
#                      objective ="reg:linear")



require(xgboost)
pro.sparse <- xgboost(data = x.3, label = y.3, max.depth = 10, eta = 0.1, nround = 2000, alpha=0.2,lambda=0.1,num_parallel_tree=10,
                      colsample_bytree = 0.7, subsample = 0.9,
                      objective ="reg:linear",maximize=TRUE)

pro.smse <- mean((y.3 - predict(pro.sparse, newdata = x.3))^2)
pro.mspe <- mean((y.4- predict(pro.sparse, newdata = x.4))^2)



library(gbm)


pro.boost <- gbm(data=data.frame(x.3,Y=y.3), Y ~ ., distribution="gaussian", 
                 n.trees=5000, interaction.depth=15,verbose=FALSE, shrinkage=0.005,
                 bag.fraction=0.75, cv.folds=5,n.cores=8)

min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 

pro.smse.boost <- mean((y.3 - predict(pro.boost,newdata=data.frame(x.3),n.trees=5000))^2)

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
plot(density(y.4),ylim = c(0,0.05))
lines(density(predict(pro.boost,newdata=data.frame(x.2),n.trees=min.tree)),col="blue")
lines(density(predict(pro.sparse,newdata=x.2)),col="orange")
lines(density(pred.earth.4),col="red")


quartz(h=12, w=12)
plot(density(y.3),ylim = c(0,0.05))
lines(density(predict(pro.boost,newdata=data.frame(x.3),n.trees=3000)),col="blue")
lines(density(predict(pro.sparse,newdata=x.3)),col="orange")
lines(density(pred.earth.3),col="red")


points(predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree), col = "blue")







