

# Bayesian model averaging via BIC using BMA package
pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

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




#pro$X20.6 <- pro$X20 * pro$X6




rice <- runif(1,0,1)
set.seed(rice * 10000000) 
pro$set <- ifelse(runif(n=nrow(pro))>0.75, yes=2, no=1)



x.3 <- as.matrix(pro[which(pro$set==1),1:21])
y.3 <- as.matrix(pro[which(pro$set==1),22])

x.4 <- as.matrix(pro[which(pro$set==2),1:21])
y.4 <- as.matrix(pro[which(pro$set==2),22])


x.3.scaled <- rescale(x.3, x.3)
x.4.scaled <- rescale(x.4, x.3)

require(xgboost)
pro.sparse <- xgboost(booster = 'gblinear', data = x.3, label = y.3, lambda = 1, alpha = 1,  nrounds = 1000,
                      objective ="reg:linear")

pro.smse <- mean((y.1 - predict(pro.sparse, newdata = x.3))^2)
pro.mspe <- mean((y.2 - predict(pro.sparse, newdata = x.3))^2)



library(gbm)


pro.boost <- gbm(data=data.frame(x.1,Y=y.1), Y ~ ., distribution="gaussian", 
                 n.trees=3000, interaction.depth=3,verbose=FALSE, shrinkage=0.01,
                 bag.fraction=0.25, cv.folds=10,n.cores=16)

min.tree <- gbm.perf(pro.boost, method="cv" ) #Make plot of training error and CV MSPE 

pro.smse.boost <- mean((y.3 - predict(pro.boost,newdata=data.frame(x.3),n.trees=3000))^2)

pro.mspe.boost <- mean((y.4 - predict(pro.boost,newdata=data.frame(x.4),n.trees=min.tree))^2)






#-----------------------------------------Regression Tree----------------------------------------#

library(rpart)
####################################################################
## Default tree
####################################################################

control1 <- rpart.control(xval = 5)
abelone.tree <- rpart(data=abelone[which(abelone$set==1),-c(10,11,12)],Rings ~ ., method="anova", 
                      control = control1, cp = 0)


pred.regtree <- predict(abelone.tree, newdata = abelone[which(abelone$set==2),-c(10,11,12)])

MSPE.rpart[i] <-  mean((data.frame(y.2)-pred.regtree)^2)

cpt <- abelone.tree$cptable
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
abelone.prune.min <- prune(abelone.tree, cp=cp.min)
abelone.prune.1se <- prune(abelone.tree, cp=cp.1se)

pred.prune <- predict(abelone.prune.1se, newdata = abelone[which(abelone$set==2),-c(10,11,12)])


MSPE.rpart.prune[i] <- mean((data.frame(y.2)-pred.prune)^2)










library(earth)
prostate.earth <- earth(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X10 + X11 + X12+ X13 + X14 + X15 + X16+ X17
                        + X18 + X19 + X20,
                        data=pro[which(pro$set==1),-23], trace=3, degree = 1)




pred.earth.3 <- predict(prostate.earth, newdata = pro[which(pro$set==1),])

pred.earth.4 <- predict(prostate.earth, newdata = pro[which(pro$set==2),])


sMSE.earth <- mean((y.1 - pred.earth.3)^2)
MSPE.earth <- mean((y.2- pred.earth.4)^2)



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







