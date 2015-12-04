# Bayesian model averaging via BIC using BMA package
abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")

abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)


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

set.seed(29003092)
U <- runif(n=nrow(abelone))
abelone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))
abelone <- abelone[,-1] # Dump old Sex variable
abelone <- abelone[is.finite(rowSums(abelone)),]
#head(abelone)

x.1 <- as.matrix(abelone[which(abelone$set==1),-c(8,32)])
y.1 <- as.matrix(abelone[which(abelone$set==1),8])
x.2 <- as.matrix(abelone[which(abelone$set==2),-c(8,32)])
y.2 <- as.matrix(abelone[which(abelone$set==2),8])
x.3 <- as.matrix(abelone[which(abelone$set==3),-c(8,32)])
y.3 <- as.matrix(abelone[which(abelone$set==3),8])

# Model averaging a linear model is done with bicreg().  
# Other functions can handle other regression models
#  Calculations are based on leaps() all-subsets regression with BIC on each model
#  Probabilities are found as shown in lecture notes
# 

library(BMA)

mod.avg1 <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 80)
bma.coef <- mod.avg1$postmean
bma.pred2 <- cbind(1,x.2)%*%bma.coef
mspe.bma2 <- mean((y.2-bma.pred2)^2)
bma.pred3 <- cbind(1,x.3)%*%bma.coef
mspe.bma3 <- mean((y.3-bma.pred3)^2)


library(MASS)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec <- lm.ridge (data=abelone[which(abelone$set==1),-32], Rings ~ ., lambda = seq(0, 2, .01))
select(ridgec)
ridge.final <- lm.ridge(data=abelone[which(abelone$set==1),-32], Rings ~ ., lambda=1.95)
ridge.coef <- coef(ridge.final)
ridge.pred2 <- cbind(1,x.2)%*%ridge.coef
mspe.ridge2 <- mean((y.2-ridge.pred2)^2)
ridge.pred3 <- cbind(1,x.3)%*%ridge.coef
mspe.ridge3 <- mean((y.3-ridge.pred3)^2)


library(glmnet)

# cv.glmnet() uses crossvalidation to estimate optimal lambda
cv.lasso.2 <- cv.glmnet(y=y.1, x= x.1[,-31], family="gaussian")
lasso.pred2 <- predict(cv.lasso.2, newx=x.2)
mspe.lasso2 <- mean((y.2-lasso.pred2)^2)
lasso.pred3 <- predict(cv.lasso.2, newx=x.3)
mspe.lasso3 <- mean((y.3-lasso.pred3)^2)


library(lars)
lasso.1 <- lars(y=y.1, x=x.1, type="lasso")
cv.lasso.1 <- cv.lars(y=y.1, x= x.1, type="lasso")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lasso.1$cv) + cv.lasso.1$cv.error[which.min(cv.lasso.1$cv)]
s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv < limit))]
# Print out coefficients at optimal s=0.2424.
lasso.pred2l <- predict(lasso.1, newx=x.2, s=s.cv.1, mode="fraction")$fit
mspe.lasso2l <- mean((y.2-lasso.pred2l)^2)


lar.1 <- lars(y=y.1, x=x.1, type="lar")
cv.lar.1 <- cv.lars(y=y.1, x= x.1, type="lar")
# Use the "+1SE rule" to find best model: 
#    Take the min CV and add its SE ("limit").  
#    Find smallest model that has its own CV within this limit (at "s.cv.1")
limit <- min(cv.lar.1$cv) + cv.lar.1$cv.error[which.min(cv.lar.1$cv)]
s.cv.1 <- cv.lar.1$index[min(which(cv.lar.1$cv < limit))]
# Print out coefficients at optimal s.
lar.pred2 <- predict(lar.1, newx=x.2, s=s.cv.1, mode="step")$fit
mspe.lar2 <- mean((y.2-lar.pred2)^2)
lar.pred3 <- predict(lar.1, newx=x.3, s=s.cv.1, mode="step")$fit
mspe.lar3 <- mean((y.3-lar.pred3)^2)


ols <- lm(data=abelone[which(abelone$set==1),-32], Rings ~ .)
ols.pred2 <- predict(ols, newdata=data.frame(x.2))
mspe.ols2 <- mean((y.2-ols.pred2)^2)
ols.pred3 <- predict(ols, newdata=data.frame(x.3))
mspe.ols3 <- mean((y.3-ols.pred3)^2)


initial.1 <- lm(data=abelone[which(abelone$set==1),-32],formula=Rings~ 1)
final.1 <- lm(data=abelone[which(abelone$set==1),-32], formula=Rings~.)
step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,
              k = log(nrow(abelone[which(abelone$set==1),-32])))
step.pred2 <- predict(step1,as.data.frame(x.2))
mspe.step2 <- mean((y.2-step.pred2)^2)
step.pred3 <- predict(step1,as.data.frame(x.3))
mspe.step3 <- mean((y.3-step.pred3)^2)

library(leaps)
fit <- regsubsets(x=x.1, y=y.1, nbest=1, nvmax=30)
fit.bic <- summary(fit)$bic
n <- nrow(x.1)
fit.aicc <- fit.bic
for(ind in c(1:30)){
  fit.aicc[ind] <- fit.bic[ind] +(2*n/(n-ind) - log(n))*(ind+1)
}
order.bic <- order(summary(fit)$bic)
fit.cols <- which(summary(fit)$which[order.bic[1],]==TRUE)
fit.cols1 <- fit.cols[-1]-1
fit.x2 <- x.2[,fit.cols1]
beta <- coef(fit,id=order.bic[1])
bic.pred2 <- cbind(1,fit.x2)%*%beta
mspe.bic2 <- mean((y.2-bic.pred2)^2)
fit.x3 <- x.3[,fit.cols1]
bic.pred3 <- cbind(1,fit.x3)%*%beta
mspe.bic3 <- mean((y.3-bic.pred3)^2)

 
order.aicc <- order(fit.aicc)
fit.cols <- which(summary(fit)$which[order.aicc[1],]==TRUE)
fit.cols1 <- fit.cols[-1]-1
fit.x2 <- x.2[,fit.cols1]
beta <- coef(fit,id=order.aicc[1])
aicc.pred2 <- cbind(1,fit.x2)%*%beta
mspe.aicc2 <- mean((y.2-aicc.pred2)^2)
fit.x3 <- x.3[,fit.cols1]
aicc.pred3 <- cbind(1,fit.x3)%*%beta
mspe.aicc3 <- mean((y.3-aicc.pred3)^2)

