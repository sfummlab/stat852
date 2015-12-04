abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")

abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)

# Scatterplot matrix of all but nominal variable

#win.graph(h=12, w=12)
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
set.seed(29003092)
U <- runif(n=nrow(abelone))
abelone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))


x.1 <- as.matrix(abelone[which(abelone$set==1),-c(8,32)])
y.1 <- as.matrix(abelone[which(abelone$set==1),8])
x.2 <- as.matrix(abelone[which(abelone$set==2),-c(8,32)])
y.2 <- as.matrix(abelone[which(abelone$set==2),8])
x.3 <- as.matrix(abelone[which(abelone$set==3),-c(8,32)])
y.3 <- as.matrix(abelone[which(abelone$set==3),8])



library(glmnet)
library(bootstrap)

# Model fit function
lasso.fit <- function(x,y){
  cv.glmnet(y=y, x= x, family="gaussian")
  }

lasso.pred <- function(fit,x){
  predict(fit, x)
  }
#  Tested the functions first
#fit <- lasso.fit(x.1,y.1)
#lpred <- lasso.pred(fit,x.1,y.1)

# Crossvalidation using CV with V=10 ("ngroup")
lasso.cv10 <- crossval(x=x.1, y=y.1, theta.fit=lasso.fit, theta.predict=lasso.pred, ngroup=10)
quartz(h=12, w=12)
plot(y=lasso.cv10$cv.fit, x=y.1)
mspe.lasso.cv10 <- mean((y.1 - lasso.cv10$cv.fit)^2)
# Crossvalidation using CV with V=n (supposedly the default).  Why doesn't this work?'
# lasso.cvn <- crossval(x=x.1, y=y.1, theta.fit=lasso.fit, theta.predict=lasso.pred, ngroup=length(y.1))
# win.graph(h=12, w=12)
# plot(y=lasso.cvn$cv.fit, x=y.1)
# mspe.lasso.cvn <- mean((y.1 - lasso.cvn$cv.fit)^2)
# MSPE on test data set 2 & 3
lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian",)
lasso.pred.x23 <- predict(lasso,x.23)
mspe.lasso.x23 <- mean((y.23 - lasso.pred.x23)^2)
lasso.pred.x1 <- predict(lasso,x.1)
mspe.lasso.x1 <- mean((y.1 - lasso.pred.x1)^2)


lasso.pred.x3 <- predict(lasso,x.3)
mspe.lasso.x3 <- mean((y.3 - lasso.pred.x3)^2)


library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub <- regsubsets(x=x.1[,-31], y=y.1, nbest=1, nvmax=30)
order.bic <- order(summary(allsub)$bic)
allsub.cols <- which(summary(allsub)$which[order.bic[1],]==TRUE)
allsub.cols1 <- allsub.cols[-1]-1
allsub.x1 <- x.1[,allsub.cols1]
allsub.x3 <- x.3[,allsub.cols1]
beta <- coef(allsub,id=order.bic[1])
allsub.pred.x1 <- cbind(1,allsub.x1)%*%beta 
allsub.pred.x3 <- cbind(1,allsub.x3)%*%beta 


mspe.allsub.x3 <- mean((y.3 - allsub.pred.x3)^2)
mspe.allsub.x1 <- mean((y.1 - allsub.pred.x1)^2)

allsub.fit <- function(x,y) {data=data.frame(y,x)
                           allsub <- regsubsets(x=x, y=y, nbest=1, nvmax=30)
                           allsub
}

allsub.pred <- function(fit,x){
                           order.bic <- order(summary(fit)$bic)
                           fit.cols <- which(summary(fit)$which[order.bic[1],]==TRUE)
                           fit.cols1 <- fit.cols[-1]-1
                           fit.x1 <- x[,fit.cols1]
                           beta <- coef(fit,id=order.bic[1])
                           cbind(1,fit.x1)%*%beta 
}

#fit <- allsub.fit(x.1[,-31],y.1)
#allsub.pred(fit, x.1[,-31])

allsub.cv10 <- crossval(x=x.1[,-31], y=y.1, theta.fit=allsub.fit, theta.predict=allsub.pred, ngroup=10)
quartz(h=12, w=12)
plot(y=allsub.cv10$cv.fit, x=y.1)
mspe.allsub.cv10 <- mean((y.1 - allsub.cv10$cv.fit)^2)

allsub.cv5 <- crossval(x=x.1[,-31], y=y.1, theta.fit=allsub.fit, theta.predict=allsub.pred, ngroup=5)
quartz(h=12, w=12)
plot(y=allsub.cv5$cv.fit, x=y.1)
mspe.allsub.cv5 <- mean((y.1 - allsub.cv5$cv.fit)^2)


