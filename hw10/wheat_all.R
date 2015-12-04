library(gbm)
library(nnet)
library(e1071)
wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")

wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]


rescale.set1 <- function(x1,x2){
  minx <- apply(X=x1, MARGIN=2, FUN=min)
  maxx <- apply(X=x1, MARGIN=2, FUN=max)
  x3 <- matrix (nrow=nrow(x2), ncol=ncol(x2))
  for(i in c(1:ncol(x2))){
    x3[,i] <- (x2[,i] - minx[i])/(maxx[i] - minx[i])
  }
  x3
}


kernels <- cbind("Healthy", "Scab", "Sprout")


wheat.boost <- gbm(data=set1, type ~., distribution="multinomial", 
                   n.trees=3000, interaction.depth=5,verbose=FALSE, shrinkage=0.001,
                   bag.fraction=0.25, cv.folds=0,n.cores=16)


pred.boost.0  <-  predict(wheat.boost, newdata=set1[,1:6], n.trees=3000, type="response")

class.boost.0 <- kernels[apply(pred.boost.0[,,1], 1, which.max)]

mspe.boost.0 <- mean(ifelse(class.boost.0 == set1$type, yes=0, no=1))


pred.boost  <-  predict(wheat.boost, newdata=set2[,1:6], n.trees=3000, type="response")

class.boost <- kernels[apply(pred.boost[,,1], 1, which.max)]

mspe.boost <- mean(ifelse(class.boost == set2$type, yes=0, no=1))


table(class.boost, set2$type,  dnn=c("Predicted","Observed"))



x.1.unscaled <- set1[,1:6]
x.1 <- rescale.set1(x.1.unscaled,x.1.unscaled)
y.1 <- class.ind(set1[,7])
y.r <-set1[,7]

x.2.unscaled <- set2[,1:6]
x.2 <- rescale.set1(x.1.unscaled,x.2.unscaled)
y.2 <- class.ind(set2[,7])
y.p <-set2[,7]


nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=0.01, maxit=500, trace=FALSE,softmax=TRUE)

pred.nn.0 <- predict(nn, newdata=x.1, type="class")
misclass.nn.0 <- mean(ifelse(pred.nn.0 == y.r, yes=0, no=1))

pred.nn <- predict(nn, newdata=x.2, type="class")
misclass.nn <- mean(ifelse(pred.nn == y.p, yes=0, no=1))

table(pred.nn, set2$type,  dnn=c("Predicted","Observed"))



wheat.tune.0 <-  tune.svm(data=set1, type  ~ ., kernel="radial", gamma = 10^(-7:-2), cost = 10^(-3:3))
summary(wheat.tune.0)
radi <- summary(wheat.tune.0)$performances
radi[order(radi[,3]),]
#### Note: Optimum is on edge of parameter space. Ought to pursue further (larger) costs
quartz(h=6, w=8)

plot(wheat.tune.0, type="contour", transform.x=log10, transform.y=log10)

quartz(h=6, w=8)
plot(wheat.tune.0, type="perspective", transform.x=log10, transform.y=log10, theta=150)

wheat.tune <-  svm(data=set1, type ~ ., kernel="radial", gamma=.01, cost=100)

pred.radi.0 <- predict(wheat.tune, newdata=set1, type="vector")
(misclass.radi.0 <- mean(ifelse(pred.radi.0 == set1$type, yes=0, no=1)))

pred.radi <- predict(wheat.tune, newdata=set2, type="vector")
(misclass.radi <- mean(ifelse(pred.radi == set2$type, yes=0, no=1)))
table(pred.radi, set2$type,  dnn=c("Predicted","Observed"))

quartz(h=7, w=6, pointsize=10)
plot(wheat.tune, set2, weight~density )



poly.tune.0 <-  tune.svm(data=set1, type  ~ ., kernel="polynomial", gamma = 10^(-5:0), cost = 10^(-3:2), coef0 = (1:8), degree = (1:3))
summary(poly.tune.0)
poly <- summary(poly.tune.0)$performances
poly[order(poly[,3]),]
#### Note: Optimum is on edge of parameter space. Ought to pursue further (larger) costs
quartz(h=7, w=6, pointsize=12)
plot(poly.tune.0, type="contour", gamma ~ cost, transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(poly.tune.0, type="perspective", transform.x=log10, transform.y=log10, theta=150)



poly.tune <-  svm(data=set1, type  ~ ., kernel="polynomial", gamma = 0.01, cost = 100, coef0 =5 , degree = 2)



pred.poly.0 <- predict(poly.tune, newdata=set1, type="vector")
(misclass.poly.0 <- mean(ifelse(pred.poly.0 == set1$type, yes=0, no=1)))

pred.poly <- predict(poly.tune, newdata=set2, type="vector")
(misclass.poly <- mean(ifelse(pred.poly == set2$type, yes=0, no=1)))
table(pred.poly, set2$type,  dnn=c("Predicted","Observed"))









