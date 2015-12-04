wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")

wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]

library(e1071)
####################################################################
## Support Vector Machines using e1071::svm
##  Data are scaled internally to mean 0 variance 1 before analysis
##  type = C-classification (same as in notes) is default if y is a factor
##  kernel= "radial" is default, "linear", "polynomial", and "sigmoid" are options
##         Each kernel has its own tuning parameters:
##         radial: gamma=
##         polynomial: gamma=, degree=, coef0= (gamma is a coefficient on the inner product)
##  cost= is the C-parameter
##  cross= gives # folds for CV estimation of error
##
## Note: does 1 vs. 1 approach to K>2 classification
##
## Just using Gaussian Radial Basis.  Can try others; may work better
####################################################################

svm.1.1 <- svm(data=set1, type ~ ., kernel="radial", gamma=1, cost=1, cross=10)
summary(svm.1.1)
head(svm.1.1$decision.values)
head(svm.1.1$fitted)
svm.1.1$tot.accuracy # Total CV Accuracy
svm.1.1$accuracies # Individual fold accuracies


pred1.1.1 <- predict(svm.1.1, newdata=set1)
table(pred1.1.1, set1$type,  dnn=c("Predicted","Observed"))

pred2.1.1 <- predict(svm.1.1, newdata=set2)
table(pred2.1.1, set2$type,  dnn=c("Predicted","Observed"))
(misclass2.1.1 <- mean(ifelse(pred2.1.1 == set2$type, yes=0, no=1)))


# This plot would work better if I supplied values at which to fix 
#   the other 16 variables, using the slice= argument.  Then 
#   Background fill would be coloured according to prediction.
#   Instead, it assumes the value "0" for all other variables,
#   And that evidently always leads to class 1.
# Support vectors are "x", colours are classes.  
quartz(h=7, w=6, pointsize=10)
plot(svm.1.1, set1, weight~density)





svm.1.2 <- svm(data=set1, type ~ ., kernel="polynomial", gamma=1, cost=1e02, coef0 = 1, degree = 3, cross=10)
summary(svm.1.2)
head(svm.1.2$decision.values)
head(svm.1.2$fitted)
svm.1.2$tot.accuracy # Total CV Accuracy
svm.1.2$accuracies # Individual fold accuracies


pred1.1.2 <- predict(svm.1.2, newdata=set1)
table(pred1.1.2, set1$type,  dnn=c("Predicted","Observed"))

pred2.1.2 <- predict(svm.1.2, newdata=set2)
table(pred2.1.2, set2$type,  dnn=c("Predicted","Observed"))
(misclass2.1.2 <- mean(ifelse(pred2.1.2 == set2$type, yes=0, no=1)))


quartz(h=7, w=6, pointsize=10)
plot(svm.1.2, set1, weight~density)

####################################################################
###  EXTRA COOL AND EXCELLENT!  THIS SAME PACKAGE HAS A tune() 
###   FUNCTION THAT AUTOMATICALLY SELECTS A BEST SET OF TUNING
###   PARAMETERS!!!
### There is a method for svm, randomForest,nnet, rpart, and knn
###  Default is 10-fold CV, but can tune to a validation set
###  (At least in theory you can tune to a validation set, but it 
###  doesn't seem to work...)
####################################################################

wheat.tune <-  tune.svm(data=set1, type  ~ ., kernel="radial", gamma = 10^(-5:0), cost = 10^(-3:3))
summary(wheat.tune)
aa <- summary(wheat.tune)$performances
aa[order(aa[,3]),]
#### Note: Optimum is on edge of parameter space. Ought to pursue further (larger) costs
quartz(h=7, w=6, pointsize=12)
plot(wheat.tune, type="contour", transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(wheat.tune, type="perspective", transform.x=log10, transform.y=log10, theta=150)

poly.tune <-  tune.svm(data=set1, type  ~ ., kernel="polynomial", gamma = 10^(-4:0), cost = 10^(-2:2), coef0 = (1:5), degree = (1:4))
summary(poly.tune)
aa <- summary(poly.tune)$performances
aa[order(aa[,3]),]
#### Note: Optimum is on edge of parameter space. Ought to pursue further (larger) costs
quartz(h=7, w=6, pointsize=12)
plot(poy.tune, type="contour", transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(poly.tune, type="perspective", transform.x=log10, transform.y=log10, theta=150)

# Show tuning on a validation set...or not (see summary)
wheat.tune.val <-  tune.svm(data=set1, type ~ ., kernel="radial", 
                          validation.x=set2, gamma = 10^(-3:-1), cost = 10^(2:7))
summary(wheat.tune.val) 
aav <- summary(wheat.tune.val)$performances
aav[order(aav[,3]),]
quartz(h=7, w=6, pointsize=12)
plot(wheat.tune.val, type="contour", transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(wheat.tune.val, type="perspective", transform.x=log10, transform.y=log10, theta=150)

svm.01.1000 <- svm(data=set1, type ~ ., kernel="radial", gamma=.01, cost=1000)
summary(svm.01.1000)


pred.train <- predict(svm.01.1000, newdata=set1, type="vector")
(misclass.train <- mean(ifelse(pred.train == set1$class, yes=0, no=1)))
table(pred.train, set1$class,  dnn=c("Predicted","Observed"))

pred.val <- predict(svm.01.1000, newdata=set2, type="vector")
(misclass.val <- mean(ifelse(pred.val == set2$class, yes=0, no=1)))
table(pred.val, set2$class,  dnn=c("Predicted","Observed"))

pred.test <- predict(svm.01.1000, newdata=set3, type="vector")
(misclass.test <- mean(ifelse(pred.test == set3$class, yes=0, no=1)))
table(pred.test, set3$class,  dnn=c("Predicted","Observed"))
