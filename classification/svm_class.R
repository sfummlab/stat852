# Classification by SVM
# Vehicle image data

vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 
vehdata$class <- as.factor(vehdata$class)
set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

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

svm.1.1 <- svm(data=set1, class ~ ., kernel="radial", gamma=1, cost=1, cross=10)
summary(svm.1.1)
head(svm.1.1$decision.values)
head(svm.1.1$fitted)
svm.1.1$tot.accuracy # Total CV Accuracy
svm.1.1$accuracies # Individual fold accuracies


pred1.1.1 <- predict(svm.1.1, newdata=set1)
table(pred1.1.1, set1$class,  dnn=c("Predicted","Observed"))

pred2.1.1 <- predict(svm.1.1, newdata=set2)
table(pred2.1.1, set2$class,  dnn=c("Predicted","Observed"))
(misclass2.1.1 <- mean(ifelse(pred2.1.1 == set2$class, yes=0, no=1)))

pred3.1.1 <- predict(svm.1.1, newdata=set3)
table(pred3.1.1, set3$class,  dnn=c("Predicted","Observed"))
(misclass3.1.1 <- mean(ifelse(pred3.1.1 == set3$class, yes=0, no=1)))

# This plot would work better if I supplied values at which to fix 
#   the other 16 variables, using the slice= argument.  Then 
#   Background fill would be coloured according to prediction.
#   Instead, it assumes the value "0" for all other variables,
#   And that evidently always leads to class 1.
# Support vectors are "x", colours are classes.  
quartz(h=7, w=6, pointsize=10)
plot(svm.1.1, set1, Compactness~Circularity )

####################################################################
###  EXTRA COOL AND EXCELLENT!  THIS SAME PACKAGE HAS A tune() 
###   FUNCTION THAT AUTOMATICALLY SELECTS A BEST SET OF TUNING
###   PARAMETERS!!!
### There is a method for svm, randomForest,nnet, rpart, and knn
###  Default is 10-fold CV, but can tune to a validation set
###  (At least in theory you can tune to a validation set, but it 
###  doesn't seem to work...)
####################################################################

veh.tune <-  tune.svm(data=set1, class ~ ., kernel="radial", gamma = 10^(-5:0), cost = 10^(-3:3))
summary(veh.tune)
aa <- summary(veh.tune)$performances
aa[order(aa[,3]),]
#### Note: Optimum is on edge of parameter space. Ought to pursue further (larger) costs
quartz(h=7, w=6, pointsize=12)
plot(veh.tune, type="contour", transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(veh.tune, type="perspective", transform.x=log10, transform.y=log10, theta=150)


# Show tuning on a validation set...or not (see summary)
veh.tune.val <-  tune.svm(data=set1, class ~ ., kernel="radial", 
                          validation.x=set2, gamma = 10^(-3:-1), cost = 10^(2:7))
summary(veh.tune.val) 
aav <- summary(veh.tune.val)$performances
aav[order(aav[,3]),]
quartz(h=7, w=6, pointsize=12)
plot(veh.tune.val, type="contour", transform.x=log10, transform.y=log10)
quartz(h=7, w=6, pointsize=12)
plot(veh.tune.val, type="perspective", transform.x=log10, transform.y=log10, theta=150)

svm.01.1000 <- svm(data=set1, class ~ ., kernel="radial", gamma=.01, cost=1000)
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
