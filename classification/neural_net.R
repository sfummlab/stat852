# Classification by Nerual Net
# Vehicle image data

vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 
vehdata$class <- factor(vehdata$class)
set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

library(nnet)

############
## Computing will work better if explanatories are rescaled to lie in [0,1]
############
rescale.set1 <- function(x1,x2){
  minx <- apply(X=x1, MARGIN=2, FUN=min)
  maxx <- apply(X=x1, MARGIN=2, FUN=max)
  x3 <- matrix (nrow=nrow(x2), ncol=ncol(x2))
  for(i in c(1:ncol(x2))){
    x3[,i] <- (x2[,i] - minx[i])/(maxx[i] - minx[i])
  }
  x3
}


x.1.unscaled <- as.matrix(set1[,-19])
x.1 <- rescale.set1(x.1.unscaled, x.1.unscaled)
x.2.unscaled <- as.matrix(set2[,-19])
x.2 <- rescale.set1(x.1.unscaled, x.2.unscaled)
x.3.unscaled <- as.matrix(set3[,-19])
x.3 <- rescale.set1(x.1.unscaled, x.3.unscaled)


########################################################################
## Responses for SOFTMAX classificaiton must be matrix of indicators 
## for each category.  These are created by class.ind().
########################################################################

y.1 <- class.ind(set1[,19])
y.2 <- class.ind(set2[,19])
y.3 <- class.ind(set3[,19])
head(y.1)

###########################################################################
# First using nnet{nnet}.  This function can do regression or classification.
#  Only allows one hidden layer.  
#  For classification, MUST SPECIFY "softmax=TRUE"), ONE hidden layer, 
#     generate initial weights randomly, NO SHRINKAGE ("weight decay") 
# Recommend shrinkage between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
###########################################################################

### Fit to set 1, validate on Set 2
nn.1.0 <- nnet(x=x.1, y=y.1, size=1, maxit=600, softmax=TRUE)
# Train error
p1.nn.1.0 <-predict(nn.1.0, newdata=x.1, type="class")
table(p1.nn.1.0, as.factor(set1$class),  dnn=c("Predicted","Observed"))
(misclass1.1.0 <- mean(ifelse(p1.nn.1.0 == as.factor(set1$class), yes=0, no=1)))
# Val set error
p2.nn.1.0 <-predict(nn.1.0, newdata=x.2, type="class")
table(p2.nn.1.0, as.factor(set2$class),  dnn=c("Predicted","Observed"))
(misclass2.1.0 <- mean(ifelse(p2.nn.1.0 == set2$class, yes=0, no=1)))
# Test set error
p3.nn.1.0 <-predict(nn.1.0, newdata=x.3, type="class")
table(p3.nn.1.0, as.factor(set3$class),  dnn=c("Predicted","Observed"))
(misclass3.1.0 <- mean(ifelse(p3.nn.1.0 == as.factor(set3$class), yes=0, no=1)))


#############################################
# Try tuning with caret::train
# "caret" stands for "classification and regression training"
# See http://topepo.github.io/caret/training.html for full details
# 
# Uses resampling (default is 25 bootstrap runs) to tune to a 
#   specified grid.
#
# "Kappa" is Cohen's Kappa, a version of misclassificaiton rate adjusted for chance.
#   Proportional improvement in misclassification relative to random assignment.
#############################################
library(caret)

tuned.nnet <- train(x=set1[,-19], y=set1[,19], method="nnet", preProcess="range", trace=FALSE, 
                    tuneGrid=expand.grid(.size=c(1,5,10,15),.decay=c(0,0.001,0.1,1)))
names(tuned.nnet)
tuned.nnet$results[order(tuned.nnet$results[,3]),]
tuned.nnet$bestTune

# I might continue by examining a few more size/decay combinations.  
# For now let's go with 5/.1, 10/.1, and 15/.001 as options.

### Estimate test error
nn.5.100 <- nnet(x=x.1, y=y.1, size=5, maxit=1000, decay=.1, softmax=TRUE)
p3.nn.5.100 <-predict(nn.5.100, newdata=x.3, type="class")
table(p3.nn.5.100, as.factor(set3$class),  dnn=c("Predicted","Observed"))
(misclass3.5.100 <- mean(ifelse(p3.nn.5.100 == as.factor(set3$class), yes=0, no=1)))

nn.10.100 <- nnet(x=x.1, y=y.1, size=10, maxit=1000, decay=.1, softmax=TRUE)
p3.nn.10.100 <-predict(nn.10.100, newdata=x.3, type="class")
table(p3.nn.10.100, as.factor(set3$class),  dnn=c("Predicted","Observed"))
(misclass3.10.100 <- mean(ifelse(p3.nn.10.100 == as.factor(set3$class), yes=0, no=1)))

nn.15.001 <- nnet(x=x.1, y=y.1, size=15, maxit=3000, decay=.001, softmax=TRUE)
p3.nn.15.001 <-predict(nn.15.001, newdata=x.3, type="class")
table(p3.nn.15.001, as.factor(set3$class),  dnn=c("Predicted","Observed"))
(misclass3.15.001 <- mean(ifelse(p3.nn.15.001 == as.factor(set3$class), yes=0, no=1)))
