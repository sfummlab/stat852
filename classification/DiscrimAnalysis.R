 vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

library(MASS)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

###############################################################
## Linear discriminant analysis via MASS::lda()
## Does dimension-reduction version
##  "prior=" allows you to set prior probabilities if they are known.  
#  Default is the sample proportions.
###############################################################

lda.fit <- lda(x=set1[,-19], grouping=set1$class)
lda.fit


class.col <- ifelse(set1$class==1,y=53,n=
                      ifelse(set1$class==2,y=68,n=
                               ifelse(set1$class==3,y=203,n=464)))
# Plot empirical densities for discriminant functions 
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="histogram", main="Values along canonical variate")
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="density", main="Values along ")

# Note that in above fit, the "Linear Discriminants" are directions 
#   of the 3-dim subspace within which the 4 centroids (p-dim means) lie.
# The proportion of trace is the amount of variability explained in each direction.

# Plot results 
quartz(h=7,w=6,pointsize=12)
class.col <- ifelse(set1$class==1,y=53,n=
  ifelse(set1$class==2,y=68,n=
  ifelse(set1$class==3,y=203,n=464)))
plot(lda.fit, col=colors()[class.col])

# Plot in 3d
pred <- predict(lda.fit, newdata=set1[,-19])

pred.col <- ifelse(pred$class==1,y=53,n=
  ifelse(pred$class==2,y=68,n=
  ifelse(pred$class==3,y=203,n=464)))

library(rgl)  
  open3d()
  plot3d(x=pred$x[,1], y=pred$x[,2], z=pred$x[,3],col=colors()[c(pred.col)])

# The "Confusion Matrix" in the training set
table(set1$class, pred$class, dnn=c("Obs","Pred"))

# Calculate in-sample and out-of-sample misclassification error
lda.pred.train <- pred$class
lda.pred.valid <- predict(lda.fit, newdata=set2[,-19])$class
lda.pred.test <- predict(lda.fit, newdata=set3[,-19])$class
(lmisclass.train <- mean(ifelse(lda.pred.train == set1$class, yes=0, no=1)))
(lmisclass.valid <- mean(ifelse(lda.pred.valid == set2$class, yes=0, no=1)))
(lmisclass.test <- mean(ifelse(lda.pred.test == set3$class, yes=0, no=1)))

# Test set confusion matrix
table(set3$class, lda.pred.test, dnn=c("Obs","Pred"))


##################################################################
## Quadratic fit
##   Fewer options available (no plot function, no canonical variates
##  to plot)
##################################################################

qda.fit <- qda(x=set1[,-19], grouping=set1$class)
qda.fit

qda.pred.train <- predict(qda.fit, newdata=set1[,-19])$class
qda.pred.valid <- predict(qda.fit, newdata=set2[,-19])$class
qda.pred.test <- predict(qda.fit, newdata=set3[,-19])$class
(qmisclass.train <- mean(ifelse(qda.pred.train == set1$class, yes=0, no=1)))
(qmisclass.valid <- mean(ifelse(qda.pred.valid == set2$class, yes=0, no=1)))
(qmisclass.test <- mean(ifelse(qda.pred.test == set3$class, yes=0, no=1)))

# Test set confusion matrix
table(set3$class, qda.pred.test, dnn=c("Obs","Pred"))

