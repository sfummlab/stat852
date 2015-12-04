
wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")


wheat$class <- as.numeric(wheat$class)

library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]


quartz(h=7,w=12)
pairs(wheat[,-1])



wheat.pc <-  prcomp(x=set1[,-7], scale.=TRUE)
summary(wheat.pc)


quartz(h=7,w=12)
par(mfrow=c(1,2))
plot(wheat.pc)

evals <- wheat.pc$sdev^2
plot(y=evals, x=c(1:6))
abline(a=0,b=0)
abline(a=0.5,b=0)

# Look at eigenvectors to see how variables contribute to PC's
wheat.pc$rotation



lda.fit <- lda(x=set1[,-7], grouping=set1$type)
lda.fit


pred <- predict(lda.fit, newdata=set1[,-7])

pred.col <- ifelse(pred$class=="Healthy",y=53,n=
                     ifelse(pred$class=="Sprout",y=68,n=
                              ifelse(pred$class=="Scab",y=203,n=464)))


quartz(h=7,w=6,pointsize=12)
plot(lda.fit, col=colors()[pred.col])

# Plot empirical densities for discriminant functions 
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="histogram", main="Values along canonical variate")
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="density", main="Values along ")


# The "Confusion Matrix" in the training set
table(set1$type, pred$class, dnn=c("Obs","Pred"))

# Calculate in-sample and out-of-sample misclassification error
lda.pred.train <- pred$class
lda.pred.test <- predict(lda.fit, newdata=set2[,-7])$class
(lmisclass.train <- mean(ifelse(lda.pred.train == set1$type, yes=0, no=1)))

(lmisclass.test <- mean(ifelse(lda.pred.test == set2$type, yes=0, no=1)))

# Test set confusion matrix
table(set2$type, lda.pred.test, dnn=c("Obs","Pred"))


##################################################################
## Quadratic fit
##   Fewer options available (no plot function, no canonical variates
##  to plot)
##################################################################

qda.fit <- qda(x=set1[,-7], grouping=set1$type)
qda.fit

qda.pred.train <- predict(qda.fit, newdata=set1[,-7])$class

qda.pred.test <- predict(qda.fit, newdata=set2[,-7])$class

(qmisclass.train <- mean(ifelse(qda.pred.train == set1$type, yes=0, no=1)))


(qmisclass.test <- mean(ifelse(qda.pred.test == set2$type, yes=0, no=1)))

# Test set confusion matrix
table(set2$type, qda.pred.test, dnn=c("Obs","Pred"))



###############################################################
## Multinomial Logistic Regression using multinom(nnet)
#  Manual says to rescale data between 0-1 first
###############################################################


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


set1.rescale <- data.frame(cbind(rescale(set1[,-7], set1[,-7]), class=set1$type))
set2.rescale <- data.frame(cbind(rescale(set2[,-7], set1[,-7]), class=set2$type))

library(nnet)

##################################################################
# multinom() uses first group as the baseline class
# Default number of iterations is only 100. Often needs to be increased!
#   Make sure it runs to convergence.  I had to rerun this several times, increasing the maxit=
mod.fit <- multinom(data=set1.rescale, formula=class.1 ~ ., trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula=class.1 ~ ., maxit=1000, trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula=class.1 ~ ., maxit=5000, trace=TRUE)
mod.fit
summary(mod.fit)







# Likelihood Ratio Test

library(car)
Anova(mod.fit)

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale)
pred.prob.1 <- round(predict(mod.fit, newdata=set1.rescale, type="probs"), digits=3)

pred.class.2 <- predict(mod.fit, newdata=set2.rescale)
pred.prob.2 <- predict(mod.fit, newdata=set2.rescale, type="probs")

cbind(head(round(pred.prob.2, 3)), head(pred.class.2))



(mul.misclass.train <- mean(ifelse(pred.class.1 == set1.rescale$class.1, yes=0, no=1)))

(mul.misclass.test <- mean(ifelse(pred.class.2 == set2.rescale$class.1, yes=0, no=1)))

# Test set confusion matrix
table(set2$type, pred.class.2, dnn=c("Obs","Pred"))

mod.fit$edf






quartz(h=7,w=6,pointsize=12)
class.col <- ifelse(set1$type=="Healthy",y=53,n=
                      ifelse(set1$type=="Sprout",y=68,n=
                               ifelse(set1$type=="Scab",y=203,n=464)))
plot(x=set1.rescale[,2], y=set1.rescale[,5], col=colors()[class.col])




