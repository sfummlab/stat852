wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")


wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"


#pair.1 <- ifelse(set1$type=="Healthy", y=1, n=0)
#pair.2 <- ifelse(set2$type=="Healthy", y=1, n=0)

wheat$bina <- ifelse(wheat$type=="Healthy", y=1, n=0)



library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]




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




##################################################################
# multinom() uses first group as the baseline class
# Default number of iterations is only 100. Often needs to be increased!
#   Make sure it runs to convergence.  I had to rerun this several times, increasing the maxit=



library(nnet)


mod.fit <- multinom(data=set1.rescale, formula= bina ~ density, trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula= bina ~ density, maxit=1000, trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula= bina ~ density, maxit=5000, trace=TRUE)
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

quartz(h=12,w=12,pointsize=12)
par(mfrow=c(2,1))
plot(set1$density, pred.prob.1)

plot(set1$density, pred.class.1)

(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$bina, yes=0, no=1)))

(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$bina, yes=0, no=1)))





library(sm)


quartz(h=12,w=12,pointsize=12)
par(mfrow=c(4,2))
sm.binomial(x=set1[,2], y=set1$bina, h=0.001, xlab="h=0.001")
sm.binomial(x=set1[,2], y=set1$bina, h=0.01, xlab="h=0.01")
sm.binomial(x=set1[,2], y=set1$bina, h=0.1, xlab="h=0.1")
sm.binomial(x=set1[,2], y=set1$bina, h=1, xlab="h=1")
sm.binomial(x=set1[,2], y=set1$bina, h=2, xlab="h=2")
sm.binomial(x=set1[,2], y=set1$bina, h=6, xlab="h=6")
sm.binomial(x=set1[,2], y=set1$bina, h=8, xlab="h=8")
sm.binomial(x=set1[,2], y=set1$bina, h=1000, xlab="h=1000")





sm.train<- sm.binomial(x=set1[,2], y=set1$bina, h=2,eval.points = set1[,2])
sm.test<- sm.binomial(x=set1[,2], y=set1$bina, h=2,eval.points = set2[,2])

quartz(h=12,w=12,pointsize=12)
par(mfrow=c(2,1))
plot(sm.train$eval.points, sm.train$estimate)

plot(sm.train$linear.predictor, sm.train$estimate)
 

quartz(h=12,w=12,pointsize=12)
plot(sm.train$eval.points, sm.train$linear.predictor)

sm.pred.1 <- ifelse(round(sm.train$estimate, digits=3) > 0.5, yes = 1, no = 0)

sm.pred.2 <- ifelse(round(sm.test$estimate, digits=3)  > 0.5, yes = 1, no = 0)

(sm.misclass.train <- mean(ifelse(sm.pred.1 == set1$bina, yes=0, no=1)))

(sm.misclass.test <- mean(ifelse(sm.pred.2 == set2$bina, yes=0, no=1)))



#   GAM 

library(mgcv)

#  Generalized additive model as alternative to multivariate splines
# One variable
#s(set1$density)+s(set1$weight)+s(set1$hardness)+s(set1$size)+
gam1 <- gam(bina ~ s(density)+s(weight)+s(hardness)+s(size)+s(moisture) + classnum, family=binomial(link=logit),data=set1) 
#gam1 <- gam(pair.1 ~ s(set1$density) , family=binomial(link=logit)) 
summary(gam1)
# Plots of results
quartz(h=12,w=12,pointsize=12)
par(mfrow=c(2,3))
plot(gam1, main="Generalized Additive Model marginal splines")



gam.prob.1 <- predict(gam1, newdata=set1, type="response")
gam.class.1 <- as.numeric(predict(gam1, newdata=set1, type="link") > 0)


gam.prob.2 <- predict(gam1, newdata=set2, type="response")
gam.class.2 <- as.numeric(predict(gam1, newdata=set2, type="link") > 0)


(gam.misclass.train <- mean(ifelse(gam.class.1== set1$bina, yes=0, no=1)))

(gam.misclass.test <- mean(ifelse(gam.class.2 == set2$bina, yes=0, no=1)))


train.mse <- cbind(sm=sm.misclass.train,logistic=mul.misclass.train, gam=gam.misclass.train)
test.mse <- cbind(sm=sm.misclass.test, logistic=mul.misclass.test, gam=gam.misclass.test)

###############################################################
#### The density() function does kernel density estimation. 
####   Bandwidth, bw=, is the SD of the kernel
#### Also, the package sm has a function that can be used to 
####   quickly compare densities across classes. 
###############################################################


classnum1 <- density(set1[which(set1$type=="Healthy"),1], kernel="gaussian")
classnum2 <- density(set1[which(set1$type=="Sprout"),1], kernel="gaussian")
classnum3 <- density(set1[which(set1$type=="Scab"),1], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( classnum1, main="Gaussian Kernel - Classnum", col=colors()[53], ylim=c(0,1.5), lwd=2)
lines(classnum2, col=colors()[68], lwd=2)
lines(classnum3, col=colors()[203], lwd=2)
legend(x = 1.5, y = 1.5, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")


density1 <- density(set1[which(set1$type=="Healthy"),2], kernel="gaussian")
density2 <- density(set1[which(set1$type=="Sprout"),2], kernel="gaussian")
density3 <- density(set1[which(set1$type=="Scab"),2], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( density1, main="Gaussian Kernel-Density", col=colors()[53], xlim = c(0,2), ylim=c(0,8.00), lwd=2)
lines(density2, col=colors()[68], lwd=2)
lines(density3, col=colors()[203], lwd=2)
legend(x = 1.5, y = 8, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")



hard1 <- density(set1[which(set1$type=="Healthy"),3], kernel="gaussian")
hard2 <- density(set1[which(set1$type=="Sprout"),3], kernel="gaussian")
hard3 <- density(set1[which(set1$type=="Scab"),3], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( hard1, main="Gaussian Kernel-hardness", col=colors()[53], ylim=c(0,0.03), lwd=2)
lines(hard2, col=colors()[68], lwd=2)
lines(hard3, col=colors()[203], lwd=2)
legend(x = 1.5, y = 0.02, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")

size1 <- density(set1[which(set1$type=="Healthy"),4], kernel="gaussian")
size2 <- density(set1[which(set1$type=="Sprout"),4], kernel="gaussian")
size3 <- density(set1[which(set1$type=="Scab"),4], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( size1, main="Gaussian Kernel-Size", col=colors()[53], ylim=c(0,1.5), lwd=2)
lines(size2, col=colors()[68], lwd=2)
lines(size3, col=colors()[203], lwd=2)
#lines(kdc4.4, col=colors()[464], lwd=2)
legend(x = 1.5, y = 1.2, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")

weight1 <- density(set1[which(set1$type=="Healthy"),5], kernel="gaussian")
weight2 <- density(set1[which(set1$type=="Sprout"),5], kernel="gaussian")
weight3 <- density(set1[which(set1$type=="Scab"),5], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( weight1, main="Gaussian Kernel-weight", col=colors()[53], ylim=c(0,0.1), lwd=2)
lines(weight2, col=colors()[68], lwd=2)
lines(weight3, col=colors()[203], lwd=2)
#lines(kdc4.4, col=colors()[464], lwd=2)
legend(x = 15, y = 0.08, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")

moist1 <- density(set1[which(set1$type=="Healthy"),6], kernel="gaussian")
moist2 <- density(set1[which(set1$type=="Sprout"),6], kernel="gaussian")
moist3 <- density(set1[which(set1$type=="Scab"),6], kernel="gaussian")

quartz(h=7,w=6,pointsize=12)
plot( moist1, main="Gaussian Kernel-moisture", col=colors()[53], ylim=c(0,0.6), lwd=2)
lines(moist2, col=colors()[68], lwd=2)
lines(moist3, col=colors()[203], lwd=2)
#lines(kdc4.4, col=colors()[464], lwd=2)
legend(x = 8, y = 0.5, legend = c("Healthy", "Sprout", "Scab"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")




###########################################################
### Density comparison across groups: automatic!
###########################################################
library(sm)
quartz(h=7,w=6,pointsize=12)
sm.density.compare(x=set1[,1], group=set1$type, lwd=2)






library(e1071)
###############################################################
## Naive Bayes is done in e1071::naiveBayes(). Unfortunately, 
##  the default is simply to assume a Normal density in each margin
##    (i.e. assume multivariate normality with no correlations).
##  This makes it a cheap version of LDA.
###############################################################

set1$type = as.factor(set1$type)
set2$type = as.factor(set2$type)
nb.0 <- naiveBayes(x=set1[,1:6], y=set1[,7])


# Calculate in-sample and out-of-sample misclassification error
nb.pred.train <- predict(nb.0, newdata=set1[,-7], type="class")
table(predict(nb.0, set1[,1:6]), set1[,7])

nb.pred.test <- predict(nb.0, newdata=set2[,-7], type="class")
table(predict(nb.0, set2[,1:6]), set2[,7], dnn=c("Predicted","Observed"))


(nbmisclass.train <- mean(ifelse(nb.pred.train == set1$type, yes=0, no=1)))

(nbmisclass.test <- mean(ifelse(nb.pred.test == set2$type, yes=0, no=1)))

######################################################
## klaR::NaiveBayes is an experimental function that uses the 
##   e1071::naiveBayes() function, but does Gaussian Kernel Smothing
## ********predict() Gives error messages, but seems to work
######################################################

library(klaR)
NB <- NaiveBayes(x=set1[,1:6], grouping=set1[,7], usekernel=TRUE)

#quartz(h=7,w=6)
#plot(NB, lwd=2)

NB.pred.train <- predict(NB, newdata=set1[,1:6], type="class")
table(NB.pred.train$class, set1[,7], dnn=c("Predicted","Observed"))



NB.pred.test <- predict(NB, newdata=set2[,1:6], type="class")
table(NB.pred.test$class, set2[,7], dnn=c("Predicted","Observed"))
warnings()
round(NB.pred.test$posterior)

# Error rates
(NBmisclass.train <- mean(ifelse(NB.pred.train$class == set1$type, yes=0, no=1)))

(NBmisclass.test <- mean(ifelse(NB.pred.test$class == set2$type, yes=0, no=1)))

####################################################################
#   See Liwei Fan, Kim Leng Poh, 2007, A Comparative Study of PCA, ICA 
#   and Class-Conditional ICA for Na?ve Bayes Classifier.

pc <-  prcomp(x=set1[,1:6], scale.=TRUE)

# Create the same transformations in all three data sets 
#   and attach the response variable at the end
#   predict() does this 
xi.1 <- data.frame(pc$x,type = as.factor(set1$type))
xi.2 <- data.frame(predict(pc, newdata=set2), type = as.factor(set2$type))
#xi.3 <- data.frame(predict(pc, newdata=set3), class = as.factor(set3$class))

NB.pc <- NaiveBayes(x=xi.1[,1:6], grouping=xi.1[,7], usekernel=TRUE)




NBpc.pred.train <- predict(NB.pc, newdata=xi.1[,-7], type="class")
table(NBpc.pred.train$class, xi.1[,7], dnn=c("Predicted","Observed"))


NBpc.pred.test <- predict(NB.pc, newdata=xi.2[,-7], type="class")
table(NBpc.pred.test$class, xi.2[,7], dnn=c("Predicted","Observed"))
warnings()
round(NBpc.pred.test$posterior)

# Error rates
(NBPCmisclass.train <- mean(ifelse(NBpc.pred.train$class == xi.1$type, yes=0, no=1)))

(NBPCmisclass.test <- mean(ifelse(NBpc.pred.test$class == xi.2$type, yes=0, no=1)))

# It definitely helps!

quartz(h=12,w=12)
par(mfrow=c(2,3))
plot(NB.pc, main="Non-Parametric Naive Bayes PCA marginal Distribution")


quartz(h=12,w=12)
par(mfrow=c(2,3))
plot(NB, main="Non-Parametric Naive Bayes marginal Distribution")

quartz(h=12,w=12)
par(mfrow=c(2,3))
plot(nb.0, main="Parametric Naive Bayes marginal Distribution")




