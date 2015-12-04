vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

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

set1.rescale <- data.frame(cbind(rescale(set1[,-19], set1[,-19]), class=set1$class))
set2.rescale <- data.frame(cbind(rescale(set2[,-19], set1[,-19]), class=set2$class))
set3.rescale <- data.frame(cbind(rescale(set3[,-19], set1[,-19]), class=set3$class))


library(nnet)

##################################################################
# multinom() uses first group as the baseline class
# Default number of iterations is only 100. Often needs to be increased!
#   Make sure it runs to convergence.  I had to rerun this several times, increasing the maxit=
mod.fit <- multinom(data=set1.rescale, formula=class ~ ., trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula=class ~ ., maxit=1000, trace=TRUE)
mod.fit <- multinom(data=set1.rescale, formula=class ~ ., maxit=5000, trace=TRUE)
mod.fit
summary(mod.fit)

# Adding a function to perform LR Tests.  Legitimate here since I am fitting one model
library(car)
Anova(mod.fit)

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale)
pred.prob.1 <- round(predict(mod.fit, newdata=set1.rescale, type="probs"), digits=3)

pred.class.2 <- predict(mod.fit, newdata=set2.rescale)
pred.prob.2 <- predict(mod.fit, newdata=set2.rescale, type="probs")

pred.class.3 <- predict(mod.fit, newdata=set3.rescale)
pred.prob.3 <- predict(mod.fit, newdata=set3.rescale, type="probs")
cbind(head(round(pred.prob.3, 3)), head(pred.class.3))



(mul.misclass.train <- mean(ifelse(pred.class.1 == set1.rescale$class, yes=0, no=1)))
(mul.misclass.valid <- mean(ifelse(pred.class.2 == set2.rescale$class, yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.3 == set3.rescale$class, yes=0, no=1)))

# Test set confusion matrix
table(set3$class, pred.class.3, dnn=c("Obs","Pred"))

mod.fit$edf

# Example Plot results: Plot of two most significant variables, colour coded by class 
quartz(h=7,w=6,pointsize=12)
class.col <- ifelse(set1$class==1,y=53,n=
  ifelse(set1$class==2,y=68,n=
  ifelse(set1$class==3,y=203,n=464)))
plot(x=set1.rescale[,4], y=set1.rescale[,5], col=colors()[class.col])

####################
# Let's try rerunning with just the top 6 principal components
# Estimate the PCs from the training data.  
pc <-  prcomp(x=set1[,-19], scale.=TRUE)

# Create the same transformations in all three data sets 
#   and attach the response variable at the end
#   predict() does this 
xi.1 <- data.frame(pc$x,class = set1$class)
xi.2 <- data.frame(predict(pc, newdata=set2), class = set2$class)
xi.3 <- data.frame(predict(pc, newdata=set3), class = set3$class)

# Refit the model.  First 6 PCs are in columns 1-6.  
mod.fit.pc <- multinom(data=xi.1[,c(1:6,19)], formula=class ~ ., maxit=3000, trace=TRUE)
mod.fit.pc
summary(mod.fit.pc)
#NOTE: AIC=778.  Old AIC was  357!

# Perform LR Tests. 
Anova(mod.fit.pc)

pred.class.pc1 <- predict(mod.fit.pc, newdata=xi.1)
pred.prob.pc1 <- round(predict(mod.fit.pc, newdata=xi.1, type="probs"), digits=3)

pred.class.pc2 <- predict(mod.fit.pc, newdata=xi.2)
pred.prob.pc2 <- predict(mod.fit.pc, newdata=xi.2, type="probs")

pred.class.pc3 <- predict(mod.fit.pc, newdata=xi.3)
pred.prob.pc3 <- predict(mod.fit.pc, newdata=xi.3, type="probs")

(pc.misclass.train <- mean(ifelse(pred.class.pc1 == set1.rescale$class, yes=0, no=1)))
(pc.misclass.valid <- mean(ifelse(pred.class.pc2 == set2.rescale$class, yes=0, no=1)))
(pc.misclass.test <- mean(ifelse(pred.class.pc3 == set3.rescale$class, yes=0, no=1)))

# Test set confusion matrix
table(set3$class, pred.class.pc3, dnn=c("Obs","Pred"))

mod.fit.pc$edf

# Example Plot results: Plot of two most significant variables, colour coded by class 
quartz(h=7,w=18,pointsize=12)
par(mfrow=c(1,3))
class.col <- ifelse(set1$class==1,y=53,n=
  ifelse(set1$class==2,y=68,n=
  ifelse(set1$class==3,y=203,n=464)))
plot(x=xi.1[,1], y=xi.1[,2], col=colors()[class.col])
plot(x=xi.1[,3], y=xi.1[,4], col=colors()[class.col])
plot(x=xi.1[,5], y=xi.1[,6], col=colors()[class.col])

# Fitting all 18 PCs to see whether "significant" information was lost.
#  Clearly, YES!
# (Notice how much easier the fit is without all the correlation)
mod.fit.pc18 <- multinom(data=xi.1, formula=class ~ ., maxit=3000, trace=TRUE)
summary(mod.fit.pc18)
Anova(mod.fit.pc18)

####################################################
# Multinomial Logistic Regression using glmnet()
# Setting lambda penalty parameter to 0 in predict() is full ML fit
#  Could do LASSO here as well
###############################################################
library(glmnet)
logit.fit <- glmnet(x=as.matrix(set1[,1:18]), y=set1[,19], family="multinomial")
quartz(h=15,w=15)
par(mfrow=c(3,2))
plot(logit.fit)

logit.fit.class <- predict(object=logit.fit, newx=as.matrix(set1[,1:18]), s=0, type="class")
logit.fit.prob <- predict(logit.fit, newx=as.matrix(set1[,1:18]), s=0, type="response")

# Calculate in-sample and out-of-sample misclassification error
las0.pred.train <- predict(object=logit.fit, newx=as.matrix(set1[,1:18]), s=0, type="class")
las0.pred.valid <- predict(logit.fit, newx=as.matrix(set2[,1:18]), s=0, type="class")
las0.pred.test <- predict(logit.fit, newx=as.matrix(set3[,1:18]), s=0, type="class")
(las0misclass.train <- mean(ifelse(las0.pred.train == set1$class, yes=0, no=1)))
(las0misclass.valid <- mean(ifelse(las0.pred.valid == set2$class, yes=0, no=1)))
(las0misclass.test <- mean(ifelse(las0.pred.test == set3$class, yes=0, no=1)))


# "Optimal" LASSO Fit
logit.cv <- cv.glmnet(x=as.matrix(set1[,1:18]), y=set1[,19], family="multinomial")
logit.cv
plot(logit.cv)

## Find nonzero lasso coefficients
c <- coef(logit.fit,s=logit.cv$lambda.min) 
cmat <- cbind(as.matrix(c[[1]]), as.matrix(c[[2]]), as.matrix(c[[3]]), 
              as.matrix(c[[4]]))
round(cmat,2)
cmat!=0

lascv.pred.train <- predict(object=logit.cv, newx=as.matrix(set1[,1:18]), s=logit.cv$lambda.min, type="class")
lascv.pred.valid <- predict(logit.cv, newx=as.matrix(set2[,1:18]), s=logit.cv$lambda.min, type="class")
lascv.pred.test <- predict(logit.cv, newx=as.matrix(set3[,1:18]), s=logit.cv$lambda.min, type="class")
(lascvmisclass.train <- mean(ifelse(lascv.pred.train == set1$class, yes=0, no=1)))
(lascvmisclass.valid <- mean(ifelse(lascv.pred.valid == set2$class, yes=0, no=1)))
(lascvmisclass.test <- mean(ifelse(lascv.pred.test == set3$class, yes=0, no=1)))





