phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")

lda.fit <- lda(x=phoneme[,2:257], grouping=phoneme$g)

lda.fit


pred <- predict(lda.fit, newdata=phoneme[,2:257])

class.col <- ifelse(phoneme$g=="sh",y=393,n=
                      ifelse(phoneme$g=="iy",y=68,n=
                               ifelse(phoneme$g=="dcl",y=203, n=
                                        ifelse(phoneme$g=="aa", y = 385,n=464))))





quartz(h=7,w=6,pointsize=12)
plot(lda.fit, col=colors()[class.col])

# Plot empirical densities for discriminant functions 
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="histogram", main="Values along canonical variate")
quartz(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="density", main="Values along ")

table(phoneme$g, pred$class, dnn=c("Obs","Pred"))





lda.miss.train <- rep(0,5)
qda.miss.train <- rep(0,5)
multinom.miss.train <- rep(0,5)
lasso.miss.train <- rep(0,5)
naive1.miss.train <- rep(0,5)
naive2.miss.train <- rep(0,5)
naive3.miss.train <- rep(0,5)


lda.miss.test <- rep(0,5)
qda.miss.test <- rep(0,5)
multinom.miss.test <- rep(0,5)
lasso.miss.test <- rep(0,5)
naive1.miss.test <- rep(0,5)
naive2.miss.test <- rep(0,5)
naive3.miss.test <- rep(0,5)



lda.miss.test.re <- rep(0,5)
qda.miss.test.re <- rep(0,5)
multinom.miss.test.re <- rep(0,5)
lasso.miss.test.re <- rep(0,5)
naive1.miss.test.re <- rep(0,5)
naive2.miss.test.re <- rep(0,5)
naive3.miss.test.re <- rep(0,5)

set.seed(67982193)

for(i in 1:5)
{
  resamp <- sample.int(n=nrow(phoneme), size=0.7 * nrow(phoneme), replace=FALSE)
  x.r  <- phoneme[resamp,2:257]
  y.r  <- phoneme[resamp,258]
  
  x.p  <- phoneme[-unique(resamp),2:257]
  y.p  <- phoneme[-unique(resamp),258]
  
  library(MASS)
  
  lda.fit <- lda(x=x.r, grouping=y.r)
  
  # Calculate in-sample and out-of-sample misclassification error
  lda.pred.train <- predict(lda.fit, newdata=x.r)$class
  lda.pred.test <- predict(lda.fit, newdata=x.p)$class
  
  
  (lmisclass.train <- mean(ifelse(lda.pred.train == y.r, yes=0, no=1)))
  
  (lmisclass.test <- mean(ifelse(lda.pred.test == y.p, yes=0, no=1)))
  
  lda.miss.train[i] <- lmisclass.train
  lda.miss.test[i] <- lmisclass.test
  
  
  ##################################################################
  ## Quadratic fit
  ##   Fewer options available (no plot function, no canonical variates
  ##  to plot)
  ##################################################################
  
  qda.fit <- qda(x=x.r, grouping=y.r)
  
  qda.pred.train <- predict(qda.fit, newdata=x.r)$class
  
  qda.pred.test <- predict(qda.fit, newdata=x.p)$class
  
  (qmisclass.train <- mean(ifelse(qda.pred.train == y.r, yes=0, no=1)))
  
  
  (qmisclass.test <- mean(ifelse(qda.pred.test == y.p, yes=0, no=1)))
  
  
  qda.miss.train[i] <- qmisclass.train
  qda.miss.test[i] <- qmisclass.test
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
  
  
  set1.rescale <- data.frame(cbind(rescale(x.r,x.r), class=y.r))
  set2.rescale <- data.frame(cbind(rescale(x.p,x.r), class=y.p))
  
  library(glmnet)
  # "Optimal" LASSO Fit
  mod.fit <- glmnet(x=as.matrix(x.r), y=y.r, nlambda= 20,  family="multinomial")
  
  # Misclassification Errors
  pred.class.1 <- predict(mod.fit, newx=as.matrix(x.r),type="class", s=0)
  pred.prob.1 <- predict(mod.fit, newx=as.matrix(x.r), type="response",s=0)
  
  pred.class.2 <- predict(mod.fit, newx=as.matrix(x.p),s=0, type="class")
  pred.prob.2 <- predict(mod.fit, newx=as.matrix(x.p), type="response",s=0)
  
  (mul.misclass.train <- mean(ifelse(pred.class.1 ==y.r, yes=0, no=1)))
  
  (mul.misclass.test <- mean(ifelse(pred.class.2 == y.p, yes=0, no=1)))
  
  multinom.miss.train[i] <- mul.misclass.train
  multinom.miss.test[i] <- mul.misclass.test
  ####################################################
  # Multinomial Logistic Regression using glmnet()
  # Setting lambda penalty parameter to 0 in predict() is full ML fit
  #  Could do LASSO here as well
  ###############################################################
  library(glmnet)
  # "Optimal" LASSO Fit
  logit.cv <- cv.glmnet(x=as.matrix(x.r), y=y.r, nfold = 5, nlambda = 20, type.measure = "class",  family="multinomial", parallel = TRUE)
  logit.cv
  plot(logit.cv)
  
  ## Find nonzero lasso coefficients
  c <- coef(logit.cv,s=logit.cv$lambda.min) 
  cmat <- cbind(as.matrix(c[[1]]), as.matrix(c[[2]]), as.matrix(c[[3]]), 
                as.matrix(c[[4]]))
  round(cmat,2)
  cmat!=0
  
  lascv.pred.train <- predict(object=logit.cv, newx=as.matrix(x.r), s=logit.cv$lambda.min, type="class")
  lascv.pred.test <- predict(logit.cv, newx=as.matrix(x.p), s=logit.cv$lambda.min, type="class")
  (lascvmisclass.train <- mean(ifelse(lascv.pred.train == y.r, yes=0, no=1)))
  (lascvmisclass.test <- mean(ifelse(lascv.pred.test == y.p, yes=0, no=1)))
  lasso.miss.train[i] <- lascvmisclass.train
  lasso.miss.test[i] <- lascvmisclass.test
  
  
  
  
  library(e1071)
  ###############################################################
  ## Naive Bayes is done in e1071::naiveBayes(). Unfortunately, 
  ##  the default is simply to assume a Normal density in each margin
  ##    (i.e. assume multivariate normality with no correlations).
  ##  This makes it a cheap version of LDA.
  ###############################################################
  
  y.r <- as.factor(y.r)
  nb.0 <- naiveBayes(x=x.r, y=y.r)
  
  
  # Calculate in-sample and out-of-sample misclassification error
  nb.pred.train <- predict(nb.0, newdata=x.r, type="class")
  table(predict(nb.0, x.r), y.r)
  
  nb.pred.test <- predict(nb.0, newdata=x.p, type="class")
  table(predict(nb.0, x.p), y.p, dnn=c("Predicted","Observed"))
  
  
  (nbmisclass.train <- mean(ifelse(nb.pred.train == y.r, yes=0, no=1)))
  
  (nbmisclass.test <- mean(ifelse(nb.pred.test == y.p, yes=0, no=1)))
   naive1.miss.test[i] <- nbmisclass.test
   naive1.miss.train[i] <- nbmisclass.train

 
  ######################################################
  ## klaR::NaiveBayes is an experimental function that uses the 
  ##   e1071::naiveBayes() function, but does Gaussian Kernel Smothing
  ## ********predict() Gives error messages, but seems to work
  ######################################################
  
  library(klaR)
  NB <- NaiveBayes(x=x.r, grouping=y.r, usekernel=TRUE)
  
 # quartz(h=7,w=6)
#  plot(NB, lwd=2)
  
  NB.pred.train <- predict(NB, newdata=x.r, type="class")
  #table(NB.pred.train$class, y.r, dnn=c("Predicted","Observed"))
  
  
  
  NB.pred.test <- predict(NB, newdata=x.p, type="class")
  #table(NB.pred.test$class, y.p, dnn=c("Predicted","Observed"))
  warnings()
  round(NB.pred.test$posterior)
  
  # Error rates
  (NBmisclass.train <- mean(ifelse(NB.pred.train$class == y.r, yes=0, no=1)))
  
  (NBmisclass.test <- mean(ifelse(NB.pred.test$class == y.p, yes=0, no=1)))
  
   naive2.miss.test[i] <- NBmisclass.test
   naive2.miss.train[i] <- NBmisclass.train
  ####################################################################
  #   See Liwei Fan, Kim Leng Poh, 2007, A Comparative Study of PCA, ICA 
  #   and Class-Conditional ICA for Na?ve Bayes Classifier.
  
  pc <-  prcomp(x=x.r, scale.=TRUE)
  
  # Create the same transformations in all three data sets 
  #   and attach the response variable at the end
  #   predict() does this 
  xi.1 <- data.frame(pc$x,type = as.factor(y.r))
  xi.2 <- data.frame(predict(pc, newdata=x.p), type = as.factor(y.p))
  #xi.3 <- data.frame(predict(pc, newdata=set3), class = as.factor(set3$class))
  
  NB.pc <- NaiveBayes(x=xi.1[,-257], grouping=xi.1[,257], usekernel=TRUE)
  
  
  NBpc.pred.train <- predict(NB.pc, newdata=xi.1[,-257], type="class")
  #table(NBpc.pred.train$class, xi.1[,257], dnn=c("Predicted","Observed"))
  
  
  NBpc.pred.test <- predict(NB.pc, newdata=xi.2[,-257], type="class")
  #table(NBpc.pred.test$class, xi.2[,257], dnn=c("Predicted","Observed"))
  warnings()
  round(NBpc.pred.test$posterior)
  
  # Error rates
  (NBPCmisclass.train <- mean(ifelse(NBpc.pred.train$class == xi.1$type, yes=0, no=1)))
  
  (NBPCmisclass.test <- mean(ifelse(NBpc.pred.test$class == xi.2$type, yes=0, no=1)))
  
  # It definitely helps!
  
   naive3.miss.test[i] <-  NBPCmisclass.test
   naive3.miss.train[i] <- NBPCmisclass.train


  
  
  mspe <- cbind(lascvmisclass.test, mul.misclass.test, qmisclass.test, lmisclass.test, NBmisclass.test, NBPCmisclass.test, nbmisclass.test)
  
  mspe.min <- min(mspe)
  
  
  lda.miss.test.re[i] <- lda.miss.test[i] / mspe.min
  qda.miss.test.re[i] <- qda.miss.test[i] / mspe.min
  multinom.miss.test.re[i] <-  multinom.miss.test[i] / mspe.min
  lasso.miss.test.re[i] <- lasso.miss.test[i] / mspe.min
  naive1.miss.test.re[i] <- naive1.miss.test[i]/ mspe.min
  naive2.miss.test.re[i] <- naive2.miss.test[i]/ mspe.min
  naive3.miss.test.re[i] <- naive3.miss.test[i]/ mspe.min
  
}




train.mse <- cbind(lda = lda.miss.train, qda = qda.miss.train, multinom = multinom.miss.train, lasso = lasso.miss.train, naive1 = naive1.miss.train,
                    naive2 = naive2.miss.train, naive.pc = naive3.miss.train)


test.mse <- cbind(lda = lda.miss.test, qda = qda.miss.test, multinom = multinom.miss.test, lasso = lasso.miss.test, naive1 = naive1.miss.test,
                    naive2 = naive2.miss.test, naive.pc = naive3.miss.test)

test.mse.re <- cbind(lda = lda.miss.test.re, qda = qda.miss.test.re, multinom = multinom.miss.test.re, lasso = lasso.miss.test.re, naive1 = naive1.miss.test.re,
                    naive2 = naive2.miss.test.re, naive.pc = naive3.miss.test.re)

ColumnMean <- data.frame(colMeans(train.mse),colMeans(test.mse),colMeans(test.mse.re))

quartz(h=7,w=12, title = "training error rate")

boxplot(train.mse)

quartz(h=7,w=12, title = "test error rate")

boxplot(test.mse)

quartz(h=7,w=12, title = "relative test error rate")

boxplot(test.mse.re)
