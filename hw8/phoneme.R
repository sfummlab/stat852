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



lda.miss.test <- rep(0,5)
qda.miss.test <- rep(0,5)
multinom.miss.test <- rep(0,5)
lasso.miss.test <- rep(0,5)


lda.miss.test.re <- rep(0,5)
qda.miss.test.re <- rep(0,5)
multinom.miss.test.re <- rep(0,5)
lasso.miss.test.re <- rep(0,5)




phoneme.pc <-  prcomp(x=phoneme[,2:257], scale.=TRUE)
summary(phoneme.pc)


quartz(h=7,w=12)
par(mfrow=c(1,2))
plot(phoneme.pc)

evals <- phoneme.pc$sdev^2
plot(y=evals, x=c(1:256))
abline(a=0,b=0)
abline(a=0.5,b=0)

# Look at eigenvectors to see how variables contribute to PC's
phoneme.pc$rotation



phoneme.nume <- as.matrix(phoneme[,2:257])

phoneme.nume <- phoneme.nume %*% phoneme.pc$rotation[,1:256]


set.seed(67982193)

for(i in 1:5)
{
    resamp <- sample.int(n=nrow(phoneme.nume), size=0.7 * nrow(phoneme.nume), replace=FALSE)
    x.r  <- phoneme.nume[resamp,]
    y.r  <- phoneme[resamp,258]

    x.p  <- phoneme.nume[-unique(resamp),]
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

# Misclassification Errors
        pred.class.1 <- predict(mod.fit, newdata=set1.rescale)
        pred.prob.1 <- round(predict(mod.fit, newdata=set1.rescale, type="probs"), digits=3)

        pred.class.2 <- predict(mod.fit, newdata=set2.rescale)
        pred.prob.2 <- predict(mod.fit, newdata=set2.rescale, type="probs")

        (mul.misclass.train <- mean(ifelse(pred.class.1 == set1.rescale$class, yes=0, no=1)))

        (mul.misclass.test <- mean(ifelse(pred.class.2 == set2.rescale$class, yes=0, no=1)))

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


        mspe <- cbind(lascvmisclass.test, mul.misclass.test, qmisclass.test, lmisclass.test)
       
        mspe.min <- min(mspe)


        lda.miss.test.re[i] <- lda.miss.test[i] / mspe.min
        qda.miss.test.re[i] <- qda.miss.test[i] / mspe.min
        multinom.miss.test.re[i] <-  multinom.miss.test[i] /mspe.min
        lasso.miss.test.re[i] <- lasso.miss.test[i] /mspe.min

}




train.mse <- cbind(lda = lda.miss.train, qda = qda.miss.train, multinom = multinom.miss.train, lasso = lasso.miss.train)


test.mse <- cbind(lda = lda.miss.test, qda = qda.miss.test, multinom = multinom.miss.test, lasso = lasso.miss.test)

test.mse.re <- cbind(lda = lda.miss.test.re, qda = qda.miss.test.re, multinom = multinom.miss.test.re, lasso = lasso.miss.test.re)

quartz(h=7,w=12, title = "training error rate")

boxplot(train.mse)

quartz(h=7,w=12, title = "test error rate")

boxplot(test.mse)

quartz(h=7,w=12, title = "relative test error rate")

boxplot(test.mse.re)

