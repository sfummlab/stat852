#  Crossvalidation to get prediction error.  The crossval() function in package bootstrap seems most direct

prostate <-  read.table("~/stat852/Prostate.csv", header=TRUE, sep=",", na.strings=" ")

library(glmnet)
library(bootstrap)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

# crossval() requires two functions: 
#   One that takes x and y as the arguments and returns a model fit of any type
#   One that takes the model fit and x as arguments and returns predicted values.

# I coundn't figure out how to make lars() give me a model fit in one function 
#   and predicted values in another, so using glmnet()

# Model fit function
lasso.fit <- function(x,y){
  cv.glmnet(y=y, x= x, family="gaussian")
}
# Prediction function
lasso.pred <- function(fit,x){
  predict(fit, x)
}
#  Tested the functions first
fit <- lasso.fit(x.1,y.1)
lpred <- lasso.pred(fit,x.1)

###############################################################
# NOTE: RERUN THESE SEVERAL TIMES TO SEE HOW VARIABLE CV IS!  #
#  Running these on the first group to estimate MSPE          #
###############################################################
# Crossvalidation using CV with V=5 ("ngroup")
lasso.cv5 <- crossval(x=x.1, y=y.1, theta.fit=lasso.fit, 
                      theta.predict=lasso.pred, ngroup=5)
plot(y=lasso.cv5$cv.fit, x=y.1)
mspe.lasso.cv5 <- mean((y.1 - lasso.cv5$cv.fit)^2)
# Crossvalidation using CV with V=10 ("ngroup")
lasso.cv10 <- crossval(x=x.1, y=y.1, theta.fit=lasso.fit, 
                       theta.predict=lasso.pred, ngroup=10)
plot(y=lasso.cv10$cv.fit, x=y.1)
mspe.lasso.cv10 <- mean((y.1 - lasso.cv10$cv.fit)^2)
# Crossvalidation using CV with V=n (supposedly the default).  Why doesn't this work?
#   Note: n=52.  Works with 26, not with 30 or higher.  
lasso.cvn <- crossval(x=x.1, y=y.1, theta.fit=lasso.fit, 
                      theta.predict=lasso.pred, ngroup=length(y.1))
plot(y=lasso.cvn$cv.fit, x=y.1)
mspe.lasso.cvn <- mean((y.1 - lasso.cvn$cv.fit)^2)
# MSPE on test data set 2
lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
lasso.pred.x2 <- predict(lasso,x.2)
mspe.lasso.x2 <- mean((y.2 - lasso.pred.x2)^2)


# Repeating calculations with OLS on all variables
ols.fit <- function(x,y){
  lsfit(x,y)
}
ols.predict <- function(fit,x){
  cbind(1,x) %*% fit$coef
}
ols.cv5 <- crossval(x=x.1, y=y.1, theta.fit=ols.fit, 
                    theta.predict=ols.predict, ngroup=5)
mspe.ols.cv5 <- mean((y.1 - ols.cv5$cv.fit)^2)
ols.cv10 <- crossval(x=x.1, y=y.1, theta.fit=ols.fit, 
                     theta.predict=ols.predict, ngroup=10)
mspe.ols.cv10 <- mean((y.1 - ols.cv10$cv.fit)^2)
# Default ngroups is supposed to be N for leave-one-out CV. 
# Produces an error!  ?????
ols.cvn <- crossval(x=x.1, y=y.1, theta.fit=ols.fit, theta.predict=ols.predict)
mspe.ols.cvn <- mean((y.1 - ols.cvn$cv.fit)^2)
# MSPE on test data set 2
ols <- lsfit(x.1,y.1)
ols.pred.x2 <- cbind(1,x.2) %*% ols$coef
mspe.ols.x2 <- mean((y.2-ols.pred.x2)^2)

# Now do stepwise selection

step.fit <- function(x,y) {
  data=data.frame(y,x)
  initial.1 <- lm(data=data,formula=y~ 1)
  final.1 <- lm(data=data, formula=y~.)
  step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,
                k = log(nrow(data)))
  step1
}

step.pred <- function(fit,x){
  predict(fit,as.data.frame(x))
}
step.cv5 <- crossval(x=x.1, y=y.1, theta.fit=step.fit, 
                     theta.predict=step.pred, ngroup=5)
mspe.step.cv5 <- mean((y.1 - step.cv5$cv.fit)^2)
step.cv10 <- crossval(x=x.1, y=y.1, theta.fit=step.fit, 
                      theta.predict=step.pred, ngroup=10)
mspe.step.cv10 <- mean((y.1 - step.cv10$cv.fit)^2)
data=data.frame(y.1,x.1)
initial.1 <- lm(data=data,formula=y.1~ 1)
final.1 <- lm(data=data, formula=y.1~.)
step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,
              k = log(nrow(data)))
step.pred.x2 <- predict(step1,as.data.frame(x.2))
mspe.step.x2 <- mean((y.2-step.pred.x2)^2)



#Now rerun the comparison of the three estimation methods on multiple data splits

R=20
set.seed(1289650) 
MSPEs <- matrix(NA, nrow=3*R, ncol=2)
counter <- 1
for(r in 1:R){
  prostate$new <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  y.1n <- prostate[which(prostate$new==1), 10]
  x.1n <- as.matrix(prostate[which(prostate$new==1), c(2:9)])
  y.2n <- prostate[which(prostate$new==2), 10]
  x.2n <- as.matrix(prostate[which(prostate$new==2), c(2:9)])
  
  lasso.n <- cv.glmnet(y=y.1n, x= x.1n, family="gaussian")
  lasso.pred.x2n <- predict(lasso.n, x.2n)
  MSPEs[counter,]<- c("LASSO", mean((y.2n - lasso.pred.x2n)^2))
  counter <- counter+1
  
  olsn <- lsfit(x.1n,y.1n)
  ols.pred.x2n <- cbind(1,x.2n) %*% olsn$coef
  MSPEs[counter,]<- c("OLS", mean((y.2n - ols.pred.x2n)^2))
  counter <- counter+1
  
  mspe.ols.x2 <- mean((y.2-ols.pred.x2)^2)
  data=data.frame(y.1n,x.1n)
  initial.1n <- lm(data=data,formula=y.1n~ 1)
  final.1n <- lm(data=data, formula=y.1n~.)
  step1n <- step(object=initial.1n, scope=list(upper=final.1n), trace=0,
                 k = log(nrow(data)))
  step.pred.x2n <- predict(step1n, as.data.frame(x.2n))
  MSPEs[counter,]<- c("STEP", mean((y.2n-step.pred.x2n)^2))
  counter <- counter+1
}
MSPEs

# Plots
x11(height=5, width=5, pointsize=15)
stripchart(as.numeric(MSPEs[,2]) ~ MSPEs[,1], method="jitter", jitter=.1, 
           vertical=TRUE, pch=20, ylab='MSPE')


# NOW TRY BOOTSTRAP
# Need to specify the prediction function as before.
# Also Need to specify loss function for computing error

sqerr <- function(y,yhat){(y-yhat)^2}

# bootpred() lists .632 bootstrap estimate third. First is the sample MSE
mspe.lasso.boot.100 <- bootpred(x=x.1, y=y.1, nboot=100, theta.fit=lasso.fit, 
                                theta.predict=lasso.pred, err.meas=sqerr)
mspe.lasso.boot.100

# Warnings are because row names (observation IDs) are duplicated
mspe.step.boot.100 <- bootpred(x=x.1, y=y.1, nboot=100, theta.fit=step.fit, 
                               theta.predict=step.pred, err.meas=sqerr)
mspe.step.boot.100

mspe.ols.boot.100 <- bootpred(x=x.1, y=y.1, nboot=100, theta.fit=ols.fit, theta.predict=ols.predict, err.meas=sqerr)
mspe.ols.boot.100