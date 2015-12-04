# LASSO on prostate data using glmnet package 
#  (THERE IS ANOTHER PACKAGE THAT DOES LASSO.  WE WILL SEE IT LATER)
# Splitting the data in half and modeling each half separately.

prostate <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(glmnet)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

# Fit LASSO by glmnet(y=, x=). Gaussian is default, but other families are available  
#  Function produces series of fits for many values of lambda.  


# cv.glmnet() uses crossvalidation to estimate optimal lambda
cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
cv.lasso.1
plot(cv.lasso.1) # Plot CV-MSPE
coef(cv.lasso.1) # Print out coefficients at optimal lambda
# Predict both halves using first-half fit
predict.1.1 <- predict(cv.lasso.1, newx=x.1)
predict.1.2 <- predict(cv.lasso.1, newx=x.2)

# Repeat for second half of data
cv.lasso.2 <- cv.glmnet(y=y.2, x= x.2, family="gaussian")
cv.lasso.2
plot(cv.lasso.2)
coef(cv.lasso.2)
predict.2.1 <- predict(cv.lasso.2, newx=x.1)
predict.2.2 <- predict(cv.lasso.2, newx=x.2)

MSPE.lasso.1.1 <- mean((y.1 - predict.1.1)^2)
MSPE.lasso.1.2 <- mean((y.2 - predict.1.2)^2)
MSPE.lasso.2.1 <- mean((y.1 - predict.2.1)^2)
MSPE.lasso.2.2 <- mean((y.2 - predict.2.2)^2)

