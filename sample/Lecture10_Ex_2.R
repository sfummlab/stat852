# Regression trees for Abelone Data
abelone <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\abelone.csv", header=TRUE, sep=",", na.strings=" ")

abelone$sex.m = ifelse(abelone$Sex==1, yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex==2, yes=1, no=0)

head(abelone)

#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test

set.seed(29003092)
U <- runif(n=nrow(abelone))
set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))

library(neuralnet)
library(nnet)

# Splitting data in half using random uniform selection to make two "set"s.


############
## Computing will work better if explanatories are rescaled to lie in [0,1]
############
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
    }
  x1
}


y.1 <- as.matrix(abelone[which(set==1),9])
x.1.unscaled <- as.matrix(abelone[which(set==1),-c(1,9)])
x.1 <- rescale(x.1.unscaled,x.1.unscaled)
y.2 <- as.matrix(abelone[which(set==2),9])
x.2.unscaled <- as.matrix(abelone[which(set==2),-c(1,9)])
x.2 <- rescale(x.2.unscaled,x.1.unscaled)
y.3 <- as.matrix(abelone[which(set==3),9])
x.3.unscaled <- as.matrix(abelone[which(set==3),-c(1,9)])
x.3 <- rescale(x.3.unscaled,x.1.unscaled)

# First half of data 
###########################################################################
# First using nnet{nnet}.  This function can do regression or classification.
#  Only allows one hidden layer.  
#  For regression, default is loss function RSS, logistic activation functon, 
#     linear output function (MUST SPECIFY "linout=TRUE"), ONE hidden layer, 
#     generate initial weights randomly, NO SHRINKAGE ("weight decay") 
# Recommend shrinkage between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
###########################################################################

### Fit to set 1, test on Set 2
nn.1 <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, maxit=500)
p.nn.1 <-predict(nn.1, newdata=x.2)
MSPR.nn1 <- mean((y.2 - p.nn.1)^2)
MSE.in1 <- nn.1$value/nrow(x.1)
plot(x=y.2, y=p.nn.1)
abline(a=0,b=1)
summary(nn.1)

# Add small shrinkage
nn.1a <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=1e-4, maxit=500)
p.nn.1a <-predict(nn.1a, newdata=x.2)
MSPR.nn1a <- mean((y.2 - p.nn.1a)^2)
MSE.in1a <- nn.1a$value/nrow(x.1)
plot(x=y.2, y=p.nn.1a)
abline(a=0,b=1)
summary(nn.1a)


# Increase shrinkage
nn.1b <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.001, maxit=500)
p.nn.1b <-predict(nn.1b, newdata=x.2)
MSPR.nn1b <- mean((y.2 - p.nn.1b)^2)
MSE.in1b <- nn.1b$value/nrow(x.1)
plot(x=y.2, y=p.nn.1b)
abline(a=0,b=1)
summary(nn.1b)


# Increase penalty MORE:  
nn.1c <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.01, maxit=500)
p.nn.1c <-predict(nn.1c, newdata=x.2)
MSPR.nn1c <- mean((y.2 - p.nn.1c)^2)
MSE.in1c <- nn.1c$value/nrow(x.1)
plot(x=y.2, y=p.nn.1c)
abline(a=0,b=1)
summary(nn.1c)

# Add hidden node to size 2: 
nn.2 <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, maxit=500)
p.nn.2 <-predict(nn.2, newdata=x.2)
MSPR.nn2 <- mean((y.2 - p.nn.2)^2)
MSE.in2 <- nn.2$value/nrow(x.1)
plot(x=y.2, y=p.nn.2)
abline(a=0,b=1)
summary(nn.2)

# Increase decay
nn.2b <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.001, maxit=500)
p.nn.2b <-predict(nn.2b, newdata=x.2)
MSPR.nn2b <- mean((y.2 - p.nn.2b)^2)
MSE.in2b <- nn.2b$value/nrow(x.1)
plot(x=y.2, y=p.nn.2b)
abline(a=0,b=1)
summary(nn.2b)

# Increase decay more: 
nn.2c <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.1, maxit=500)
p.nn.2c <-predict(nn.2c, newdata=x.2)
MSPR.nn2c <- mean((y.2 - p.nn.2c)^2)
MSE.in2c <- nn.2c$value/nrow(x.1)
plot(x=y.2, y=p.nn.2c)
abline(a=0,b=1)
summary(nn.2c)

# 4 nodes

nn.4 <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.001, maxit=1000, trace=FALSE)
p.nn.4 <-predict(nn.4, newdata=x.2)
MSPR.nn4 <- mean((y.2 - p.nn.4)^2)
MSE.in4 <- nn.4$value/nrow(x.1)
plot(x=y.2, y=p.nn.4)
abline(a=0,b=1)
summary(nn.4)

# Cranking up decay helps

nn.4a <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.1, maxit=500)
p.nn.4a <-predict(nn.4a, newdata=x.2)
MSPR.nn4a <- mean((y.2 - p.nn.4a)^2)
MSE.in4a <- nn.4a$value/nrow(x.1)
plot(x=y.2, y=p.nn.4a)
abline(a=0,b=1)
summary(nn.4a)


nn.8 <- nnet(y=y.1, x=x.1, linout=TRUE, size=8, decay=.1, maxit=500, trace=FALSE)
p.nn.8 <-predict(nn.8, newdata=x.2)
MSPR.nn8 <- mean((y.2 - p.nn.8)^2)
MSE.in8 <- nn.8$value/nrow(x.1)
plot(x=y.2, y=p.nn.8)
abline(a=0,b=1)
summary(nn.8)

nn.10 <- nnet(y=y.1, x=x.1, linout=TRUE, size=10, decay=.1, maxit=500, trace=FALSE)
p.nn.10 <-predict(nn.10, newdata=x.2)
MSPR.nn10 <- mean((y.2 - p.nn.10)^2)
MSE.in10 <- nn.10$value/nrow(x.1)
plot(x=y.2, y=p.nn.10)
abline(a=0,b=1)
summary(nn.10)

nn.15 <- nnet(y=y.1, x=x.1, linout=TRUE, size=15, decay=.1, maxit=500, trace=FALSE, rang=1)
p.nn.15 <-predict(nn.15, newdata=x.2)
MSPR.nn15 <- mean((y.2 - p.nn.15)^2)
MSE.in15 <- nn.15$value/nrow(x.1)
plot(x=y.2, y=p.nn.15)
abline(a=0,b=1)
summary(nn.15)

nn.20 <- nnet(y=y.1, x=x.1, linout=TRUE, size=20, decay=1, maxit=500, trace=FALSE)
p.nn.20 <-predict(nn.20, newdata=x.2)
MSPR.nn20 <- mean((y.2 - p.nn.20)^2)
MSE.in20 <- nn.20$value/nrow(x.1)
plot(x=y.2, y=p.nn.20)
abline(a=0,b=1)
#plot(x=y.1, y=predict(nn.20, newdata=x.1))
#abline(a=0,b=1)
summary(nn.20)

nn.50 <- nnet(y=y.1, x=x.1, linout=TRUE, size=50, decay=1, maxit=500, trace=FALSE)
p.nn.50 <-predict(nn.50, newdata=x.2)
MSPR.nn50 <- mean((y.2 - p.nn.50)^2)
MSE.in50 <- nn.50$value/nrow(x.1)
plot(x=y.2, y=p.nn.50)
abline(a=0,b=1)

#Final Model: Compute set 3 test error

nn.fin <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.001, maxit=500)
p.nn.fin <-predict(nn.fin, newdata=x.3)
MSPR.nnfin3 <- mean((y.3 - p.nn.fin)^2)
MSE.infin <- nn.fin$value/nrow(x.1)
plot(x=y.3, y=p.nn.fin)
abline(a=0,b=1)
summary(nn.fin)

#Final Model: Compute set 2+3 "test" error
y.23 <- as.matrix(abelone[which(set!=1),9])
x.23.unscaled <- as.matrix(abelone[which(set!=1),-c(1,9)])
x.23 <- rescale(x.23.unscaled,x.1.unscaled)


p.nn.fin23 <-predict(nn.fin, newdata=x.23)
MSPR.nnfin23 <- mean((y.23 - p.nn.fin23)^2)
plot(x=y.3, y=p.nn.fin)
abline(a=0,b=1)

