#Neural nets for Air Qulaity Data

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

y.1 <- as.matrix(airquality[which(airquality$Ozone>0),1])
x.1.unscaled <- as.matrix(airquality[which(airquality$Ozone>0),c(3,4)])
x.1 <- rescale(x.1.unscaled,x.1.unscaled)



# First half of data 
###########################################################################
# First using nnet{nnet}.  This function can do regression or classification.
#  Only allows one hidden layer.  
#  For regression, default is loss function RSS, logistic activation functon, 
#     linear output function (MUST SPECIFY "linout=TRUE"), ONE hidden layer, 
#     generate initial weights randomly, NO SHRINKAGE ("weight decay") 
# Recommend shrinkage between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
###########################################################################

### 1 node
nn.1.0 <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, maxit=500)
p.nn.1.0 <-predict(nn.1.0, newdata=x.1)
MSE.in1.0 <- nn.1.0$value/nrow(x.1)
plot(x=y.1, y=p.nn.1.0)
abline(a=0,b=1)
summary(nn.1.0)

# Add small shrinkage
### 1 node
nn.1.001 <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.001, maxit=500)
p.nn.1.001 <-predict(nn.1.001, newdata=x.1)
MSE.in1.001 <- nn.1.001$value/nrow(x.1)
plot(x=y.1, y=p.nn.1.001)
abline(a=0,b=1)
summary(nn.1.001)

# Add large shrinkage
### 1 node
nn.1.1 <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.1, maxit=500)
p.nn.1.1 <-predict(nn.1.1, newdata=x.1)
MSE.in1.1 <- nn.1.1$value/nrow(x.1)
plot(x=y.1, y=p.nn.1.1)
abline(a=0,b=1)
summary(nn.1.1)

### 2 node
nn.2.0 <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, maxit=500)
p.nn.2.0 <-predict(nn.2.0, newdata=x.1)
MSE.in2.0 <- nn.2.0$value/nrow(x.1)
plot(x=y.1, y=p.nn.2.0)
abline(a=0,b=1)
summary(nn.2.0)

# Add small shrinkage
### 2 node
nn.2.001 <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.001,maxit=500)
p.nn.2.001 <-predict(nn.2.001, newdata=x.1)
MSE.in2.001 <- nn.2.001$value/nrow(x.1)
plot(x=y.1, y=p.nn.2.001)
abline(a=0,b=1)
summary(nn.2.001)

# Add large shrinkage
### 2 node
nn.2.1 <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.1,maxit=500)
p.nn.2.1 <-predict(nn.2.1, newdata=x.1)
MSE.in2.1 <- nn.2.1$value/nrow(x.1)
plot(x=y.1, y=p.nn.2.1)
abline(a=0,b=1)
summary(nn.2.1)

# 4 nodes

### 4 node
nn.4.0 <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, maxit=500)
p.nn.4.0 <-predict(nn.4.0, newdata=x.1)
MSE.in4.0 <- nn.4.0$value/nrow(x.1)
plot(x=y.1, y=p.nn.4.0)
abline(a=0,b=1)
summary(nn.4.0)

# Add small shrinkage
### 4 node
nn.4.001 <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.001,maxit=500)
p.nn.4.001 <-predict(nn.4.001, newdata=x.1)
MSE.in4.001 <- nn.4.001$value/nrow(x.1)
plot(x=y.1, y=p.nn.4.001)
abline(a=0,b=1)
summary(nn.4.001)

# Add large shrinkage
### 2 node
nn.4.1 <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.1,maxit=500)
p.nn.4.1 <-predict(nn.4.1, newdata=x.1)
MSE.in4.1 <- nn.4.1$value/nrow(x.1)
plot(x=y.1, y=p.nn.4.1)
abline(a=0,b=1)
summary(nn.4.1)

