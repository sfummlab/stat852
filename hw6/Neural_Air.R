#Neural nets for Air Quality Data
#  This program is long and complicated.  
#    First, we run one NN.  Explore it several times and 
#      see how much the results can vary from random initial values.
#    Then we run a series of NNs using different tuning parameters.  
#      We run each NN 50 times to make sure that we can capture the best 
#      model that those settings can provide.
#    Last, we create a bootstrap loop that creates data splits with replacement, 
#      cycles through a series of tuning parameters, computes the test error 
#      on the omitted data, and produces box plots of the results.  
#
# nnet() performs better is the training data are scaled to lie between 0 and 1.
#   It gets estimates either way, but they are unreliable on unscaled data
#

library(nnet)

aq <- na.omit(airquality[,c("Ozone","Wind","Temp")])

set.seed(298192001)
# Split on a fixed sample size.  Different 
train <- sample(x=1:nrow(aq), size = .7*nrow(aq), replace = FALSE)
x.1.unscaled <- aq[train, 2:3]
y.1 <- aq[train,1]
x.2.unscaled <- aq[-train, 2:3]
y.2 <- aq[-train,1]

# Function that rescales x1 to x2's min and max
# If applied to itself, produces data scaled to lie within [0,1].

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

x.1 <- rescale(x.1.unscaled, x.1.unscaled)
#Prove that it worked
apply(X=x.1, MARGIN=2, FUN=min)
apply(X=x.1, MARGIN=2, FUN=max)
x.2 <- rescale(x.2.unscaled, x.1.unscaled)
#Prove that it worked, but does not perfectly scale test set
apply(X=x.2, MARGIN=2, FUN=min)
apply(X=x.2, MARGIN=2, FUN=max)

###########################################################################
# First using nnet::nnet.  This function can do regression or classification.
#  Only allows one hidden layer.  
#  For regression, default is loss function RSS, logistic activation functon, 
#     linear output function (MUST SPECIFY "linout=TRUE"), ONE hidden layer, 
#     generate initial weights randomly, NO SHRINKAGE ("weight decay") 
# Recommend shrinkage between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
###########################################################################

# Single run, try this repeatedly
nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, maxit=500)
(MSE <- nn$value/nrow(x.1))
(MSPE <- mean((y.2 - predict(nn, x.2))^2))
#  Show weights: "b" means intercept, "i" is "input variable"
#   "h" is hidden node, and "o" is "output variable"
summary(nn)
plot(x=y.1, y=predict(nn, x.1), main="1 Node, 0 decay")
abline(a=0,b=1)


win.graph(h=12,w=16, pointsize=9)
par(mfrow=c(3,4))
# Run 50 times and select model with best *training error*

### 1 node
MSE.in1.0 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in1.0){ 
    MSE.in1.0 <- MSE
    nn.1.0 <- nn
  }
}
MSE.in1.0
p.nn.1.0 <- predict(nn.1.0, x.1)
MSPE.1.0 <- mean((y.2 - predict(nn.1.0,x.2))^2)
plot(x=y.1, y=p.nn.1.0, main=paste("1 Node, 0 decay, MSE=",round(MSE.in1.0)))
abline(a=0,b=1)

# Add small shrinkage
### 1 node
MSE.in1.001 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=0.001, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in1.001){ 
    MSE.in1.001 <- MSE
    nn.1.001 <- nn
  }
}
MSE.in1.001
p.nn.1.001 <- predict(nn.1.001, x.1)
MSPE.1.001 <- mean((y.2 - predict(nn.1.001,x.2))^2)
plot(x=y.1, y=p.nn.1.001, main=paste("1 Node, 0.001 decay, MSE=",round(MSE.in1.001)))
abline(a=0,b=1)


# Add larger shrinkage
### 1 node
MSE.in1.1 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=0.1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in1.1){ 
    MSE.in1.1 <- MSE
    nn.1.1 <- nn
  }
}
MSE.in1.1
p.nn.1.1 <- predict(nn.1.1, x.1)
MSPE.1.1 <- mean((y.2 - predict(nn.1.1,x.2))^2)
plot(x=y.1, y=p.nn.1.1, main=paste("1 Node, 0.1 decay, MSE=",round(MSE.in1.1)))
abline(a=0,b=1)


# Add large shrinkage
### 1 node
MSE.in11 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in11){ 
    MSE.in11 <- MSE
    nn.11 <- nn
  }
}
MSE.in11
p.nn.11 <- predict(nn.11, x.1)
MSPE.11 <- mean((y.2 - predict(nn.11,x.2))^2)
plot(x=y.1, y=p.nn.11, main=paste("1 Node, 1 decay, MSE=",round(MSE.in11)))
abline(a=0,b=1)

### 2 node
MSE.in2.0 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in2.0){ 
    MSE.in2.0 <- MSE
    nn.2.0 <- nn
  }
}
MSE.in2.0
p.nn.2.0 <- predict(nn.2.0, x.1)
MSPE.2.0 <- mean((y.2 - predict(nn.2.0,x.2))^2)
plot(x=y.1, y=p.nn.2.0, main=paste("2 Node, 0 decay, MSE=",round(MSE.in2.0)))
abline(a=0,b=1)

# Add small shrinkage
### 2 node
MSE.in2.001 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=0.001, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in2.001){ 
    MSE.in2.001 <- MSE
    nn.2.001 <- nn
  }
}
MSE.in2.001
p.nn.2.001 <- predict(nn.2.001, x.1)
MSPE.2.001 <- mean((y.2 - predict(nn.2.001,x.2))^2)
plot(x=y.1, y=p.nn.2.001, main=paste("2 Node, 0.001 decay, MSE=",round(MSE.in2.001)))
abline(a=0,b=1)


# Add larger shrinkage
### 2 node
MSE.in2.1 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=0.1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in2.1){ 
    MSE.in2.1 <- MSE
    nn.2.1 <- nn
  }
}
MSE.in2.1
p.nn.2.1 <- predict(nn.2.1, x.1)
MSPE.2.1 <- mean((y.2 - predict(nn.2.1,x.2))^2)
plot(x=y.1, y=p.nn.2.1, main=paste("2 Node, 0.1 decay, MSE=",round(MSE.in2.1)))
abline(a=0,b=1)


# Add large shrinkage
### 2 node
MSE.in21 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in21){ 
    MSE.in21 <- MSE
    nn.21 <- nn
  }
}
MSE.in21
p.nn.21 <- predict(nn.21, x.1)
MSPE.21 <- mean((y.2 - predict(nn.21,x.2))^2)
plot(x=y.1, y=p.nn.21, main=paste("2 Node, 1 decay, MSE=",round(MSE.in21)))
abline(a=0,b=1)

### 4 node
MSE.in4.0 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in4.0){ 
    MSE.in4.0 <- MSE
    nn.4.0 <- nn
  }
}
MSE.in4.0
p.nn.4.0 <- predict(nn.4.0, x.1)
MSPE.4.0 <- mean((y.2 - predict(nn.4.0,x.2))^2)
plot(x=y.1, y=p.nn.4.0, main=paste("4 Node, 0 decay, MSE=",round(MSE.in4.0)))
abline(a=0,b=1)

# Add small shrinkage
### 4 node
MSE.in4.001 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=0.001, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in4.001){ 
    MSE.in4.001 <- MSE
    nn.4.001 <- nn

  }
}
MSE.in4.001
p.nn.4.001 <- predict(nn.4.001, x.1)
MSPE.4.001 <- mean((y.2 - predict(nn.4.001,x.2))^2)
plot(x=y.1, y=p.nn.4.001, main=paste("4 Node, 0.001 decay, MSE=",round(MSE.in4.001)))
abline(a=0,b=1)


# Add larger shrinkage
### 4 node
MSE.in4.1 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=0.1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in4.1){ 
    MSE.in4.1 <- MSE
    nn.4.1 <- nn
  }
}
MSE.in4.1
p.nn.4.1 <- predict(nn.4.1, x.1)
MSPE.4.1 <- mean((y.2 - predict(nn.4.1,x.2))^2)
plot(x=y.1, y=p.nn.4.1, main=paste("4 Node, 0.1 decay, MSE=",round(MSE.in4.1)))
abline(a=0,b=1)


# Add large shrinkage
### 4 node
MSE.in41 <- 9e99
for(i in 1:50){
  nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=1, maxit=500)
  MSE <- nn$value/nrow(x.1)
  if(MSE < MSE.in41){ 
    MSE.in41 <- MSE
    nn.41 <- nn
  }
}
MSE.in41
p.nn.41 <- predict(nn.41, x.1)
MSPE.41 <- mean((y.2 - predict(nn.41,x.2))^2)
plot(x=y.1, y=p.nn.41, main=paste("4 Node, 1 decay, MSE=",round(MSE.in41)))
abline(a=0,b=1)

# MSPE Plot
win.graph(h=12,w=16, pointsize=9)
par(mfrow=c(3,4))


plot(x=y.2, y=predict(nn.1.0,x.2), main=paste("1 Node, 0 decay, MSPE=",round(MSPE.1.0)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.1.001,x.2), main=paste("1 Node, 0.001 decay, MSPE=",round(MSPE.1.001)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.1.1,x.2), main=paste("1 Node, 0.1 decay, MSPE=",round(MSPE.1.1)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.11,x.2), main=paste("1 Node, 1 decay, MSPE=",round(MSPE.11)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.2.0,x.2), main=paste("2 Node, 0 decay, MSPE=",round(MSPE.2.0)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.2.001,x.2), main=paste("2 Node, 0.001 decay, MSPE=",round(MSPE.2.001)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.2.1,x.2), main=paste("2 Node, 0.1 decay, MSPE=",round(MSPE.2.1)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.21,x.2), main=paste("2 Node, 1 decay, MSPE=",round(MSPE.21)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.4.0,x.2), main=paste("4 Node, 0 decay, MSPE=",round(MSPE.4.0)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.4.001,x.2), main=paste("4 Node, 0.001 decay, MSPE=",round(MSPE.4.001)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.4.1,x.2), main=paste("4 Node, 0.1 decay, MSPE=",round(MSPE.4.1)))
abline(a=0,b=1)
plot(x=y.2, y=predict(nn.41,x.2), main=paste("4 Node, 1 decay, MSPE=",round(MSPE.41)))
abline(a=0,b=1)


##################################################
# uSING BOOTSTRAP TO SELECT TUNING PARAMETERS
###################################################

set.seed(41891019)
reps=10 # Boot Reps, Feel free to increase this to tolerable value
nets=20 # Number of nnets per setting.  Could reduce to just a few
siz <- c(1,2,4,6)
dec <- c(0,0.001,0.1,1)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)
MSE <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)

# Start data resampling loop
for(r in 1:reps){
  resamp <- sample.int(n=nrow(aq), size=nrow(aq), replace=TRUE)
  x.1 <- aq[resamp,2:3]
  xr <- rescale(x.1,x.1)
  yr <- aq[resamp,1]
  x.2 <- aq[-unique(resamp),2:3]
  xp <- rescale(x.2, x.1)
  yp <- aq[-unique(resamp),1]
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((yp - p.min)^2)
      MSE[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

# Compute mean, minimum, and maximum
(MSPR.mean <- cbind(MSPR[,1:2], apply(X=MSPR[,-c(1,2)], MARGIN=1, FUN=mean)))
(MSPR.min <- cbind(MSPR[,1:2], apply(X=MSPR[,-c(1,2)], MARGIN=1, FUN=min)))
(MSPR.max <- cbind(MSPR[,1:2], apply(X=MSPR[,-c(1,2)], MARGIN=1, FUN=max)))
(MSE.mean <- cbind(MSE[,1:2], apply(X=MSE[,-c(1,2)], MARGIN=1, FUN=mean)))
(MSE.min <- cbind(MSE[,1:2], apply(X=MSE[,-c(1,2)], MARGIN=1, FUN=min)))
(MSE.max <- cbind(MSE[,1:2], apply(X=MSE[,-c(1,2)], MARGIN=1, FUN=max)))

# Plot results. 
siz.dec <- paste(MSPR[,1],MSPR[,2])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)

# The first plot has some extreme results in it, and we really only care about the lower ones, 
#    so eliminate extremes. 
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[which(MSPR.max[,3]<1000),-c(1,2)]), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<1000)])

# Make plot relative to best
best <- apply(X=MSPR[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPR[which(MSPR.max[,3]<1000),-c(1:2)])/best)), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<1000)])


# Best results are at boundary, so try some more values
siz <- c(4,5,6,8)
dec <- c(1,1.5,2)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR2 <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)
MSE2 <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)

# Start data resampling loop
for(r in 1:reps){
  resamp <- sample.int(n=nrow(aq), size=nrow(aq), replace=TRUE)
  x.1 <- aq[resamp,2:3]
  xr <- rescale(x.1,x.1)
  yr <- aq[resamp,1]
  x.2 <- aq[-unique(resamp),2:3]
  xp <- rescale(x.2, x.1)
  yp <- aq[-unique(resamp),1]
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR2[qq,1:2] <- c(s,d)
      MSE2[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:50){
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR2[qq, r+2] <- mean((yp - p.min)^2)
      MSE2[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

# Compute mean, minimum, and maximum
(MSPR2.mean <- cbind(MSPR2[,1:2], apply(X=MSPR2[,-c(1,2)], MARGIN=1, FUN=mean)))
(MSPR2.min <- cbind(MSPR2[,1:2], apply(X=MSPR2[,-c(1,2)], MARGIN=1, FUN=min)))
(MSPR2.max <- cbind(MSPR2[,1:2], apply(X=MSPR2[,-c(1,2)], MARGIN=1, FUN=max)))
(MSE2.mean <- cbind(MSE2[,1:2], apply(X=MSE2[,-c(1,2)], MARGIN=1, FUN=mean)))
(MSE2.min <- cbind(MSE2[,1:2], apply(X=MSE2[,-c(1,2)], MARGIN=1, FUN=min)))
(MSE2.max <- cbind(MSE2[,1:2], apply(X=MSE2[,-c(1,2)], MARGIN=1, FUN=max)))

# Plot results. 
siz.dec <- paste(MSPR2[,1],MSPR2[,2])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR2[,-c(1,2)]), use.cols=FALSE, names=siz.dec)

# The first plot has some extreme results in it, and we really only care about the lower ones, 
#    so eliminate extremes. 
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR2[which(MSPR2.max[,3]<1000),-c(1,2)]), use.cols=FALSE, names=siz.dec[which(MSPR2.max[,3]<1000)])

# Make plot relative to best
best <- apply(X=MSPR2[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPR2[,-c(1:2)])/best)), use.cols=FALSE, names=siz.dec)
