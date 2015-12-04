# Neural nets via "nnet" and neuralnet"

prostate <-  read.table("C:\\Users\\tloughin\\Dropbox\\852 Modern Applied Methods\\R\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(neuralnet)
library(nnet)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

############
## Computing will work better if explanatories are rescaled to lie in [0,1]
# Function below scales values in a single object according to its own min and max.
#   Can apply it to training data, but validation set needs to be scaled
#   BY THE TRAINING SET MIN AND MAX!
############
rescale <- function(x){(x-min(x))/(max(x)-min(x))}

y.1 <- as.matrix(prostate[which(set==1),10])
x.1.unscaled <- as.matrix(prostate[which(set==1),c(2:9)]) # Original data set 1
x.1 <- apply(X=x.1.unscaled, MARGIN=2, FUN=rescale) #scaled data set 1
y.2 <- as.matrix(prostate[which(set==2),10])
x.2.unscaled <- as.matrix(prostate[which(set==2),c(2:9)]) # Original data set 2

# Scale set 2 to set 1's min and max
minx1 <- apply(X=x.1.unscaled, MARGIN=2, FUN=min)
maxx1 <- apply(X=x.1.unscaled, MARGIN=2, FUN=max)
x.2 <- matrix(data=NA, nrow=nrow(x.2.unscaled), ncol=ncol(x.2.unscaled))
for(i in 1:ncol(x.2.unscaled)){
  x.2[,i] <- (x.2.unscaled[,i] - minx1[i])/(maxx1[i] - minx1[i])
}

set1 <- which(set==1)

# First half of data 
###########################################################################
# First using nnet{nnet}.  This function can do regression or classification.
#  Only allows one hidden layer.  
#  For regression, defaults 
#     loss function RSS, 
#     sigmoidal activation functon, 
#     linear output function (MUST SPECIFY "linout=TRUE"), 
#     ONE hidden layer, 
#     generate initial weights randomly, 
#     NO SHRINKAGE ("weight decay") 
# Recommend shrinkage between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
###########################################################################

### Fit to set 1, test on Set 2
### RERUN THE CODE BELOW SEVERAL TIMES
#  Gives 2 main results.  Less common fit has worse training error, but better test error!
nn.1 <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, maxit=500)
p.nn.1 <-predict(nn.1, newdata=x.2)
MSPR.nn1 <- mean((y.2 - p.nn.1)^2)
MSE.in1 <- nn.1$value/nrow(x.1)
plot(x=y.2, y=p.nn.1,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.1)

# Unscaled X: If you run this MANY times, you may once in a while get the same fit as above.
# Otherwise, most commonly the fit is degernate.
# Note: trace=FALSE suppresses iteration history
nn.1u <- nnet(y=y.1, x=x.1.unscaled, linout=TRUE, size=1, maxit=500, trace=FALSE)
p.nn.1u <-predict(nn.1u, newdata=x.2.unscaled)
MSPR.nn1u <- mean((y.2 - p.nn.1u)^2)
MSE.in1u <- nn.1u$value/nrow(x.1.unscaled)
plot(x=y.2, y=p.nn.1u, ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.1u)


# Add small shrinkage
nn.1a <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=1e-4, maxit=500, trace=FALSE)
p.nn.1a <-predict(nn.1a, newdata=x.2)
MSPR.nn1a <- mean((y.2 - p.nn.1a)^2)
MSE.in1a <- nn.1a$value/nrow(x.1)
plot(x=y.2, y=p.nn.1a,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.1a)


# Increase shrinkage
nn.1b <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.001, maxit=500, trace=FALSE)
p.nn.1b <-predict(nn.1b, newdata=x.2)
MSPR.nn1b <- mean((y.2 - p.nn.1b)^2)
MSE.in1b <- nn.1b$value/nrow(x.1)
plot(x=y.2, y=p.nn.1b,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.1b)


# Increase penalty MORE:  
nn.1c <- nnet(y=y.1, x=x.1, linout=TRUE, size=1, decay=.01, maxit=500, trace=FALSE)
p.nn.1c <-predict(nn.1c, newdata=x.2)
MSPR.nn1c <- mean((y.2 - p.nn.1c)^2)
MSE.in1c <- nn.1c$value/nrow(x.1)
plot(x=y.2, y=p.nn.1c,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.1c)

# Add hidden node to size 2: OVERFITTING
nn.2 <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, maxit=500, trace=FALSE)
p.nn.2 <-predict(nn.2, newdata=x.2)
MSPR.nn2 <- mean((y.2 - p.nn.2)^2)
MSE.in2 <- nn.2$value/nrow(x.1)
plot(x=y.2, y=p.nn.2,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.2)

# Increase decay
nn.2b <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.001, maxit=500, trace=FALSE)
p.nn.2b <-predict(nn.2b, newdata=x.2)
MSPR.nn2b <- mean((y.2 - p.nn.2b)^2)
MSE.in2b <- nn.2b$value/nrow(x.1)
plot(x=y.2, y=p.nn.2b,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.2b)

# Increase decay more: Trial and error using Validation (test) set suggests 0.1
nn.2c <- nnet(y=y.1, x=x.1, linout=TRUE, size=2, decay=.1, maxit=500, trace=FALSE)
p.nn.2c <-predict(nn.2c, newdata=x.2)
MSPR.nn2c <- mean((y.2 - p.nn.2c)^2)
MSE.in2c <- nn.2c$value/nrow(x.1)
plot(x=y.2, y=p.nn.2c,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.2c)

# Can try adding nodes, but I don't recommend it here.  Massive overfitting.

nn.4 <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.001, maxit=500, trace=FALSE)
p.nn.4 <-predict(nn.4, newdata=x.2)
MSPR.nn4 <- mean((y.2 - p.nn.4)^2)
MSE.in4 <- nn.4$value/nrow(x.1)
plot(x=y.2, y=p.nn.4,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.4)

# Cranking up decay helps

nn.4a <- nnet(y=y.1, x=x.1, linout=TRUE, size=4, decay=.1, maxit=500, trace=FALSE)
p.nn.4a <-predict(nn.4a, newdata=x.2)
MSPR.nn4a <- mean((y.2 - p.nn.4a)^2)
MSE.in4a <- nn.4a$value/nrow(x.1)
plot(x=y.2, y=p.nn.4a,ylim=c(-1,6))
abline(a=0,b=1)
summary(nn.4a)


#I'll leave it to you to mess around with this further.  
#   Mess with the number of nodes and the decay.

nn.8 <- nnet(y=y.1, x=x.1, linout=TRUE, size=8, decay=.1, maxit=500, rang=1/max(x.1), trace=FALSE)
p.nn.8 <-predict(nn.8, newdata=x.2)
MSPR.nn8 <- mean((y.2 - p.nn.8)^2)
MSE.in8 <- nn.8$value/nrow(x.1)
plot(x=y.2, y=p.nn.8)
abline(a=0,b=1)
summary(nn.8)

###########################################################################
# "caret" is a library of functions for Classification And REgression Training. 
#   The train() function trains various predictors using bootstrap 
#   to fit model to data and predict remaining data.
#   See http://topepo.github.io/caret/training.html for details
# It uses the average performance across the multiple resampling iterations
#   as the primary performance criterion.  
#
# The warning about missing values is an error that is produced when 
#   a predictor returns constant values. 
###########################################################################

library(caret)

tuned.nnet <- train(x=x.1, y=y.1, method="nnet", preProcess="range", 
                    tuneGrid=expand.grid(.size=c(1,2,4,6),.decay=c(0,0.001,0.1,1)))
tuned.nnet
names(tuned.nnet)
# I am suspicious of these results.  From what we saw above, the RMSEs here are all too similar.  

# Let's make our own tuning code, shall we?
# Will use bootstrap to resample n observations with replacement
# For each resample, will examine every combination of tuning parameters
# Set of tuning parameters is in the "siz" and "dec" objects, number of bootstrap reps is "reps"

set.seed(39021039)
reps=100 # Feel free to increase this to tolerable value
siz <- c(1,2,4,6)
dec <- c(0,0.001,0.1,1)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)
MSE <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)

# Start data resampling loop
for(r in 1:reps){
  resamp <- sample.int(n=nrow(x.1), size=nrow(x.1), replace=TRUE)
  xr <- x.1[resamp,]
  yr <- y.1[resamp,]
  xp <- x.1[-unique(resamp),]
  yp <- y.1[-unique(resamp),]
# Set counter for storate of results
  qq <- 1
# Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
      p.nn <-predict(nn, newdata=xp)
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((yp - p.nn)^2)
      MSE[qq, r+2] <- nn$value/nrow(xr)
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
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR[which(MSPR.max[,3]<3),-c(1,2)]), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<3)])


###########################################################################
# Now try neuralnet.  This function can do only regression.
#  Advantage is that it allows any number of hidden layers.  
#  For regression, default is loss function RSS, logistic activation functon, 
#     linear output function, generate initial weights randomly, 
#     "rep" random starts, 
#  Does not do SHRINKAGE.
#  Takes a LONG time to run and gives weird results.
###########################################################################
prostate1 <- prostate[set1,-c(1,11)]
nnn.1 <- neuralnet(formula=lpsa ~ lcavol + lweight + age + lbph + 
                   svi + lcp + gleason + pgg45, 
                   data = prostate1, hidden=c(1), rep=10, stepmax=2e+06)
p.nnn.1 <- compute(nnn.1, covariate=x.2, rep=1)$net.result
MSPR.nnn1 <- mean((y.2 - p.nnn.1)^2)
plot(x=y.2, y=p.nnn.1,ylim=c(-1,6))
abline(a=0,b=1)
