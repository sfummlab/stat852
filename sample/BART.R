# Gradient Boosting using gbm 
# Air Quality data

library(BayesTree)
####################################################################
## bart() fits the BART model using Bayesian backfitting  
## Tuning parameters and defaults include 
# sigdf = 3 (degrees of freedom for inverse Gaussian prior on sigma^2)
#           (paper suggests 3-10)
# sigquant = .90 (used for calibrating IG prior for sigma^2)
#                (paper suggests .90-.99)
# k = 2.0 (Used for calibrating the prior variance on the distribution of node values)
#         (paper suggests 1-3)
# ntree = 200 (Number of trees)
#             (Paper suggests that large number is OK, but computing issues dominate)
# Also, MCMC iterations are controlled by ndpost=1000 and nskip=100.
#   These are the number of draws that are used in the posterior and the number of initial draws 
#   skipped for burn-in, respectively. Are these defaults adequate?  I don't know!
####################################################################
AQ <- na.omit(airquality[,c(1,3,4)])

# x.train and y.train are the training set.  Note that y.train cannot be integer, as it is here.
#   Can also include x.test for a test sample
# Fit using defaults
air.bart <- bart(x.train=AQ[,-1], y.train=as.numeric(AQ[,1]))

summary(air.bart)
#Number of times each variable is used in each MCMC cycle (across 200 trees)
air.bart$varcount 
# Matrix containing the predicted values from each MCMC cycle
dim(air.bart$yhat.train)
# Can pull off important quantities like means and quantiles using apply()
#   Mean is already contained in yhat.train.mean
# Could get test data predictions from yhat.test.mean

(MSE <- mean((AQ[,1] - air.bart$yhat.train.mean)^2))
# Useful plots: Plot of sigma against MCMC cycle,
#  Plot of posterior intervals (95%?) for each predicted value
x11(h=7, w=16, pointsize=12)
plot(air.bart)

###############################################################
# Let's try training!  We will use the bootstrap algorithm I gave
# in the neural net program, but this time we will use something more 
# like multiple CV runs.  we will train to 90% of the data and
# test on the remaining 10%.  This requires only reducing the number 
# of draws in the sample() and not allowing replacement in the selection

x.1 <- AQ[,-1]
y.1 <- AQ[,1]

set.seed(9290271)
reps=10 # Feel free to increase this to tolerable value
k <- c(1,2,3)
ntree <- c(100,200,300)
sigdf <- c(3,6,9)
sigquant = c(.90,.95,.99)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR <- matrix(NA, nrow=length(k)*length(ntree)*length(sigdf)*length(sigquant),
               ncol=reps+4)
MSE <- matrix(NA, nrow=length(k)*length(ntree)*length(sigdf)*length(sigquant),
              ncol=reps+4)

# Start data resampling loop
for(r in 1:reps){
# This changes for train/test vs. Boot
  resamp <- sample.int(n=nrow(x.1), size=round(.9*nrow(x.1)), replace=FALSE)
  xr <- x.1[resamp,]
  yr <- y.1[resamp,]
  xp <- x.1[-unique(resamp),]
  yp <- y.1[-unique(resamp),]
  # Set counter for storate of results
  qq <- 1
  # Cycle over all parameter values
  for(kk in k){
    for(nn in ntree){
      for(df in sigdf){
        for(sq in sigquant){
          MSPR[qq,1:4] <- c(kk,nn,df,sq)
          MSE[qq,1:4] <- c(kk,nn,df,sq)
      # Run BART and get MSPE and MSE from run
          bb <- bart(y.train=as.numeric(yr), x.train=xr, x.test=xp''
                     k=kk, ntree=nn, sigdf=df, sigquant=sq, verbose=FALSE)
     # Save results in new column of matrix
          MSPR[qq, r+2] <- mean((yp - bb$yhat.test.mean)^2)
          MSE[qq, r+2] <- mean((yp - bb$yhat.train.mean)^2)
      # Increment counter for next row
          qq <- qq + 1
        }
      }
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
index <- paste(MSPR[,1],MSPR[,2],MSPR[,3],MSPR[,4])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[,-c(1:4)]), use.cols=FALSE, names=index)

# The first plot has some extreme results in it, and we really only care about the lower ones, 
#    so eliminate extremes. 
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR[which(MSPR.max[,3]<3),-c(1,2)]), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<3)])
