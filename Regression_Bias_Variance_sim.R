# Simulation of regression to compare linear models with different
#   numbers of variables.
# Model is E(Y) = 0 + 1 X1 + 1 X2 + e   with e~N(0,1)
# Three variables are measured: x1,x2,x3.  All are generated U(0,1)

# We fit 3 models: 
#   1: X1   2: X1, X2    3: X1, X2, X3
# We then compute predicted values at a small grid of points:
#  (.1,.3,.5,.7,.9)^3
# We compute the bias, variance, and MSE of the prediction at each X-combo
# We then summarize these results in plots and with summary statistics
# We also compute the model-based estimate of variance and estimate
#   its expectation as the average across simulations 



set.seed(392039853)

reps <- 200 # Number of data sets
N <- 20      # Sample size

# Create test data
test <- expand.grid(x1 = c(.1,.3,.5,.7,.9), x2 = c(.1,.3,.5,.7,.9), x3=c(.1,.3,.5,.7,.9))

# Assuming beta1=1, beta2=1, beta3=0
# Create vector of true means = 1*x1 + 1*x2
mu <- test$x1 + test$x2

# Prepare for looping over reps
counter <- 1
# Matrix to save predictions: rows are replicates, 
#   columns are different X combinations times 3 (one for each model)
save.pred <- matrix(data=NA, ncol=3*nrow(test), nrow=reps)
# Matrix to save estimates of sigma^2
#   Rows are replicates, columns are different models 
save.sig <- matrix(data=NA, ncol=3, nrow=reps)

# Loop to generate data, analyze, and save results
for(counter in c(1:reps)){
# Generating Uniform X's and Normal errors
  x1 <- runif(n=N)
  x2 <- runif(n=N)
  x3 <- runif(n=N)
  ep <- rnorm(n=N)
# Setting beta1=1, beta2=1, beta3=0
  y <- 1*x1 + 1*x2 + ep

# reg* is model-fit object, sig* is MSE, pred* is list of predicted values over grid 
  reg1 <- lm(y~x1)
  sig1 <- sum(resid(reg1)^2) / reg1$df.residual
# Could have used summary(reg1)$sigma^2
  pred1 <- predict(reg1,newdata=test)

  reg2 <- lm(y~x1 + x2)
  sig2 <- sum(resid(reg2)^2) / reg2$df.residual
  pred2 <- predict(reg2,newdata=test)

  reg3 <- lm(y~x1 + x2 + x3)
  sig3 <- sum(resid(reg3)^2) / reg3$df.residual
  pred3 <- predict(reg3,newdata=test)

# Saving all results into storage objects and incrementing row counter
  save.pred[counter,] <- c(pred1, pred2, pred3)
  save.sig[counter,] <- c(sig1,sig2,sig3)
  counter <- counter + 1
}

# Estimate bias, variance, and MSE of predictions at each X-combo
mean.pred <- apply(save.pred, MARGIN=2, FUN=mean)
bias <- mean.pred - rep(mu, times=3)
var <- apply(save.pred, MARGIN=2, FUN=var)
MSE <- bias^2 + var

# Vector of model numbers
model <- rep(c(1,2,3), each=nrow(test))

# Plots
quartz(height=5, width=5, pointsize=15)
stripchart(bias ~ model, method="jitter", jitter=.1, vertical=TRUE, pch=20)
abline(h=0, lty="solid")

quartz(height=5, width=5, pointsize=15)
stripchart(var ~ model, method="jitter", jitter=.1, vertical=TRUE, pch=20)

quartz(height=5, width=5, pointsize=15)
stripchart(MSE ~ model, method="jitter", jitter=.1, vertical=TRUE, pch=20)

# Summary statistics for variances and MSEs for prediction by model
mean(var[which(model==1)])
mean(var[which(model==2)])
mean(var[which(model==3)])
mean(MSE[which(model==1)])
mean(MSE[which(model==2)])
mean(MSE[which(model==3)])

# Mean of model-based estimates of variance 
apply(save.sig,MARGIN=2,FUN=mean) 


