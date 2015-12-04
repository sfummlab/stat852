###########################################################################
# Demonstration of the error-chasing bias that occurs when variables are 
#  selected using the data and MSE is subsequently calculated on the same 
#  data.
###########################################################################
# Generating independent normal observations, n=20, p=19, sigma=6.

set.seed(392039113)
x <- 200 + 6*matrix(data=rnorm(n=400),nrow=20)

# lm() Requires a data.frame to fit regression
xd <- as.data.frame(x)

# Initialize a storage object
sig.hat <- data.frame(vars=c(1:20),sig=rep(0,20))

# Fit the empty model and store results
null.fit <- lm(formula=V1 ~ 1, data=xd)
sig.hat[1,] <- c(1, summary(null.fit)$sigma)

# Successively add the columns of X to the regression *randomly*
#  (since data are generated randolmy already, I add them in sequence)

for(j in c(2:20)){
  fit <- summary(lm(formula=V1 ~ ., data=xd[,c(1:(j))]))
  sig.hat[j,] <- c(j,fit$sigma)
}

# Now do stepwise selection, adding them in order of improvement to the SSE

final.fit <- lm(formula=V1 ~ ., data=xd)

step.fit <- step(object = null.fit, scope=list(upper=final.fit), direction = "forward", k=0, trace=0)
step.anova <- step.fit$anova
sig.hat$step.sig <- sqrt(step.anova[,5]/step.anova[,4])

# Plot results

win.graph(height=5, width=8, pointsize=15)
plot(y=sig.hat$sig, x=sig.hat$vars, ylim=c(0,8), type="l", col="blue", 
     main="Demo of bias in MSE caused by variable selection")
lines(y=sig.hat$step.sig, x=sig.hat$vars, type="l", col="red")
abline(h=6,lty="dotted")




