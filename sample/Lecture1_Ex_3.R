library(leaps)

allsub <- regsubsets(y=mtcars$mpg, x=mtcars[,-1], nbest=5, nvmax=10)

allsub$ress
summary(allsub)

minbic <- min(summary(allsub)$bic)
within2 <- which(summary(allsub)$bic < minbic + 2)
summary(allsub)$which[within2,]

nvar <- row(as.matrix(allsub$ress)) - 1
bic <- log(allsub$ress/nrow(mtcars)) + nvar*log(nrow(mtcars))

Close <- ifelse(bic < 2+min(bic),yes=1,no=0)

# Plot of results in a special form
win.graph(h=7, w=6, pointsize=12)
plot(allsub, main="All Subsets on MPG, mtcars data")


# Fitting the models in succession from smallest to largest.  
# Fit one-var model. then update to 2-var model.  Could keep going.
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 
mod1.1 <- lm(data=mtcars, formula=mpg~wt + qsec + am)
sMSE.1 <- summary(mod1.1)$sigma^2
BIC.1 <- extractAIC(mod1.1, k=log(nrow(mtcars)))
summary(mod1.1)
