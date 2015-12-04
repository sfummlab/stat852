prostate <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Teaching\\890 Modern Applied Methods\\R\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)
# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub1 <- regsubsets(x=prostate[which(prostate$set==1),2:9], 
                      y=prostate[which(prostate$set==1),10], nbest=1)
allsub2 <- regsubsets(x=prostate[which(prostate$set==2),2:9], 
                      y=prostate[which(prostate$set==2),10], nbest=1)
summary(allsub1)
summary(allsub2)

# Plot of results in a special form
win.graph(h=7, w=10, pointsize=12)
par(mfrow=c(1,2))
plot(allsub1, main="All Subsets on half of Prostate data")
plot(allsub2, main="All Subsets on other half of Prostate data")

# Fitting the models in succession from smallest to largest.  
# Fit one-var model. then update to 2-var model.  Could keep going.
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 
mod1.0 <- lm(data=prostate[which(prostate$set==1),], formula=lpsa~1)
sMSE.0 <- summary(mod1.0)$sigma^2
BIC.0 <- extractAIC(mod1.0, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.0 <- predict(mod1.0, newdata=prostate[which(prostate$set==2),])
MSPE.0 <- mean((pred2.0-prostate[which(prostate$set==2),]$lpsa)^2)

mod1.1 <- lm(data=prostate[which(prostate$set==1),], formula=lpsa~lcavol)
sMSE.1 <- summary(mod1.1)$sigma^2
BIC.1 <- extractAIC(mod1.1, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.1 <- predict(mod1.1, newdata=prostate[which(prostate$set==2),])
MSPE.1 <- mean((pred2.1-prostate[which(prostate$set==2),]$lpsa)^2)

mod1.2 <- update(mod1.1, .~. + lweight)
sMSE.2 <- summary(mod1.2)$sigma^2
BIC.2 <- extractAIC(mod1.2, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.2 <- predict(mod1.2, newdata=prostate[which(prostate$set==2),])
MSPE.2 <- mean((pred2.2 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.3 <- update(mod1.2, .~. + svi)
sMSE.3 <- summary(mod1.3)$sigma^2
BIC.3 <- extractAIC(mod1.3, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.3 <- predict(mod1.3, newdata=prostate[which(prostate$set==2),])
MSPE.3 <- mean((pred2.3 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.4 <- update(mod1.3, .~. + gleason)
sMSE.4 <- summary(mod1.4)$sigma^2
BIC.4 <- extractAIC(mod1.4, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.4 <- predict(mod1.4, newdata=prostate[which(prostate$set==2),])
MSPE.4 <- mean((pred2.4 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.5 <- update(mod1.4, .~. + lcp)
sMSE.5 <- summary(mod1.5)$sigma^2
BIC.5 <- extractAIC(mod1.5, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.5 <- predict(mod1.5, newdata=prostate[which(prostate$set==2),])
MSPE.5 <- mean((pred2.5 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.6 <- update(mod1.5, .~. + age)
sMSE.6 <- summary(mod1.6)$sigma^2
BIC.6 <- extractAIC(mod1.6, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.6 <- predict(mod1.6, newdata=prostate[which(prostate$set==2),])
MSPE.6 <- mean((pred2.6 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.7 <- update(mod1.6, .~. + pgg45)
sMSE.7 <- summary(mod1.7)$sigma^2
BIC.7 <- extractAIC(mod1.7, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.7 <- predict(mod1.7, newdata=prostate[which(prostate$set==2),])
MSPE.7 <- mean((pred2.7 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.8 <- update(mod1.7, .~. + lbph)
sMSE.8 <- summary(mod1.8)$sigma^2
BIC.8 <- extractAIC(mod1.8, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.8 <- predict(mod1.8, newdata=prostate[which(prostate$set==2),])
MSPE.8 <- mean((pred2.8 - prostate[which(prostate$set==2),]$lpsa)^2)

sMSE <- c(sMSE.0,sMSE.1,sMSE.2,sMSE.3,sMSE.4,sMSE.5,sMSE.6,sMSE.7,sMSE.8)
MSPE <- c(MSPE.0,MSPE.1,MSPE.2,MSPE.3,MSPE.4,MSPE.5,MSPE.6,MSPE.7,MSPE.8)
vars <- c(0:8)

win.graph(h=6,w=7,pointsize=12)

plot(x=vars,y=sMSE,type="b", lty="solid",col="blue",ylim=c(0,1.6))
lines(x=vars,y=MSPE,type="b", lty="solid",col="orange")
legend(x=5, y=1.5, legend=c("Training error","Test error"), col=c("blue","orange"),lty="solid")
