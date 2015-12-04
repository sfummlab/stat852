prostate <-  read.table("~/Downloads/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
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

# Store summary() so we can see BICs (not comparable across different data sets)
summ.1 <- summary(allsub1)
summ.2 <- summary(allsub2)

summ.1
summ.2

names(summ.1)
summ.1$bic
summ.2$bic

# Plot of results in a special form
quartz(h=7, w=10, pointsize=12)
par(mfrow=c(1,2))
plot(allsub1, main="All Subsets on half of Prostate data")
plot(allsub2, main="All Subsets on other half of Prostate data")

# Fitting the models in succession from smallest to largest.  
# Fit one-var model. then update to 2-var model.  Could keep going.
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 
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

mod1.3 <- update(mod1.2, .~. + age)
sMSE.3 <- summary(mod1.3)$sigma^2
BIC.3 <- extractAIC(mod1.3, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.3 <- predict(mod1.3, newdata=prostate[which(prostate$set==2),])
MSPE.3 <- mean((pred2.3 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.4 <- update(mod1.3, .~. + lbph)
sMSE.4 <- summary(mod1.4)$sigma^2
BIC.4 <- extractAIC(mod1.4, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.4 <- predict(mod1.4, newdata=prostate[which(prostate$set==2),])
MSPE.4 <- mean((pred2.4 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.5 <- update(mod1.4, .~. + svi)
sMSE.5 <- summary(mod1.4)$sigma^2
BIC.5 <- extractAIC(mod1.4, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.5 <- predict(mod1.4, newdata=prostate[which(prostate$set==2),])
MSPE.5 <- mean((pred2.4 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.6 <- update(mod1.5, .~. + lcp)
sMSE.6 <- summary(mod1.6)$sigma^2
BIC.6 <- extractAIC(mod1.6, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.6 <- predict(mod1.6, newdata=prostate[which(prostate$set==2),])
MSPE.6 <- mean((pred2.6 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.7 <- update(mod1.6, .~. + gleason)
sMSE.7 <- summary(mod1.7)$sigma^2
BIC.7 <- extractAIC(mod1.7, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.7 <- predict(mod1.7, newdata=prostate[which(prostate$set==2),])
MSPE.7 <- mean((pred2.7 - prostate[which(prostate$set==2),]$lpsa)^2)

mod1.8 <- update(mod1.7, .~. + pgg45)
sMSE.8 <- summary(mod1.2)$sigma^2
BIC.8 <- extractAIC(mod1.2, k=log(nrow(prostate[which(prostate$set==1),])))
pred2.8 <- predict(mod1.2, newdata=prostate[which(prostate$set==2),])
MSPE.8 <- mean((pred2.2 - prostate[which(prostate$set==2),]$lpsa)^2)
