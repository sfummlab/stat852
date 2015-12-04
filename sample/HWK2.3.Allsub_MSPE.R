# LASSO on prostate data using glmnet package 
#  (THERE IS ANOTHER PACKAGE THAT DOES LASSO.  WE WILL SEE IT LATER)
# Splitting the data in half and modeling each half separately.

prostate <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(glmnet)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub1 <- regsubsets(x=prostate[which(prostate$set==1),2:9], 
                      y=prostate[which(prostate$set==1),10], nbest=1)
allsub2 <- regsubsets(x=prostate[which(prostate$set==2),2:9], 
                      y=prostate[which(prostate$set==2),10], nbest=1)
summary(allsub1)
summary(allsub2)

minbic1 <- which.min(summary(allsub1)$bic)
summary(allsub1)$which[minbic1,]

mod1 <- lm(data=prostate[which(prostate$set==1),], formula=lpsa~lcavol + lweight + svi)
sMSE1 <- summary(mod1)$sigma^2
pred1.2 <- predict(mod1, newdata=prostate[which(prostate$set==2),])
MSPE.1.2 <- mean((pred1.2-prostate[which(prostate$set==2),]$lpsa)^2)


minbic2 <- which.min(summary(allsub2)$bic)
summary(allsub2)$which[minbic2,]

mod2 <- lm(data=prostate[which(prostate$set==2),], formula=lpsa~lcavol + age + lbph + svi)
sMSE2 <- summary(mod2)$sigma^2
pred2.1 <- predict(mod2, newdata=prostate[which(prostate$set==1),])
MSPE.2.1 <- mean((pred2.1-prostate[which(prostate$set==1),]$lpsa)^2)

sMSE1; MSPE.1.2; sMSE2; MSPE.2.1


# Repeat, using ridge to estimate coefficients instead
library(MASS)

ridge1 <- lm.ridge(data=prostate[which(prostate$set==1),], formula=lpsa~lcavol + lweight + svi, 
                   lambda = seq(0, 6, .02))
plot(ridge1)
select(ridge1)
ridge1 <- lm.ridge(data=prostate[which(prostate$set==1),], formula=lpsa~lcavol + lweight + svi, 
                   lambda = 5)

pred.ridge.1.1 <- coef(ridge1)[1] + coef(ridge1)[2]*x.1[,1] + 
  coef(ridge1)[3]*x.1[,2] + coef(ridge1)[4]*x.1[,5]
MSPE.ridge.1.1 <- mean((pred.ridge.1.1 - y.1)^2)

pred.ridge.1.2 <- coef(ridge1)[1] + coef(ridge1)[2]*x.2[,1] + 
  coef(ridge1)[3]*x.2[,2] + coef(ridge1)[4]*x.2[,5]
MSPE.ridge.1.2 <- mean((pred.ridge.1.2 - y.2)^2)


ridge2 <- lm.ridge(data=prostate[which(prostate$set==2),], formula=lpsa~lcavol + age + lbph + svi, 
                   lambda = seq(0, 10, .01))
plot(ridge1)
select(ridge1)
ridge2 <- lm.ridge(data=prostate[which(prostate$set==2),], formula=lpsa~lcavol + age + lbph + svi, 
                   lambda = 5)

pred.ridge.2.2 <- coef(ridge2)[1] + coef(ridge2)[2]*x.2[,1] + 
  coef(ridge2)[3]*x.2[,3] + coef(ridge2)[4]*x.2[,4] + coef(ridge2)[5]*x.2[,5]
MSPE.ridge.2.2 <- mean((pred.ridge.2.2 - y.2)^2)

pred.ridge.2.1 <- coef(ridge2)[1] + coef(ridge2)[2]*x.1[,1] + 
  coef(ridge2)[3]*x.1[,3] + coef(ridge2)[4]*x.1[,4] + coef(ridge2)[5]*x.1[,5]
MSPE.ridge.2.1 <- mean((pred.ridge.2.1 - y.1)^2)





