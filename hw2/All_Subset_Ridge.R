# LASSO on prostate data using glmnet package 
#  (THERE IS ANOTHER PACKAGE THAT DOES LASSO.  WE WILL SEE IT LATER)
# Splitting the data in half and modeling each half separately.

prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
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

# Fit LASSO by glmnet(y=, x=). Gaussian is default, but other families are available  
#  Function produces series of fits for many values of lambda.  



library(leaps)
allsub1 <- regsubsets(x.1, y.1, nbest=1)
allsub2 <- regsubsets(x.2, y.2, nbest=1)

# Store summary() so we can see BICs (not comparable across different data sets)
summ.1 <- summary(allsub1)
summ.2 <- summary(allsub2)

summ.1
summ.2

names(summ.1)
names(summ.2)
summ.1$bic
summ.2$bic


order.bic.summ1 <- order(summ.1$bic)
top.summ1<- summ.1$which[order.bic.summ1[c(1)],]


order.bic.summ2 <- order(summ.2$bic)
top.summ2<- summ.2$which[order.bic.summ2[c(1)],]

# Best Model based on BIC

library(MASS)

allreg.1 <- lm(formula =  lpsa ~ lcavol + lweight + svi,
               data = prostate[which(prostate$set == 1), ])

allridge.1 <- lm.ridge(lpsa ~ lcavol + lweight + svi, prostate[which(prostate$set==1),] ,lambda = seq(0, 10.0, .001))



allridge.final1 <- lm.ridge(lpsa ~ lcavol + lweight + svi, prostate[which(prostate$set==1),], lambda = 5.01)




allreg.2 <- lm(formula =  lpsa ~ lcavol + age + lbph + svi,
               data = prostate[which(prostate$set == 2), ])

allridge.2 <- lm.ridge(lpsa ~lcavol + age + lbph + svi, prostate[which(prostate$set==2),] ,lambda = seq(0, 10.0, .001))


allridge.final2 <- lm.ridge(lpsa ~ lcavol + age + lbph + svi, prostate[which(prostate$set==2),], lambda = 2.88)


allsumm.1 <- summary(allreg.1)

allsumm.2 <- summary(allreg.2)

ridgesumm.1 <- summary(allridge.1)

ridgesumm.2 <- summary(allridge.2)



coef.ridge1 <- coef(allridge.final1)
coef.ridge2 <- coef(allridge.final2)


#pred.final1.1 <- t( cbind(x.1[,1], x.1[,2], x.1[,4])  %*%  t(t(coef.ridge1[-1]))  + t(t(coef.ridge1[1] * rep(1,dim(x.1)[1]))) )

#pred.final1.2 <- t( cbind(x.2[,1], x.2[,2], x.2[,4])  %*%  t(t(coef.ridge2[-1]))  + t(t(coef.ridge2[1] * rep(1,dim(x.2)[1])))  )

pred.final1.1 <-  cbind(1,cbind(x.1[,1], x.1[,2], x.1[,5])) %*% coef.ridge1

pred.final2.2 <-  cbind(1,cbind(x.2[,1], x.2[,3], x.2[,4]), x.2[,5]) %*% coef.ridge2


pred.final1.2 <-  cbind(1,cbind(x.2[,1], x.2[,2], x.2[,5])) %*% coef.ridge1

pred.final2.1 <-  cbind(1,cbind(x.1[,1], x.1[,3], x.1[,4]), x.1[,5]) %*% coef.ridge2


sMSE.final1 <- mean((pred.final1.1-y.1)^2)


sMSE.final2 <- mean((pred.final2.2-y.2)^2)

MSPE.final1 <- mean((pred.final1.2-y.2)^2)

MSPE.final2 <- mean((pred.final2.1-y.1)^2)






sMSE.all.1 <- allsumm.1$sigma^2
pred.all.1 <- predict(allreg.1, newdata=prostate[which(prostate$set==2),2:9])
MSPE.all.1 <- mean((pred.all.1-y.2)^2)


sMSE.all.2 <- allsumm.2$sigma^2
pred.all.2 <- predict(allreg.2, newdata=prostate[which(prostate$set==1),2:9])
MSPE.all.2 <- mean((pred.all.2-y.1)^2)

