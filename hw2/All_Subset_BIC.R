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


allreg.1 <- lm(formula =  lpsa ~ lcavol + lweight + svi,
               data = prostate[which(prostate$set == 1), ])


allreg.2 <- lm(formula =  lpsa ~ lcavol + age + lbph + svi,
                data = prostate[which(prostate$set == 2), ])

allsumm.1 <- summary(allreg.1)

allsumm.2 <- summary(allreg.2)


#sMSE.all.1 <- allsumm.1$sigma^2


pred.all.1.1 <- predict(allreg.1, newdata=prostate[which(prostate$set==1),2:9])

sMSE.all.1 <- mean((pred.all.1.1 - y.1)^2)

pred.all.1.2 <- predict(allreg.1, newdata=prostate[which(prostate$set==2),2:9])

MSPE.all.1 <- mean((pred.all.1.2 -y.2)^2)




pred.all.2.2 <- predict(allreg.2, newdata=prostate[which(prostate$set==2),2:9])

sMSE.all.2 <- mean((pred.all.2.2 - y.2)^2)

pred.all.2.1 <- predict(allreg.2, newdata=prostate[which(prostate$set==1),2:9])


MSPE.all.2 <- mean((pred.all.2.1-y.1)^2)












