phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")

gbm.miss.train <- rep(0,5)
nnet.miss.train <- rep(0,5)
svm.miss.train <- rep(0,5)
svmd.miss.train <- rep(0,5)
gbmd.miss.train <- rep(0,5)


gbm.miss.test <- rep(0,5)
nnet.miss.test <- rep(0,5)
svm.miss.test <- rep(0,5)

gbmd.miss.test <- rep(0,5)
svmd.miss.test <- rep(0,5)

gbm.miss.test.re <- rep(0,5)
nnet.miss.test.re <- rep(0,5)
svm.miss.test.re <- rep(0,5)
gbmd.miss.test.re <- rep(0,5)
svmd.miss.test.re <- rep(0,5)


set.seed(67982193)

kernels <- cbind("aa","ao","dcl", "iy", "sh")
rescale.set1 <- function(x1,x2){
  minx <- apply(X=x1, MARGIN=2, FUN=min)
  maxx <- apply(X=x1, MARGIN=2, FUN=max)
  x3 <- matrix (nrow=nrow(x2), ncol=ncol(x2))
  for(i in c(1:ncol(x2))){
    x3[,i] <- (x2[,i] - minx[i])/(maxx[i] - minx[i])
  }
  x3
}

library(MASS)
library(nnet)
library(gbm)
library(e1071)

resamp <-  read.table("~/stat852/hw9/resamp_max.csv", header=TRUE, sep=",", na.strings=" ")

for(i in 1:1)
{
    x.r  <- phoneme[resamp[,i+5],2:257]
    y.r  <- phoneme[resamp[,i+5],258]

    x.p  <- phoneme[-unique(resamp[,i+3]),2:257]
    y.p  <- phoneme[-unique(resamp[,i+3]),258]

    x.1 <- rescale.set1(x.r,x.r)
    y.1 <- class.ind(y.r)
    x.2 <- rescale.set1(x.r,x.p)
    y.2 <- class.ind(y.p)

    library(MASS)
    phoneme.boost <- gbm(data=data.frame(x.r,class=y.r), class ~., distribution="multinomial", 
                         n.trees=2000, interaction.depth=10,verbose=FALSE, shrinkage=0.01,
                         bag.fraction=0.75, cv.folds=0,n.cores=128)

    pred.boost.test <-  predict(phoneme.boost, newdata=x.p, n.trees=2000, type="response")
    pred.boost.train <-  predict(phoneme.boost, newdata=x.r, n.trees=2000, type="response")

    class.boost.test <- kernels[apply(pred.boost.test[,,1], 1, which.max)]
    class.boost.train <- kernels[apply(pred.boost.train[,,1], 1, which.max)]

    mspe.boost  <- mean(ifelse(class.boost.test  == y.p, yes=0, no=1))
    smse.boost  <- mean(ifelse(class.boost.train == y.r, yes=0, no=1))

    print("boost finish!")
    
    phoneme.boostd <- gbm(data=data.frame(x.r,class=y.r), class ~., distribution="multinomial", n.tree=2000,
                         n.cores=128)
    
    pred.boostd.test <-  predict(phoneme.boostd, newdata=x.p,  n.tree=2000,type="response")
    pred.boostd.train <-  predict(phoneme.boostd, newdata=x.r, n.tree=2000,type="response")
    
    class.boostd.test <- kernels[apply(pred.boostd.test[,,1], 1, which.max)]
    class.boostd.train <- kernels[apply(pred.boostd.train[,,1], 1, which.max)]
    
    mspe.boostd  <- mean(ifelse(class.boostd.test  == y.p, yes=0, no=1))
    smse.boostd  <- mean(ifelse(class.boostd.train == y.r, yes=0, no=1))
    
    
    phoneme.svm <- svm(data=data.frame(x.r,type=y.r), type ~ ., kernel="polynomial", gamma=0.01, cost=0.1, degree=2, coef0 = 5)
    
    
    pred.svm.train <- predict(phoneme.svm, newdata=x.r)
    smse.svm  <- mean(ifelse(pred.svm.train == y.r, yes=0, no=1))
    
    pred.svm.test <- predict(phoneme.svm, newdata=x.p)
    mspe.svm  <- mean(ifelse(pred.svm.test == y.p, yes=0, no=1))
    
    
    phoneme.svmd <- svm(data=data.frame(x.r,type=y.r), type ~ ., kernel="polynomial")
    
    
    pred.svmd.train <- predict(phoneme.svmd, newdata=x.r)
    smse.svmd  <- mean(ifelse(pred.svmd.train == y.r, yes=0, no=1))
    
    pred.svmd.test <- predict(phoneme.svmd, newdata=x.p)
    mspe.svmd  <- mean(ifelse(pred.svmd.test == y.p, yes=0, no=1))
    


    nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=3, decay=1, maxit=500, trace=FALSE,softmax=TRUE)

    pred.nn.test <- predict(nn,newdata = x.2,type="class")
    pred.nn.train <- predict(nn,newdata = x.1,type="class")

    smse.nn <-  mean(ifelse(pred.nn.train == y.r, yes=0, no=1))
    mspe.nn <-  mean(ifelse(pred.nn.test == y.p, yes=0, no=1))

    gbm.miss.train[i] <- smse.boost 
    nnet.miss.train[i] <- smse.nn
    svm.miss.train[i] <- smse.svm
    
    gbmd.miss.train[i] <- smse.boostd 

    svmd.miss.train[i] <- smse.svmd


    gbm.miss.test[i] <- mspe.boost
    nnet.miss.test[i] <- mspe.nn
    svm.miss.test[i] <- mspe.svm
    
    gbmd.miss.test[i] <- mspe.boostd

    svmd.miss.test[i] <- mspe.svmd
    
    print("one loop finish!")

}

train_2.mse <- cbind(gbm = gbm.miss.train, gbm_default = gbmd.miss.train, nnet = nnet.miss.train, svm = svm.miss.train,
                     svm_default = svmd.miss.train)

test_2.mse <- cbind(gbm = gbm.miss.test,  gbm_default=  gbmd.miss.test, nnet = nnet.miss.test, svm = svm.miss.test, 
                   svm_default = svmd.miss.test)

train_1.mse <- read.table("~/stat852/hw9/train_mse.csv", header=TRUE, sep=",", na.strings=" ")

test_1.mse <- read.table("~/stat852/hw9/test_mse.csv", header=TRUE, sep=",", na.strings=" ")

train_1.mse <- train_1.mse[,-1]
test_1.mse <- test_1.mse[,-1]

train.mse <- cbind.data.frame(train_1.mse,train_2.mse)

test.mse <- cbind.data.frame(test_1.mse,test_2.mse)

row_min <- apply(test.mse,1,min)

test.mse.re <- test.mse / row_min


write.csv(train.mse,"~/stat852/hw10/train_all.csv")

write.csv(test.mse,"~/stat852/hw10/test_all.csv")

ColumnMean <- data.frame(colMeans(train.mse),colMeans(test.mse),colMeans(test.mse.re))

quartz(h=7,w=12, title = "training error rate")

boxplot(train.mse)

quartz(h=7,w=12, title = "test error rate")

boxplot(test.mse)

quartz(h=7,w=12, title = "relative test error rate")

boxplot(test.mse.re,ylim=c(1,2.0))

train
gbm gbm_default       nnet        svm svm_default

0  0.08745247 0.05735108 0.05228137  0.07858048   
0  0.08586819 0.05069708 0.04626109  0.07984791   

0  0.08618504 0.05386565 0.04531052  0.07446134   


  0  0.08618504 0.05449937 0.05133080  0.07636248 
  0  0.08618504 0.05164766 0.04752852  0.07826362 

test
gbm gbm_default       nnet        svm svm_default


0.05912786   0.0849963 0.06725795 0.06208426  0.08425721 
0.07243163   0.1042129 0.08056171 0.06947524  0.08943089 
0.07686622   0.1019956 0.09386548 0.07464893    0.104952  
0.07760532  0.09386548 0.07464893 0.07095344  0.09977827 
0.07982262  0.09460458 0.08277901 0.07243163  0.10125647

