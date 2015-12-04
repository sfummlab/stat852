library(gbm)

wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")

wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]


kernels <- cbind("Healthy", "Scab", "Sprout")

num<- cbind(0.3,0.5,0.8,1.0)
Tree.siz <- cbind(1,5,10,20)
Shrink <- cbind(0.001, 0.01, 0.1)
sample.ratio <- cbind(0.25,0.5,0.75)

iter=10
wheat.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

for(i in 1:iter)
{
  resamp <- sample.int(n=nrow(set1), size =  nrow(set1), replace=TRUE)
  x.r <- set1[resamp,1:6]
  y.r <- set1[resamp,7]
  x.p <- set1[-unique(resamp),1:6]
  y.p <- set1[-unique(resamp),7]
  
  
  ii = 1
  for(siz in Tree.siz)
    for(sh in Shrink)
      for(rati in sample.ratio)
      {
        wheat.boost <- gbm(data=data.frame(x.r,class=y.r), class ~., distribution="multinomial", 
                           n.trees=5000, interaction.depth=siz,verbose=FALSE, shrinkage=sh,
                           bag.fraction=rati, cv.folds=0,n.cores=16)
        
        wheat.val[ii,1:4] <- c(siz,sh,rati,num[1]*10000)
        wheat.val[ii+1,1:4] <- c(siz,sh,rati,num[2]*10000)
        wheat.val[ii+2,1:4] <- c(siz,sh,rati,num[3]*10000)
        wheat.val[ii+3,1:4] <- c(siz,sh,rati,num[4]*10000)
        
        
        
        
        
        
        pred.mul.val.1  <-  predict(wheat.boost, newdata=x.p, n.trees=num[1]*5000, type="response")
        pred.mul.val.2  <-  predict(wheat.boost, newdata=x.p, n.trees=num[2]*5000, type="response")
        pred.mul.val.3  <-  predict(wheat.boost, newdata=x.p, n.trees=num[3]*5000, type="response")
        pred.mul.val.4  <-  predict(wheat.boost, newdata=x.p, n.trees=num[4]*5000, type="response")
        
        class.mul.val.1 <- kernels[apply(pred.mul.val.1[,,1], 1, which.max)]
        class.mul.val.2 <- kernels[apply(pred.mul.val.2[,,1], 1, which.max)]
        class.mul.val.3 <- kernels[apply(pred.mul.val.3[,,1], 1, which.max)]
        class.mul.val.4 <- kernels[apply(pred.mul.val.4[,,1], 1, which.max)]
        
        wheat.val[ii,i+4]     <- mean(ifelse(class.mul.val.1 == y.p, yes=0, no=1))
        wheat.val[ii+1,i+4]   <- mean(ifelse(class.mul.val.2 == y.p, yes=0, no=1))
        wheat.val[ii+2,i+4]   <- mean(ifelse(class.mul.val.3 == y.p, yes=0, no=1))
        wheat.val[ii+3,i+4]   <- mean(ifelse(class.mul.val.4 == y.p, yes=0, no=1))
        
        
        ii <- ii + 4
      }
  
  
  print("iteration finished once!")
  Mean_val <- rowMeans(wheat.val[,-c(1,2,3,4)])
  best_index <- which.min(Mean_val)
  best_para <- wheat.val[best_index,1:4]
}

write.csv(wheat.val,"~/stat852/hw10/wheat_gbm.csv")



# Best Parameter   5.0e+00 1.0e-03 2.5e-01 3.0e+03

