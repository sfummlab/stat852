wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")


wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

rescale.set1 <- function(x1,x2){
  minx <- apply(X=x1, MARGIN=2, FUN=min)
  maxx <- apply(X=x1, MARGIN=2, FUN=max)
  x3 <- matrix (nrow=nrow(x2), ncol=ncol(x2))
  for(i in c(1:ncol(x2))){
    x3[,i] <- (x2[,i] - minx[i])/(maxx[i] - minx[i])
  }
  x3
}


kernels <- cbind("Healthy","Scab", "Sprout")

nets=10 # Number of nnets per setting.  Could reduce to just a few
siz <- c(1,2,4,6)
dec <- c(0,0.001,0.01,0.1,1)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)
MSE <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)

MSPE.nn <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)
MSPE.nn.re <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)

Best_index <- matrix(NA, nrow=20, ncol=3)


library(nnet)
for(i in 1:20)
{
  #-----------------Randomize on Data Selection-------------#
  
  resamp <- sample.int(n=nrow(set1), size= nrow(set1), replace=TRUE)
  x.1.unscaled <- set1[resamp,1:6]
  x.1 <- rescale.set1(x.1.unscaled,x.1.unscaled)
  y.1 <- class.ind(set1[resamp,7])
  y.r <-set1[resamp,7]
  
  x.2.unscaled <- set1[-unique(resamp),1:6]
  x.2 <- rescale.set1(x.1.unscaled,x.2.unscaled)
  y.2 <- class.ind(set1[-unique(resamp),7])
  y.p <-set1[-unique(resamp),7]
  
  qq = 1
  
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      MSPE.nn[qq,1:2] <- c(s,d)
      MSPE.nn.re[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(iner_iter in 1:nets){
        nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE,softmax=TRUE)
        
        pred.nn <- predict(nn, newdata=x.1, type="class")
        misclass.nn <- mean(ifelse(pred.nn == y.r, yes=0, no=1))
        MS <- misclass.nn/nrow(x.1)
        if(MS < MSEmin){
          MSEmin <- MS
          p.min <- predict(nn, newdata=x.2,type="class")
        }
      }
      # Save results in new column of matrix
      
      MSPR[qq, 3] <- mean(ifelse(p.min == y.p, yes=0, no=1))
      MSE[qq, 3]  <- MSEmin
      
      nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE,softmax=TRUE)
      
      pred.nn <- predict(nn,newdata = x.2,type="class")
      
      MSPE.nn[qq,i+2] <-  mean(ifelse(pred.nn == y.p, yes=0, no=1))
      qq = qq + 1
    }
  }
  
  
  
  
  Best_index[i,] <- MSPR[which.min(MSPR[,3]),1:3]
  
  
  
  Best_MSPR <- MSPE.nn[which.min((MSPR[,3])),i+2]
  
  
  MSPE.nn.re[,i+2] <- MSPE.nn[,i+2] / Best_MSPR
  
  
}






root_MSPE <- sqrt(MSPE.nn)

root_MSPE.nn <- sqrt(MSPE.nn.re)


siz.dec <- paste(MSPE.nn[,1],MSPE.nn[,2])
quartz(pointsize=6)
boxplot.matrix(x=sqrt(MSPE.nn[,-c(1,2)]), use.cols=FALSE, names=siz.dec)


quartz(pointsize=6)
boxplot.matrix(x=sqrt(MSPE.nn.re[,-c(1,2)]), use.cols=FALSE, names=siz.dec)


quartz(h=10, w=12, pointsize=11)
boxplot(root_MSPE[,-c(1,2)])
quartz(h=10, w=12, pointsize=11)
boxplot(root_MSPE.nn[,-c(1,2)])
