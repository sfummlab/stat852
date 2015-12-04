library(nnet)
abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)




set.seed(41891019)




rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}






nets=2 # Number of nnets per setting.  Could reduce to just a few
siz <- c(1,2,4,6)
dec <- c(0,0.001,0.01,0.1,1)
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MSPR <- matrix(NA, nrow=length(siz)*length(dec), ncol=3)
MSE <- matrix(NA, nrow=length(siz)*length(dec), ncol=3)

MSPE.nn <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)
MSPE.nn.re <- matrix(NA, nrow=length(siz)*length(dec), ncol=22)

Best_index <- matrix(NA, nrow=20, ncol=3)

for(i in 1:20)
{
  
  
  #-----------------Randomize on Data Selection-------------#
  rice <- runif(1,0,1)
  set.seed(rice * 10000000) 
  abelone$set <- ifelse(runif(n=nrow(abelone))>0.75, yes=2, no=1)
  
  
  x.1.unscaled <- as.matrix(abelone[which(abelone$set==1),-c(1,9,12)])
  y.1 <- as.matrix(abelone[which(abelone$set==1),9])
  x.2.unscaled <- as.matrix(abelone[which(abelone$set==2),-c(1,9,12)])
  y.2 <- as.matrix(abelone[which(abelone$set==2),9])
  
  
  x.1 <- rescale(x.1.unscaled, x.1.unscaled)
  x.2 <- rescale(x.2.unscaled, x.1.unscaled)
  
  resamp <- sample.int(n=nrow(x.1), size=0.75 * nrow(x.1), replace=FALSE)
  xr <- x.1[resamp,-c(1,9)]
  #xr <- rescale(x.1,x.1)
  yr <- y.1[resamp]
  xp <- x.1[-unique(resamp),-c(1,9)]
  #xp <- rescale(x.2, x.1)
  yp <- y.1[-unique(resamp)]

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
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR[qq, 3] <- mean((yp - p.min)^2)
      MSE[qq, 3] <- MSEmin
      
      
     
      nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
      
      pred.nn <- predict(nn,newdata = x.2)
      
      MSPE.nn[qq,i+2] <- mean((y.2 - pred.nn)^2)
      
      
      
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
