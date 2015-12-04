require(xgboost)

pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)


pro$X9.1 <- pro$X9 * pro$X1
pro$X9.2 <- pro$X9 * pro$X2
pro$X9.3 <- pro$X9 * pro$X3
pro$X9.4 <- pro$X9 * pro$X4
pro$X9.5 <- pro$X9 * pro$X5
pro$X9.6 <- pro$X9 * pro$X6
pro$X9.7 <- pro$X9 * pro$X7
pro$X9.8 <- pro$X9 * pro$X8
pro$X9.10<- pro$X9 * pro$X10
pro$X9.11 <- pro$X9 * pro$X11
pro$X9.12<- pro$X9 * pro$X12
pro$X9.13 <- pro$X9 * pro$X13
pro$X9.14 <- pro$X9 * pro$X14
pro$X9.15 <- pro$X9 * pro$X15
pro$X9.16 <- pro$X9 * pro$X16
pro$X9.17 <- pro$X9 * pro$X17
pro$X9.18 <- pro$X9 * pro$X18
pro$X9.19 <- pro$X9 * pro$X19
pro$X9.20 <- pro$X9 * pro$X20

pro$X21.1 <- pro$X21 * pro$X1
pro$X21.2 <- pro$X21 * pro$X2
pro$X21.3 <- pro$X21 * pro$X3
pro$X21.4 <- pro$X21 * pro$X4
pro$X21.5 <- pro$X21 * pro$X5
pro$X21.6 <- pro$X21 * pro$X6
pro$X21.7 <- pro$X21 * pro$X7
pro$X21.8 <- pro$X21 * pro$X8
pro$X21.10 <- pro$X21 * pro$X10
pro$X21.11 <- pro$X21 * pro$X11
pro$X21.12 <- pro$X21* pro$X12
pro$X21.13 <- pro$X21 * pro$X13
pro$X21.14 <- pro$X21 * pro$X14
pro$X21.15 <- pro$X21 * pro$X15
pro$X21.16 <- pro$X21 * pro$X16
pro$X21.17 <- pro$X21 * pro$X17
pro$X21.18 <- pro$X21 * pro$X18
pro$X21.19 <- pro$X21 * pro$X19
pro$X21.20 <- pro$X21 * pro$X20




pro$logX1  <- log(pro$X1 )
pro$logX2  <- log(pro$X2 )
pro$logX3  <- log(pro$X3 )
pro$logX4  <- log(pro$X4 )
pro$logX5  <- log(pro$X5 )
pro$logX6  <- log(pro$X6 )
pro$logX7  <- log(pro$X7 )
pro$logX8  <- log(pro$X8 )
pro$logX10 <- log(pro$X10)
pro$logX11 <- log(pro$X11)
#pro$logX12 <- log(pro$X12)
pro$logX13 <- log(pro$X13)
pro$logX14 <- log(pro$X14)
pro$logX15 <- log(pro$X15)
pro$logX16 <- log(pro$X16)
pro$logX17 <- log(pro$X17)
pro$logX18 <- log(pro$X18)
#pro$logX19 <- log(pro$X19)
pro$logX20 <- log(pro$X20)


rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


require(xgboost)



x.3.scaled <- rescale(x.3, x.3)
x.4.scaled <- rescale(x.4, x.3)



num<- cbind(0.3,0.5,0.8,1.0)
Tree.siz <- cbind(1,3,5,7)
Shrink <- cbind(0.001, 0.01, 0.1)
sample.ratio <- cbind(0.25,0.5,0.75)

iter=5
pro.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

for(i in 1:iter)
{
  
    rice <- runif(1,0,1)
        set.seed(rice * 10000000) 
        pro$set <- ifelse(runif(n=nrow(pro))>1, yes=2, no=1)


        x.1 <- as.matrix(pro[which(pro$set==1),-c(22,78)])
        y.1 <- as.matrix(pro[which(pro$set==1),22])
        x.2 <- as.matrix(pro[which(pro$set==2),-c(22,78)])
        y.2 <- as.matrix(pro[which(pro$set==2),22])

        x.3 <- as.matrix(pro[which(pro$set==1),1:21])
        y.3 <- as.matrix(pro[which(pro$set==1),22])
        x.4 <- as.matrix(pro[which(pro$set==2),1:21])
        y.4 <- as.matrix(pro[which(pro$set==2),22])



  ii = 1
  for(rnd in num)
  for(siz in Tree.siz)
    for(sh in Shrink)
      for(rati in sample.ratio)
      {
        dtrain <- xgb.DMatrix(x.3,label = y.3)
        pro.boost <- xgb.cv(data = dtrain, nfold = 10,  max.depth = siz, nround = rnd * 1000, subsample = rati, objctive = "reg:linear", metrics = list("rmse"))
       

        pro.val[ii,1:4] <- c(siz,sh,rati,rnd *10000)
        
        
        pro.val[ii,i+4] <- min(pro.boost$test.rmse.mean)

        ii <- ii + 1
      }
  
  Mean_val <- rowMeans(pro.val[,-c(1,2,3,4)])
  best_index <- which.min(Mean_val)
  best_para <- pro.val[best_index,1:4]
}

  jj = 1

for(j in 1:4)
{  
  mm <- jj + 26
  siz.dec <- paste(pro.val[jj:mm,1],pro.val[jj:mm,2],pro.val[jj:mm,3],pro.val[jj:mm,4])
  x11(h=7,w=12,pointsize=12)
  boxplot.matrix(x=sqrt(pro.val[jj:mm,-c(1,2,3,4)]), use.cols=FALSE, names=siz.dec)
  jj=jj+27
}




