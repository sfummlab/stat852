# Gradient Boosting using gbm 
# Air Quality data

library(gbm)


abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")




abelone$Sex <- as.factor(abelone$Sex)

set.seed(41891019)



num<- cbind(0.3,0.5,0.8,1.0)
Tree.siz <- cbind(1,3,5,7)
Shrink <- cbind(0.001, 0.01, 0.1)
sample.ratio <- cbind(0.25,0.5,0.75)

iter=20
abelone.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

for(i in 1:iter)
{

     rice <- runif(1,0,1)
     set.seed(rice * 10000000)
     abelone$set <- ifelse(runif(n=nrow(abelone))>0.75, yes=2, no=1)
     y.1 <- abelone[which(abelone$set==1),9]
     x.1 <- abelone[which(abelone$set==1),-c(9,10)]

     y.2 <- abelone[which(abelone$set==2),9]
     x.2 <- abelone[which(abelone$set==2),-c(9,10)]

     resamp <- sample.int(n=nrow(x.1), size=0.66 * nrow(x.1), replace=FALSE)
     x.r <- x.1[resamp,]
     y.r <- y.1[resamp]
     x.p <- x.1[-unique(resamp),]
     y.p <- y.1[-unique(resamp)]



    ii = 1
        for(siz in Tree.siz)
            for(sh in Shrink)
                for(rati in sample.ratio)
                {
                    abelone.boost <- gbm(data=data.frame(x.r,Rings=y.r), Rings ~ ., distribution="gaussian", 
                                            n.trees=12000, interaction.depth=siz,verbose=FALSE, shrinkage=sh,
                                            bag.fraction=rati, cv.folds=0,n.cores=16)

                    abelone.val[ii,1:4] <- c(siz,sh,rati,num[1]*10000)
                    abelone.val[ii+1,1:4] <- c(siz,sh,rati,num[2]*10000)
                    abelone.val[ii+2,1:4] <- c(siz,sh,rati,num[3]*10000)
                    abelone.val[ii+3,1:4] <- c(siz,sh,rati,num[4]*10000)


                    abelone.val[ii,i+4] <- mean((y.p- predict(abelone.boost,newdata=data.frame(x.p),n.trees=num[1]*10000))^2)
                    abelone.val[ii+1,i+4] <- mean((y.p- predict(abelone.boost,newdata=data.frame(x.p),n.trees=num[2]*10000))^2)
                    abelone.val[ii+2,i+4] <- mean((y.p- predict(abelone.boost,newdata=data.frame(x.p),n.trees=num[3]*10000))^2)
                    abelone.val[ii+3,i+4] <- mean((y.p- predict(abelone.boost,newdata=data.frame(x.p),n.trees=num[4]*10000))^2)
                    ii <- ii + 4
                }

   Mean_val <- rowMeans(abelone.val[,-c(1,2,3,4)])
   best_index <- which.min(Mean_val)
   best_para <- abelone.val[best_index,1:4]

}



7.0e+00 1.0e-03 2.5e-01 8.0e+03

jj = 1

for(j in 1:4)
  {  
     mm <- jj + 26
     siz.dec <- paste(abelone.val[jj:mm,1],abelone.val[jj:mm,2],abelone.val[jj:mm,3],abelone.val[jj:mm,4])
     x11(h=7,w=12,pointsize=12)
     boxplot.matrix(x=sqrt(abelone.val[jj:mm,-c(1,2,3,4)]), use.cols=FALSE, names=siz.dec)
     jj=jj+27
  }


##### Best Parameters 5.0e+00 1.0e-03 2.5e-01 1.0e+04

gbm(data=data.frame(x.2,Rings=y.2), Rings ~ ., distribution="gaussian", 
    n.trees=10000, interaction.depth=5,verbose=FALSE, shrinkage=0.001,
    bag.fraction=0.25, cv.folds=0,n.cores=16)




