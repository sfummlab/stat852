# Gradient Boosting using gbm 
library(gbm)
####################################################################
## Gradient boosting through gbm() allows different distributions to be used for boosting 
## Tuning parameters and defaults include 
# n.trees=100: number of trees (=M). Usually need to increase this
# interaction.depth = 1: is J-1. Default implies additive model.  
#    USUALLY need to increase this (HTF suggest 5)
# shrinkage = .001: The learning rate parameter "nu" 
#    (HTF: 0.1; author says .001 is better)
# bag.fraction=0.5: The subsampling fraction, eta (HTF: 0.5)
## Crossvalidation using cv.folds= allows you to estimate the number 
#    of trees for your current parameter settings to avoid overfitting
####################################################################


prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)


set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.7, yes=2, no=1)




num<- cbind(0.3,0.6,0.8,1.0)
Tree.siz <- cbind(1,3,5,7)
Shrink <- cbind(0.001, 0.01, 0.1)
sample.ratio <- cbind(0.5,0.75)

iter=10
prostate.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

for(i in 1:iter)
{

     rice <- runif(1,0,1)
     set.seed(rice * 10000000)
     prostate$set <- ifelse(runif(n=nrow(prostate))>0.7, yes=2, no=1)
     y.1 <- prostate[which(prostate$set==1),10]
     x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])

     y.2 <- prostate[which(prostate$set==2),10]
     x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

     resamp <- sample.int(n=nrow(x.1), size=0.75 * nrow(x.1), replace=FALSE)
     x.r <- x.1[resamp,-c(1,9)]
     y.r <- y.1[resamp]
     x.p <- x.1[-unique(resamp),-c(1,9)]
     y.p <- y.1[-unique(resamp)]



    ii = 1
        for(siz in Tree.siz)
            for(sh in Shrink)
                for(rati in sample.ratio)
                {
                    prostate.boost <- gbm(data=data.frame(x.r,lpsa=y.r), lpsa ~ ., distribution="gaussian", 
                                            n.trees=15000, interaction.depth=siz, shrinkage=sh,
                                            bag.fraction=rati, cv.folds=0,n.cores=8)

                    prostate.val[ii,1:4] <- c(siz,sh,rati,num[1]*10000)
                    prostate.val[ii+1,1:4] <- c(siz,sh,rati,num[2]*10000)
                    prostate.val[ii+2,1:4] <- c(siz,sh,rati,num[3]*10000)
                    prostate.val[ii+3,1:4] <- c(siz,sh,rati,num[4]*10000)


                    prostate.val[ii,i+4] <- mean((y.p- predict(prostate.boost,newdata=data.frame(x.p),n.trees=num[1]*10000))^2)
                    prostate.val[ii+1,i+4] <- mean((y.p- predict(prostate.boost,newdata=data.frame(x.p),n.trees=num[2]*10000))^2)
                    prostate.val[ii+2,i+4] <- mean((y.p- predict(prostate.boost,newdata=data.frame(x.p),n.trees=num[3]*10000))^2)
                    prostate.val[ii+3,i+4] <- mean((y.p- predict(prostate.boost,newdata=data.frame(x.p),n.trees=num[4]*10000))^2)
                    ii <- ii + 4
                }

   Mean_val <- rowMeans(prostate.val[,-c(1,2,3,4)])
   best_index <- which.min(Mean_val)
   best_para <- prostate.val[best_index,1:4]

}


siz.dec <- paste(prostate.val[,1],prostate.val[,2],prostate.val[,3],prostate.val[,4])
quartz(pointsize=6)
boxplot.matrix(x=sqrt(MSPE.nn[,-c(1,2)]), use.cols=FALSE, names=siz.dec)



prostate.boost.final <- gbm(data=data.frame(x.1,lpsa=y.1), lpsa ~ ., distribution="gaussian", 
                      n.trees=3000, interaction.depth=3, shrinkage=0.001,
                      bag.fraction=0.75, cv.folds=0,n.cores=8)

quartz(h=7,w=12,pointsize=12)
par(mfrow=c(2,4))

plot(prostate.boost.final, i.var=1)

plot(prostate.boost.final, i.var=2)

plot(prostate.boost.final, i.var=3)

plot(prostate.boost.final, i.var=4)

plot(prostate.boost.final, i.var=5)

plot(prostate.boost.final, i.var=6)

plot(prostate.boost.final, i.var=7)

plot(prostate.boost.final, i.var=8)


summary(prostate.boost.final,
        n.trees=3000,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE)

