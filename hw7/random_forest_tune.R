# Gradient Boosting using gbm 

library(randomForest)


abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")




abelone$Sex <- as.factor(abelone$Sex)

set.seed(41891019)



nodes <- cbind(1,2,3,4,5,6,7,8)
mino  <- cbind(5, 10 ,20)

iter=20
abelone.oob <- matrix(NA,nrow = length(nodes)*length(mino),ncol=iter+2)

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
            for(noo in nodes)
                for(moo in mino)
                {

                    abelone.oob[ii,1:2] <- c(noo,moo)

                    abalone.rf <- randomForest(x.1, y.1, importance=TRUE, ntree=600, mtry=noo,nodesize=moo, keep.forest=TRUE)
                    abelone.oob[ii,i+2] <- mean((y.1-abalone.rf$predicted)^2)
                    ii <- ii + 1

                }

   Mean_val <- rowMeans(abelone.oob[,-c(1,2)])
   best_index <- which.min(Mean_val)
   best_para <- abelone.oob[best_index,1:2]

}

siz.dec <- paste(abelone.oob[,1],abelone.oob[,2],abelone.oob[,3],abelone.oob[,4])

quartz(h=7,w=12,pointsize=12)
boxplot.matrix(x=sqrt(abelone.oob[,-c(1,2,3,4)]), use.cols=FALSE, names=siz.dec)





