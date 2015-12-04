
phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")

phoneme$g <- as.factor(phoneme$g)

library(MASS)

set.seed(41891019)

nodes <- cbind(5,16,50,100,150,200)
#mino  <- cbind(5, 10, 50, 100)

iter=5
phoneme.oob <- matrix(NA,nrow = length(nodes),ncol=iter+1)
phoneme.test <- matrix(NA,nrow = length(nodes),ncol=iter+1)

for(i in 1:iter)
{
  
  rice <- runif(1,0,1)
  set.seed(rice * 10000000)
  phoneme$set <- ifelse(runif(n=nrow(phoneme))>0.75, yes=2, no=1)
  y.1 <- phoneme[which(phoneme$set==1),258]
  x.1 <- phoneme[which(phoneme$set==1),2:257]
  
  y.2 <- phoneme[which(phoneme$set==2),258]
  x.2 <- phoneme[which(phoneme$set==2),2:257]
  
  
  ii = 1
  for(noo in nodes)
   # for(moo in mino)
    {
      
      phoneme.oob[ii,1] <- noo
      phoneme.test[ii,1] <- noo
      
      phoneme.rf <- randomForest(x.1, y.1, importance=TRUE, ntree=1200, mtry=noo, keep.forest=TRUE)
      pred.randf <- predict(phoneme.rf, type="response")
      pred.randf.test <- predict(phoneme.rf, newdata = x.2, type="response")
      (phoneme.oob[ii,i+1]<- mean(ifelse(pred.randf == y.1, yes=0, no=1)))
      (phoneme.test[ii,i+1]<- mean(ifelse(pred.randf.test == y.2, yes=0, no=1)))
      ii <- ii + 1
      print("The noo mtry has finished!")
    }
  Mean_val <- rowMeans(phoneme.oob[,-1])
  best_index <- which.min(Mean_val)
  best_para <- phoneme.oob[best_index,1]
}


################## Max Node Size , Reduce size to smaller Tree ################
phoneme.rf.1  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 5 ,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.2  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 10,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.3  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 20,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.4  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 50,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.5  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 80,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.6  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 100,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)
phoneme.rf.7  <-  randomForest(data=data.frame(x.1,type = y.1), type~.,maxnodes = 200,mtry=16, importance=TRUE, ntree=1200, keep.forest=TRUE)



pred.rf.1 <- predict(phoneme.rf.1, type="response")
(misclass.oob.1 <- mean(ifelse(pred.rf.1 == y.1, yes=0, no=1)))
pred.rf.2 <- predict(phoneme.rf.2, type="response")
(misclass.oob.2 <- mean(ifelse(pred.rf.2 == y.1, yes=0, no=1)))
pred.rf.3 <- predict(phoneme.rf.3, type="response")
(misclass.oob.3 <- mean(ifelse(pred.rf.3 == y.1, yes=0, no=1)))
pred.rf.4 <- predict(phoneme.rf.4, type="response")
(misclass.oob.4 <- mean(ifelse(pred.rf.4 == y.1, yes=0, no=1)))
pred.rf.5 <- predict(phoneme.rf.5, type="response")
(misclass.oob.5 <- mean(ifelse(pred.rf.5 == y.1, yes=0, no=1)))
pred.rf.6 <- predict(phoneme.rf.6, type="response")
(misclass.oob.6 <- mean(ifelse(pred.rf.6 == y.1, yes=0, no=1)))

pred.rf.7 <- predict(phoneme.rf.7, type="response")
(misclass.oob.7 <- mean(ifelse(pred.rf.7 == y.1, yes=0, no=1)))

