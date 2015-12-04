phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")


kernels <- cbind("aa","ao","dcl", "iy", "sh")

gammao <- 10^(-3:0)
costa <- 10^(-3:1)
degrea <- cbind(1,2,3)
coefa <- cbind(0,1,5)
iter=5


phoneme.val <- matrix(NA,nrow = length(degrea)* length(coefa)* length(gammao) * length(costa),ncol=iter+4)
phoneme.train <- matrix(NA,nrow = length(degrea)* length(coefa)* length(gammao) * length(costa),ncol=iter+4)



library(e1071)

for(i in 1:iter)
{
  resamp <- sample.int(n=nrow(phoneme), size= nrow(phoneme), replace=TRUE)
  x.r <- phoneme[resamp,2:257]
  y.r <- phoneme[resamp,258]
  x.p <- phoneme[-unique(resamp),2:257]
  y.p <- phoneme[-unique(resamp),258]
  print("Start working!")
  
  ii = 1
  for(gammap in gammao)
      for(costo in costa)
          for(degreo in degrea)
              for(coefo in coefa)
              {
                  phoneme.val[ii,1:4] <- c(gammap,costo,degreo,coefo)
                  phoneme.train[ii,1:4] <- c(gammap,costo,degreo,coefo)

                  phoneme.svm <- svm(data=data.frame(x.r,type=y.r), type ~ ., kernel="polynomial", gamma=gammap, cost=costo, degree=degreo, coef0 = coefo)


                  pred.train <- predict(phoneme.svm, newdata=x.r)
                  phoneme.train[ii,i+4] <- mean(ifelse(pred.train == y.r, yes=0, no=1))

                  pred.val <- predict(phoneme.svm, newdata=x.p)
                  phoneme.val[ii,i+4]  <- mean(ifelse(pred.val == y.p, yes=0, no=1))

                  ii <- ii + 1

                  print("iteration finished once!")
              }

  print("finish one loop!")
  Mean_val <- rowMeans(phoneme.val[,-c(1,4)])
  best_index <- which.min(Mean_val)
  best_para <- phoneme.val[best_index,1:4]
  write.csv(phoneme.val,"~/stat852/hw10/svm_val.csv")
  write.csv(phoneme.train,"~/stat852/hw10/svm_train.csv")
}

