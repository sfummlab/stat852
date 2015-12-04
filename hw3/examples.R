

library(bestglm)



abelone.for.bestglm <- within(abelone[which(abelone$set==1),], {
  ID   <- NULL        # Delete
  train <- NULL
  set <- NULL
  
  y    <-  Rings       # bwt into y
  lpsa  <- NULL        # Delete bwt
})

## Reorder variables
abelone.for.bestglm <-
  abelone.for.bestglm[, c(colnames(abelone)[-c(8,32)],"y")]


res.bestglm <-
  bestglm(Xy = abelone.for.bestglm,
          family = gaussian,
          IC = "AIC",                 # Information criteria for
          method = "exhaustive")

formula(res.bestglm$BestModel)

#allsubway <- lm(glmulti.lm.out@objects[[1]],data=prostate[which(prostate$set==1),])

abelone_all <- abelone


colnames(abelone_all)[8] <- "y"
allsubway <- lm(formula(res.bestglm$BestModel),data=abelone_all[which(abelone_all$set==1),])

smse.aic <- predict(allsubway, as.data.frame(x.1))  

mspe.aic <- predict(allsubway, as.data.frame(x.2))







sMSE.allsub.aic <- mean((y.1 - smse.aic)^2)
MSPE.allsub.aic <- mean((y.2 - mspe.aic)^2)





# All subsets regression using the "regsubsets" function from "leaps"
# allsub.aic <- regsubsets(x=x.1[,-31], y=y.1, nbest=1, nvmax=30)
# order.aic <- order(summary(allsub.aic)$aic)
#  aic.cols <- which(summary(allsub.aic)$which[order.aic[1],]==TRUE)
#  aic.cols1 <- aic.cols[-1]-1
#  aic.x1 <- x.1[,aic.cols1]
#  aic.x2 <- x.2[,aic.cols1]
#  beta.aic <- coef(allsub.aic,id=order.aic[1])
#  smse.aic <- cbind(1,aic.x1)%*%beta.aic 
#  mspe.aic<- cbind(1, aic.x2)%*%beta.aic 

library(bestglm)



abelone.for.bestglm <- within(abelone[which(abelone$set==1),], {
  ID   <- NULL        # Delete
  train <- NULL
  set <- NULL
  
  y    <-  Rings       # bwt into y
  lpsa  <- NULL        # Delete bwt
})

## Reorder variables
abelone.for.bestglm <-
  abelone.for.bestglm[, c(colnames(abelone)[-c(8,32)],"y")]


res.bestglm <-
  bestglm(Xy = abelone.for.bestglm,
          family = gaussian,
          IC = "AIC",                 # Information criteria for
          method = "exhaustive")

formula(res.bestglm$BestModel)

#allsubway <- lm(glmulti.lm.out@objects[[1]],data=prostate[which(prostate$set==1),])

abelone_all <- abelone


colnames(abelone_all)[8] <- "y"
allsubway <- lm(formula(res.bestglm$BestModel),data=abelone_all[which(abelone_all$set==1),])

smse.aic <- predict(allsubway, as.data.frame(x.1))  

mspe.aic <- predict(allsubway, as.data.frame(x.2))







sMSE.allsub.aic <- mean((y.1 - smse.aic)^2)
MSPE.allsub.aic <- mean((y.2 - mspe.aic)^2)



