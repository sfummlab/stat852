

prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)



library(leaps)

# Splitting data in half using random uniform selection to make two "set"s.


sMSE.sum.ols <- rep(0,20)
sMSE.sum.lasso <- rep(0,20)
sMSE.sum.ridge <- rep(0,20)
sMSE.sum.step <- rep(0,20)
sMSE.sum.allsub <- rep(0,20)


MSPE.sum.ols <- rep(0,20)
MSPE.sum.lasso <- rep(0,20)
MSPE.sum.ridge <- rep(0,20)
MSPE.sum.step <- rep(0,20)
MSPE.sum.allsub <- rep(0,20)

for(loo in 1:20)
{
  rice <- runif(1,0,1)
  set.seed(rice * 10000000) 
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.75, yes=2, no=1)
  
  
  
  
  y.1 <- prostate[which(prostate$set==1),10]
  x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
  y.2 <- prostate[which(prostate$set==2),10]
  x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
  
  
  
  
  
  # ----------------------OLS-------------------------#
  ols.fit <- function(x,y){
    lsfit(x,y)
  }
  ols.predict <- function(fit,x){
    cbind(1,x) %*% fit$coef
  }
  
  
  
  olson <- ols.fit(x.1,y.1)
  
  predict.ols.1 <- ols.predict(olson, x.1)
  
  predict.ols.2 <- ols.predict(olson, x.2)
  
  sMSE.ols<- mean((y.1 - predict.ols.1)^2)
  MSPE.ols <- mean((y.2 - predict.ols.2)^2)
  
  sMSE.sum.ols[loo] <-  sMSE.ols
 
  MSPE.sum.ols[loo] <-  MSPE.ols
  
  
  
  
  
  
  #------------------------LASSO--------------------#
  library(glmnet)
  #lasson <- cv.glmnet(x = x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 3, alpha=1)
  lasson <- glmnet(x = x.1, y= y.1, family="gaussian", alpha=1)
 # plot(lasson) # Plots coefficient path
#  coef(lasson) # Lists out coefficients for each lambda
  #predict.lasson.1 <- predict(lasson, newx=x.1, s = lasson$lambda.min)
  #predict.lasson.2 <- predict(lasson, newx=x.2, s = lasson$lambda.min)
  predict.lasson.1 <- predict(lasson, newx=x.1, s = lasson$lambda.1se)
  predict.lasson.2 <- predict(lasson, newx=x.2, s = lasson$lambda.1se)
  sMSE.lasson <- mean((y.1 - predict.lasson.1)^2)
  MSPE.lasson <- mean((y.2 - predict.lasson.2)^2)
  
  sMSE.sum.lasso[loo] <- sMSE.lasson
  
  MSPE.sum.lasso[loo] <- MSPE.lasson
  
  #---------------------RIDGE----------------------#
  library(glmnet)
  #ridged <- cv.glmnet(x=x.1, y= y.1, family="gaussian", type.measure="mse", nfolds = 3,alpha=0)
   ridged <- glmnet(x=x.1, y= y.1, family="gaussian",alpha=0)
  
 # plot(ridged) # Plots coefficient path
#  coef(ridged) # Lists out coefficients for each lambda
#  predict.ridged.1 <- predict(ridged, newx=x.1, s=ridged$lambda.min)
#  predict.ridged.2 <- predict(ridged, newx=x.2, s=ridged$lambda.min)

  predict.ridged.1 <- predict(ridged, newx=x.1)
  predict.ridged.2 <- predict(ridged, newx=x.2)
  sMSE.ridged <- mean((y.1 - predict.ridged.1)^2)
  MSPE.ridged <- mean((y.2 - predict.ridged.2)^2)
  
  sMSE.sum.ridge[loo] <-  sMSE.ridged

  MSPE.sum.ridge[loo] <-  MSPE.ridged



  #----------------All Subset BIC ---------------#
  #library(glmulti)
  #glmulti.lm.out <-
  #  glmulti(lpsa ~ lcavol + lweight + age  + lbph  + svi + lcp + gleason + pgg45,
  #          data =prostate[which(prostate$set==1),],
  #          level = 1,               # No interaction considered
  #         method = "h",            # Exhaustive approach
  #          crit = "bic",            # AIC as criteria
  #        confsetsize = 5,         # Keep 5 best models
    #        plotty = F, report = F,  # No plot or interim reports
    #        fitfunction = "lm")      # lm function
  
  ## Show 5 best models (Use @ instead of $ for an S4 object)
  #glmulti.lm.out@formulas
  
  
  #summary(glmulti.lm.out@objects[[1]])


  library(bestglm)



      prostate.for.bestglm <- within(prostate[which(prostate$set==1),], {
        ID   <- NULL        # Delete
        train <- NULL
        set <- NULL
        
        y    <- lpsa         # bwt into y
        lpsa  <- NULL        # Delete bwt
      })

## Reorder variables
prostate.for.bestglm <-
  prostate.for.bestglm[, c("lcavol","lweight","age","svi","lcp","gleason","pgg45","y")]


res.bestglm <-
  bestglm(Xy = prostate.for.bestglm,
          family = gaussian,
          IC = "BIC",                 # Information criteria for
          method = "exhaustive")
 
 formula(res.bestglm$BestModel)
  
  #allsubway <- lm(glmulti.lm.out@objects[[1]],data=prostate[which(prostate$set==1),])

  prostate_all <- prostate
  
   
  colnames(prostate_all)[10] <- "y"
  allsubway <- lm(formula(res.bestglm$BestModel),data=prostate_all[which(prostate_all$set==1),])
  
  predict.allsubway.1 <- predict(allsubway, as.data.frame(x.1))  

  predict.allsubway.2 <- predict(allsubway, as.data.frame(x.2))
  
  
  sMSE.allsubway <- mean((y.1 - predict.allsubway.1)^2)
  #sMSE.allsubway <- summary(allsubway)$sigma^2
  MSPE.allsubway <- mean((y.2 - predict.allsubway.2)^2)
  
  
  sMSE.sum.allsub[loo] <- sMSE.allsubway
  MSPE.sum.allsub[loo] <- MSPE.allsubway
  
  
  #--------------Step Wise BIC -----------------#
  
  
  
  step.fit <- function(x,y) {
    data=data.frame(y,x)
    initial.1 <- lm(data=data,formula=y~ 1)
    final.1 <- lm(data=data, formula=y~.)
    step1 <- step(object=initial.1, scope=list(upper=final.1), trace=0,
                  k = log(nrow(data)))
    step1
  }
  
  step.pred <- function(fit,x){
    predict(fit,as.data.frame(x))
  }
  
  
  stepon <- step.fit(x.1,y.1);
  predict.step.1 <- step.pred(stepon, x.1)
  predict.step.2 <- step.pred(stepon, x.2)
  
  
  
  sMSE.step <- mean((y.1 - predict.step.1)^2)
  MSPE.step <- mean((y.2 - predict.step.2)^2)
  
  sMSE.sum.step[loo] <- sMSE.step
  MSPE.sum.step[loo] <- MSPE.step
  
  
  
}
sMSE  <- data.frame(ols = sMSE.sum.ols, lasso = sMSE.sum.lasso, ridge = sMSE.sum.ridge,  allsub = sMSE.sum.allsub, step = sMSE.sum.step) 
MSPE  <- data.frame(ols = MSPE.sum.ols, lasso =MSPE.sum.lasso, ridge = MSPE.sum.ridge,  allsub = MSPE.sum.allsub, step = MSPE.sum.step) 

#sMSE$ols <-  sMSE.sum.ols
#sMSE$lasso <- sMSE.sum.lasso
#sMSE$ridge <- sMSE.sum.ridge
#sMSE$allsub <- sMSE.sum.allsub
#sMSE$step <- sMSE.sum.step


#MSPE$ols <- MSPE.sum.ols
#MSPE$lasso <- MSPE.sum.lasso
#MSPE$ridge <- MSPE.sum.ridge
#MSPE$allsub <- MSPE.sum.allsub
#MSPE$step <- MSPE.sum.step

boxplot(sMSE)
boxplot(MSPE)

sMSE.mat <- as.matrix(sMSE)

MSPE.mat <- as.matrix(MSPE)

ratio <- MSPE.mat / sMSE.mat

ratio.frame <- as.data.frame(ratio)

