abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")

abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)

# Scatterplot matrix of all but nominal variable

#win.graph(h=12, w=12)
#pairs(x=abelone[,-1])

# Add interactions with sex and the log of each variable
abelone$sexm.len  <- abelone$sex.m * abelone$Length
abelone$sexm.diam <- abelone$sex.m * abelone$Diameter
abelone$sexm.hgt  <- abelone$sex.m * abelone$Height
abelone$sexm.whol <- abelone$sex.m * abelone$Whole
abelone$sexm.shuc <- abelone$sex.m * abelone$Shucked
abelone$sexm.vic  <- abelone$sex.m * abelone$Viscera
abelone$sexm.shel <- abelone$sex.m * abelone$Shell
abelone$sexf.len  <- abelone$sex.f * abelone$Length
abelone$sexf.diam <- abelone$sex.f * abelone$Diameter
abelone$sexf.hgt  <- abelone$sex.f * abelone$Height
abelone$sexf.whol <- abelone$sex.f * abelone$Whole
abelone$sexf.shuc <- abelone$sex.f * abelone$Shucked
abelone$sexf.vic  <- abelone$sex.f * abelone$Viscera
abelone$sexf.shel <- abelone$sex.f * abelone$Shell
abelone$loglen  <- log(abelone$Length)
abelone$logdiam <- log(abelone$Diameter)
abelone$loghgt <- log(abelone$Height)
abelone$logwhol<- log(abelone$Whole)
abelone$logshuc<- log(abelone$Shucked)
abelone$logvic <- log(abelone$Viscera)
abelone$logshel<- log(abelone$Shell)



#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test


abelone <- abelone[,-1] # Dump old Sex variable
abelone <- abelone[is.finite(rowSums(abelone)),]


abelone <- scale(abelone[,1:31])

head(abelone)

abelone <- as.data.frame(abelone)
set.seed(29003092)
U <- runif(n=nrow(abelone))
abelone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))

x.1 <- as.matrix(abelone[which(abelone$set==1),-c(8,32)])
y.1 <- as.matrix(abelone[which(abelone$set==1),8])
x.2 <- as.matrix(abelone[which(abelone$set==2),-c(8,32)])
y.2 <- as.matrix(abelone[which(abelone$set==2),8])
x.3 <- as.matrix(abelone[which(abelone$set==3),-c(8,32)])
y.3 <- as.matrix(abelone[which(abelone$set==3),8])



library(glmnet)
library(bootstrap)

#-----------------Bayesian Model Averaging--------------#

mod.avg <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 80)


coef.bma <-  summary(mod.avg)[,4]

coef.bma <- as.numeric(coef.bma)

coef.bma[is.na(coef.bma)] <- 0.0

coef.bma <- coef.bma[1:31]

#--------------------------OLS-----------------------------#
 ols.fit <- function(x,y){
  lsfit(x,y)
  }
ols.predict <- function(fit,x){
  cbind(1,x) %*% fit$coef
  }



  olson <- ols.fit(x.1,y.1)

  coef.ols <- coef(olson)







#-------------------------Ridge-----------------------------#

ridgec <- lm.ridge(y.1 ~ x.1, lambda = seq(0, 10, .01))
plot(ridgec)
select(ridgec)
coef.ridge <- as.numeric(coef(ridgec)[which.min(ridgec$GCV),])


#--------------------------LASSO--------------------------#

lasson <- cv.glmnet(x = x.1, y= y.1, family="gaussian", alpha=1)

coef.lasson <- as.numeric(coef(lasson, s = "lambda.min"))


coef.all <- data.frame(bma = coef.bma, ols = coef.ols, ridge = coef.ridge, lasso = coef.lasson)


library(xtable)

xtable(coef.all)