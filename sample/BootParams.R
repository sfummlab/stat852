# Bootstrapping regression parameters.  Currently does not work. 

prostate <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Teaching\\890 Modern Applied Methods\\R\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(glmnet)
library(bootstrap)
 
# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

x <- as.matrix(cbind(prostate[,10],prostate[,c(2:9)]))
inds <- prostate[,1]

lasso.coef <- function(x,inds){
  assign(".inds", inds, envir=.GlobalEnv)
  coef(cv.glmnet(y=x[.inds,1], x= x[.inds,-1], family="gaussian"))
  remove(".inds", envir=.GlobalEnv)
  }

a <- boot(data=x, statistic=lasso.coef, stype="i", R=5)


