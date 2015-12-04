prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)


column.de <- apply(prostate[,2:9],2,sd)

prostate[,2:9] <- scale(prostate[,2:9], center = TRUE, scale = column.de)

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])


set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
####################################################################
## One terms
####################################################################
#  Using default smoother, increasing quality of optimizer.
    ppr1 <- ppr(data=airquality, Ozone~scale(Wind)+scale(Temp), nterms=1, optlevel=3)
summary(ppr1)

# The term weights and the coefficients on the terms. 
    ppr1$alpha
    ppr1$beta

