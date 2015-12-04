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
ppr1 <- ppr(data=prostate, lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, nterms=1, optlevel=3, sm.method="gcvspline")
summary(ppr1)

# The term weights and the coefficients on the terms. 
    ppr1$alpha
    ppr1$beta


ppr2 <- ppr(data=prostate, lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, nterms=2, optlevel=3, sm.method="gcvspline")
summary(ppr2)

# The term weights and the coefficients on the terms. 
ppr2$alpha
ppr2$beta


ppr3 <- ppr(data=prostate, lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, nterms=3, optlevel=3, sm.method="gcvspline")
summary(ppr3)

# The term weights and the coefficients on the terms. 
ppr3$alpha
ppr3$beta

