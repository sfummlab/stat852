
prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)


set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

df_bic <- matrix(0,10,2)

gam_df_bic <- matrix(0,10,2)


# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

library(mgcv)

bic_tra <- function(model,datain)
{
  bic_tra <- model$aic + (log(nrow(datain))-2) * sum(model$edf)
  bic_tra
}

#  Generalized additive model as alternative to multivariate splines
gam1 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi + gleason + s(age) + s(pgg45) + s(lbph), family=gaussian(link=identity)) 
summary(gam1)

df_bic[1,] <- extractAIC(gam1,k=log(nrow(x.1)))

xtable(summary(gam1)$p.table)
xtable(summary(gam1)$s.table)

gam2 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi + gleason + s(age) + s(lcp) + s(lbph), family=gaussian(link=identity)) 
summary(gam2)

df_bic[2,] <- extractAIC(gam2,k=log(nrow(x.1)))
gam_df_bic[2,] <- c(sum(gam2$edf), bic_tra(gam2,x.1))
xtable(summary(gam2)$p.table)
xtable(summary(gam2)$s.table)


gam3 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi + gleason + s(age) + s(lcp), family=gaussian(link=identity)) 
summary(gam3)

df_bic[3,] <- extractAIC(gam3,k=log(nrow(x.1)))
gam_df_bic[3,] <- c(sum(gam3$edf), bic_tra(gam3,x.1))
xtable(summary(gam3)$p.table)
xtable(summary(gam3)$s.table)


gam4 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi + gleason + s(age), family=gaussian(link=identity)) 
summary(gam4)

df_bic[4,] <- extractAIC(gam4,k=log(nrow(x.1)))
gam_df_bic[4,] <- c(sum(gam4$edf), bic_tra(gam4,x.1))
xtable(summary(gam4)$p.table)
xtable(summary(gam4)$s.table)



gam5 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi + gleason , family=gaussian(link=identity)) 
summary(gam5)

df_bic[5,] <- extractAIC(gam5,k=log(nrow(x.1)))
gam_df_bic[5,] <- c(sum(gam5$edf), bic_tra(gam5,x.1))
xtable(summary(gam5)$p.table)
xtable(summary(gam5)$s.table)


gam6 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) + svi , family=gaussian(link=identity)) 
summary(gam6)

df_bic[6,] <- extractAIC(gam6,k=log(nrow(x.1)))
gam_df_bic[6,] <- c(sum(gam6$edf), bic_tra(gam6,x.1))
xtable(summary(gam6)$p.table)
xtable(summary(gam6)$s.table)


gam7 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight) , family=gaussian(link=identity)) 
summary(gam7)

df_bic[7,] <- extractAIC(gam7,k=log(nrow(x.1)))
gam_df_bic[7,] <- c(sum(gam7$edf), bic_tra(gam7,x.1))
xtable(summary(gam7)$p.table)
xtable(summary(gam7)$s.table)


gam8 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol) , family=gaussian(link=identity)) 
summary(gam8)

df_bic[8,] <- extractAIC(gam8,k=log(nrow(x.1)))
gam_df_bic[8,] <- c(sum(gam8$edf), bic_tra(gam8,x.1))
xtable(summary(gam8)$p.table)
xtable(summary(gam8)$s.table)


gam9 <- gam(data=prostate[which(prostate$set==1),], lpsa~ lcavol , family=gaussian(link=identity)) 
summary(gam9)

df_bic[9,] <- extractAIC(gam9,k=log(nrow(x.1)))
gam_df_bic[9,] <- c(sum(gam9$edf), bic_tra(gam9,x.1))
xtable(summary(gam9)$p.table)
xtable(summary(gam9)$s.table)

sum(gam5$residuals^2)
pred <- predict(gam3,newdata=prostate[which(prostate$set==2),c(2:9)])