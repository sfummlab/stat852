#vehdata <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\R\\vehicle3.txt",header=TRUE,sep=" ")
vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

library(sm)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]


# Can only handle binary, so classifying Bus (class 3) versus No Bus 
pair.1 <- ifelse(set1$class==3, y=1, n=0)
pair.2 <- ifelse(set2$class==3, y=1, n=0)
pair.3 <- ifelse(set3$class==3, y=1, n=0)



pair <- ifelse(set1$class==3, y=1, n=0)

pair <- ifelse(set1$class==3, y=1, n=0)


pair <- ifelse(set1$class==3, y=1, n=0)

###############################################################
## Smooth binomial regression using sm.binomial(): Local likelihood.
## Can handle only one explanatory at a time.  
##   I've arbitrarily chosen the first.
## h= is a smoothing parameter.  Play with it until you're happy.
## Output is a graph.
###############################################################

quartz(h=12,w=12,pointsize=12)
par(mfrow=c(3,2))
sm.binomial(x=set1[,1], y=pair.1, h=0.5, xlab="h=0.5")
sm.binomial(x=set1[,1], y=pair.1, h=2, xlab="h=2")
sm.binomial(x=set1[,1], y=pair.1, h=4, xlab="h=4")
sm.binomial(x=set1[,1], y=pair.1, h=6, xlab="h=6")
sm.binomial(x=set1[,1], y=pair.1, h=8, xlab="h=8")
sm.binomial(x=set1[,1], y=pair.1, h=1000, xlab="h=1000")

# Checking the linear predictor: How non-linear is it?
quartz(h=7,w=6)
a <- sm.binomial(x=set1[,1], y=pair.1, h=4,eval.points = set1[,1])
plot(y=a$linear.predictor, x=a$eval.points)

###############################################################
# Alternative: Binomial GAM!
###############################################################

library(mgcv)

#  Generalized additive model as alternative to multivariate splines
# One variable
gam1 <- gam(pair.1~s(set1$Compactness), family=binomial(link=logit),data = set1) 
summary(gam1)

# Plots of results
quartz(h=8,w=7,pointsize=12)
plot(gam1, main="Generalized Additive Model marginal splines")

# 4 variables
gam4 <- gam(pair.1~s(set1$Compactness) + s(set1$Circularity) + 
              s(set1$Distance.Circularity) + s(set1$Radius.Ratio), 
          family=binomial(link=logit),data=set1) 
summary(gam4)

# Plot marginals
x11(h=12,w=12,pointsize=12)
par(mfrow=c(2,2))
plot(gam4, main="Generalized Additive Model marginal splines")

x11(h=12,w=12,pointsize=12)
par(mfrow=c(2,2))
plot(gam4, main="Generalized Additive Model marginal splines", se=FALSE)

pred.prob4.1 <- predict(gam4, newdata=set1[,1:4], type="response")
pred.class4.1 <- as.numeric(predict(gam4, newdata=set1[,1:4], type="link") > 0)
head(cbind(round(pred.prob4.1, digits=3), pred.class4.1))

pred.prob4.2 <- predict(gam4, newdata=set2[,1:4], type="response")
pred.class4.2 <- as.numeric(predict(gam4, newdata=set2[,1:4], type="link") > 0)

pred.prob4.3 <- predict(gam4, newdata=set3[,1:4], type="response")
pred.class4.3 <- as.numeric(predict(gam4, newdata=set3[,1:4], type="link") > 0)

(misclass4.train <- mean(ifelse(pred.class4.1 == pair, yes=0, no=1)))
(misclass4.valid <- mean(ifelse(pred.class4.2 == pair, yes=0, no=1)))
(misclass4.test <- mean(ifelse(pred.class4.3 == pair, yes=0, no=1)))


# This overfits---probably a complete separation
gam9 <- gam(pair~s(set1$Compactness) + s(set1$Circularity) 
            + s(set1$Distance.Circularity) + s(set1$Radius.Ratio) 
            + s(set1$Pr.Axis.Aspect.Ratio) + s(set1$Max.Length.Aspect.Ratio) 
            + s(set1$Scatter.Ratio) + s(set1$Elongatedness) 
            + s(set1$Pr.Axis.Rectangularity), family=binomial(link=logit)) 
summary(gam9)

x11(h=12,w=12,pointsize=12)
par(mfrow=c(3,3))
plot(gam9, main="Generalized Additive Model marginal splines")

x11(h=12,w=12,pointsize=12)
par(mfrow=c(3,3))
plot(gam9, main="Generalized Additive Model marginal splines", se=FALSE, scale=0)


pred.prob9.1 <- predict(gam9, newdata=set1[,1:9], type="response")
pred.class9.1 <- as.numeric(predict(gam9, newdata=set1[,1:9], type="link") > 0)
head(cbind(round(pred.prob9.1, digits=3), pred.class9.1))

pred.prob9.2 <- predict(gam9, newdata=set2[,1:9], type="response")
pred.class9.2 <- as.numeric(predict(gam9, newdata=set2[,1:9], type="link") > 0)

pred.prob9.3 <- predict(gam9, newdata=set3[,1:9], type="response")
pred.class9.3 <- as.numeric(predict(gam9, newdata=set3[,1:9], type="link") > 0)

# Training error is zero, but test errors are pretty good.  
(misclass9.train <- mean(ifelse(pred.class9.1 == pair, yes=0, no=1)))
(misclass9.valid <- mean(ifelse(pred.class9.2 == pair, yes=0, no=1)))
(misclass9.test <- mean(ifelse(pred.class9.3 == pair, yes=0, no=1)))


