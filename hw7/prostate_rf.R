# Regression trees using rpart 
prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.7, yes=2, no=1)


y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])




lcavol.ord <- prostate[order(prostate[which(prostate$set==1),2]),2]
lweight.ord <- prostate[order(prostate[which(prostate$set==1),3]),3]
age.ord <- prostate[order(prostate[which(prostate$set==1),4]),4]
lbph.ord <- prostate[order(prostate[which(prostate$set==1),5]),5]
svi.ord <- prostate[order(prostate[which(prostate$set==1),6]),6]
lcp.ord <- prostate[order(prostate[which(prostate$set==1),7]),7]
gleason.ord <- prostate[order(prostate[which(prostate$set==1),8]),8]
pgg.ord <- prostate[order(prostate[which(prostate$set==1),9]),9]




rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}




x.1.scaled <- rescale(x.1, x.1)

#Prove that it worked
apply(X=x.1.scaled, MARGIN=2, FUN=min)
apply(X=x.1.scaled, MARGIN=2, FUN=max)
x.2.scaled <- rescale(x.2, x.1)
#Prove that it worked, but does not perfectly scale test set
apply(X=x.2.scaled, MARGIN=2, FUN=min)
apply(X=x.2.scaled, MARGIN=2, FUN=max)

library(randomForest)
############################## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
####################################################################


prostate.rf.1 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=1500, mtry=1, keep.forest=TRUE)

mm <-  ncol(x.1) / 3

prostate.rf.2 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=1000, mtry= mm , keep.forest=TRUE)

mm <- 2 * ncol(x.1) / 3

prostate.rf.3 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=1000, mtry= mm, keep.forest=TRUE)

prostate.rf.p <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                              importance=TRUE, ntree=1000, mtry= 8, keep.forest=TRUE)

quartz(h=7,w=12,pointsize=12,title = "Pre Selection of Tree Number")
par(mfrow=c(2,2))

plot(prostate.rf.1)
plot(prostate.rf.2)
plot(prostate.rf.3)
plot(prostate.rf.p)


###  m  = 1  


prostate.rf.1 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                       importance=TRUE, ntree=700, mtry=1, keep.forest=TRUE)
prostate.rf.1             # Barely useful here
summary(prostate.rf.1)    # Not useful here.
importance(prostate.rf.1) # Print out importance measures
quartz(h=7,w=12,pointsize=12)
varImpPlot(prostate.rf.1) # Plot of importance measures; more interesting with more variables

prostate.oob.1 <- mean((predict(prostate.rf.1)-prostate[which(prostate$set==1),10])^2)

prostate.mspe.1 <- mean((predict(prostate.rf.1,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)


# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

quartz(h=7,w=12,pointsize=12,title = "m = 1")
par(mfrow=c(2,4))

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=lcavol,ylim=c(-0.5,3.5))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),2]),10] 
points(lcavol.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=lweight,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),3]),10] 
points(lweight.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=age,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),4]),10] 
points(age.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=lbph,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),5]),10] 
points(lbph.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=svi,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),6]),10] 
points(svi.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=lcp,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),7]),10] 
points(lcp.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=gleason,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),8]),10] 
points(gleason.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.1, pred.data=prostate[which(prostate$set==1),-1], x.var=pgg45,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),9]),10] 
points(pgg.ord,prostate.ord,col="gray")


###  m  = p / 3
mmm <-  ncol(x.1) / 3
prostate.rf.2 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                       importance=TRUE, ntree=600, mtry= mmm, keep.forest=TRUE)
prostate.rf.2             # Barely useful here
summary(prostate.rf.2)    # Not useful here.
importance(prostate.rf.2) # Print out importance measures
quartz(h=7,w=12,pointsize=12)
varImpPlot(prostate.rf.2) # Plot of importance measures; more interesting with more variables

prostate.oob.2 <- mean((predict(prostate.rf.2)-prostate[which(prostate$set==1),10])^2)

prostate.mspe.2<- mean((predict(prostate.rf.2,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)
# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

quartz(h=7,w=12,pointsize=12,title="m = p / 3")
par(mfrow=c(2,4))

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=lcavol,ylim=c(-0.5,3.5))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),2]),10] 
points(lcavol.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=lweight,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),3]),10] 
points(lweight.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=age,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),4]),10] 
points(age.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=lbph,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),5]),10] 
points(lbph.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=svi,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),6]),10] 
points(svi.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=lcp,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),7]),10] 
points(lcp.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=gleason,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),8]),10] 
points(gleason.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.2, pred.data=prostate[which(prostate$set==1),-1], x.var=pgg45,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),9]),10] 
points(pgg.ord,prostate.ord,col="gray")




###  m  = 2p / 3
mmm <- 2 * ncol(x.1) / 3
prostate.rf.3 <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                       importance=TRUE, ntree=600, mtry= mmm, keep.forest=TRUE)
prostate.rf.3             # Barely useful here
summary(prostate.rf.3)    # Not useful here.

importance(prostate.rf.3) # Print out importance measures
quartz(h=7,w=12,pointsize=12)
varImpPlot(prostate.rf.3) # Plot of importance measures; more interesting with more variables



prostate.oob.3 <- mean((predict(prostate.rf.3)-prostate[which(prostate$set==1),10])^2)

prostate.mspe.3<- mean((predict(prostate.rf.3,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

quartz(h=7,w=12,pointsize=12,title = "m = 2p / 3")
par(mfrow=c(2,4))

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=lcavol,ylim=c(-0.5,3.5))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),2]),10] 
points(lcavol.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=lweight,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),3]),10] 
points(lweight.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=age,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),4]),10] 
points(age.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=lbph,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),5]),10] 
points(lbph.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=svi,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),6]),10] 
points(svi.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=lcp,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),7]),10] 
points(lcp.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=gleason,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),8]),10] 
points(gleason.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.3, pred.data=prostate[which(prostate$set==1),-1], x.var=pgg45,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),9]),10] 
points(pgg.ord,prostate.ord,col="gray")




###  m = p

mmm <-  ncol(x.1)
prostate.rf.p <- randomForest(data=prostate[which(prostate$set==1),-c(1,11,12)], lpsa ~ ., 
                       importance=TRUE, ntree=600, mtry= mmm , keep.forest=TRUE)
prostate.rf.p             # Barely useful here
summary(prostate.rf.p)    # Not useful here.
importance(prostate.rf.p) # Print out importance measures
quartz(h=7,w=6,pointsize=12,title="variable importance, m = p")
varImpPlot(prostate.rf.p) # Plot of importance measures; more interesting with more variables



prostate.oob.p <- mean((predict(prostate.rf.p,)-prostate[which(prostate$set==1),10])^2)

prostate.mspe.p <- mean((predict(prostate.rf.p,prostate[which(prostate$set==2),-c(1,11,12)])-prostate[which(prostate$set==2),10])^2)



# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(prostate.rf.p)

quartz(h=7,w=12,pointsize=12,title="Marginal Plot, m = p")
par(mfrow=c(2,4))

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=lcavol,ylim=c(-0.5,4.0))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),2]),10] 
points(lcavol.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=lweight,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),3]),10] 
points(lweight.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=age,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),4]),10] 
points(age.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=lbph,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),5]),10] 
points(lbph.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=svi,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),6]),10] 
points(svi.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=lcp,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),7]),10] 
points(lcp.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=gleason,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),8]),10] 
points(gleason.ord,prostate.ord,col="gray")

partialPlot(prostate.rf.p, pred.data=prostate[which(prostate$set==1),-1], x.var=pgg45,,ylim=c(-0.5,3))
prostate.ord <- prostate[order(prostate[which(prostate$set==1),9]),10] 
points(pgg.ord,prostate.ord,col="gray")


oob <- cbind(prostate.oob.1,prostate.oob.2,prostate.oob.3,prostate.oob.p)
mspe <- cbind(prostate.mspe.1 ,prostate.mspe.2 ,prostate.mspe.3 ,prostate.mspe.p)

