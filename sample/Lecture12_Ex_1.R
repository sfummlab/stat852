# random Forests for Prostate Data

prostate <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
#prostate <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)


y.1 <- as.matrix(prostate[which(set==1),10])
x.1 <- as.matrix(prostate[which(set==1),c(2:9)])

y.2 <- as.matrix(prostate[which(set==2),10])
x.2 <- as.matrix(prostate[which(set==2),c(2:9)])

x.all <- as.matrix(prostate[,c(2:9)])
y.all <- as.matrix(prostate[,10])


library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
####################################################################

pro.rf <- randomForest(x=x.all, y=y.all, importance=TRUE, ntree=1000, mtry=1, keep.forest=TRUE)
pro.rf             # Barely useful here
importance(pro.rf)
win.graph(h=7,w=6) # Print out importance measures
varImpPlot(pro.rf) # Plot of importance measures; more interesting with more variables

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

win.graph(h=15,w=12,pointsize=12)
par(mfrow=c(4,2))
partialPlot(pro.rf, pred.data=x.all, x.var=lcavol, ylim=c(-.5,6))
points(y=y.all, x=x.all[,1])
partialPlot(pro.rf, pred.data=x.all, x.var=lweight, ylim=c(-.5,6))
points(y=y.all, x=x.all[,2])
partialPlot(pro.rf, pred.data=x.all, x.var=lcp, ylim=c(-.5,6))
points(y=y.all, x=x.all[,6])
partialPlot(pro.rf, pred.data=x.all, x.var=svi, ylim=c(-.5,6))
points(y=y.all, x=x.all[,5])
partialPlot(pro.rf, pred.data=x.all, x.var=pgg45, ylim=c(-.5,6))
points(y=y.all, x=x.all[,8])
partialPlot(pro.rf, pred.data=x.all, x.var=gleason, ylim=c(-.5,6))
points(y=y.all, x=x.all[,7])
partialPlot(pro.rf, pred.data=x.all, x.var=age, ylim=c(-.5,6))
points(y=y.all, x=x.all[,3])
partialPlot(pro.rf, pred.data=x.all, x.var=lbph, ylim=c(-.5,6))
points(y=y.all, x=x.all[,4])

# Default plot method shows OOB error vs. number of trees.

win.graph(h=7,w=6,pointsize=12)
plot(pro.rf)

# Histogram of tree sizes
win.graph(h=7,w=6,pointsize=12)
hist(treesize(pro.rf))

# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(pro.rf)

OOB.MSE <- mean((y.all-pro.rf$predicted)^2)
win.graph(h=7,w=6,pointsize=12)
plot(y.all,pro.rf$predicted)

####################################################################
# Now using p/3 covariates
####################################################################

pro.rf.3 <- randomForest(x=x.all, y=y.all, importance=TRUE, ntree=1000, keep.forest=TRUE)
pro.rf.3             # Barely useful here
importance(pro.rf.3) # Print out importance measures
varImpPlot(pro.rf.3) # Plot of importance measures; more interesting with more variables

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

win.graph(h=15,w=12,pointsize=12)
par(mfrow=c(4,2))
partialPlot(pro.rf.3, pred.data=x.all, x.var=lcavol, ylim=c(-.5,6))
points(y=y.all, x=x.all[,1])
partialPlot(pro.rf.3, pred.data=x.all, x.var=lweight, ylim=c(-.5,6))
points(y=y.all, x=x.all[,2])
partialPlot(pro.rf.3, pred.data=x.all, x.var=lcp, ylim=c(-.5,6))
points(y=y.all, x=x.all[,6])
partialPlot(pro.rf.3, pred.data=x.all, x.var=svi, ylim=c(-.5,6))
points(y=y.all, x=x.all[,5])
partialPlot(pro.rf.3, pred.data=x.all, x.var=pgg45, ylim=c(-.5,6))
points(y=y.all, x=x.all[,8])
partialPlot(pro.rf.3, pred.data=x.all, x.var=gleason, ylim=c(-.5,6))
points(y=y.all, x=x.all[,7])
partialPlot(pro.rf.3, pred.data=x.all, x.var=age, ylim=c(-.5,6))
points(y=y.all, x=x.all[,3])
partialPlot(pro.rf.3, pred.data=x.all, x.var=lbph, ylim=c(-.5,6))
points(y=y.all, x=x.all[,4])

# Default plot method shows OOB error vs. number of trees.

win.graph(h=7,w=6,pointsize=12)
plot(pro.rf.3)

# Histogram of tree sizes
win.graph(h=7,w=6,pointsize=12)
hist(treesize(pro.rf.3))

# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(pro.rf.3)

OOB.MSE.3 <- mean((y.all-pro.rf.3$predicted)^2)
####################################################################
# Now using 2P/3 covariates
####################################################################

pro.rf.5 <- randomForest(x=x.all, y=y.all, importance=TRUE, ntree=1000, mtry=5, keep.forest=TRUE)
pro.rf.5             # Barely useful here
importance(pro.rf.5) # Print out importance measures
varImpPlot(pro.rf.5) # Plot of importance measures; more interesting with more variables

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

win.graph(h=15,w=12,pointsize=12)
par(mfrow=c(4,2))
partialPlot(pro.rf.5, pred.data=x.all, x.var=lcavol, ylim=c(-.5,6))
points(y=y.all, x=x.all[,1])
partialPlot(pro.rf.5, pred.data=x.all, x.var=lweight, ylim=c(-.5,6))
points(y=y.all, x=x.all[,2])
partialPlot(pro.rf.5, pred.data=x.all, x.var=lcp, ylim=c(-.5,6))
points(y=y.all, x=x.all[,6])
partialPlot(pro.rf.5, pred.data=x.all, x.var=svi, ylim=c(-.5,6))
points(y=y.all, x=x.all[,5])
partialPlot(pro.rf.5, pred.data=x.all, x.var=pgg45, ylim=c(-.5,6))
points(y=y.all, x=x.all[,8])
partialPlot(pro.rf.5, pred.data=x.all, x.var=gleason, ylim=c(-.5,6))
points(y=y.all, x=x.all[,7])
partialPlot(pro.rf.5, pred.data=x.all, x.var=age, ylim=c(-.5,6))
points(y=y.all, x=x.all[,3])
partialPlot(pro.rf.5, pred.data=x.all, x.var=lbph, ylim=c(-.5,6))
points(y=y.all, x=x.all[,4])

# Default plot method shows OOB error vs. number of trees.

win.graph(h=7,w=6,pointsize=12)
plot(pro.rf.5)

# Histogram of tree sizes
win.graph(h=7,w=6,pointsize=12)
hist(treesize(pro.rf.5))

# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(pro.rf.5)

OOB.MSE.5 <- mean((y.all-pro.rf.5$predicted)^2)
####################################################################
# Now using 8 covariates
####################################################################

pro.rf.8 <- randomForest(x=x.all, y=y.all, importance=TRUE, ntree=1000, mtry=8, keep.forest=TRUE)
pro.rf.8             # Barely useful here
importance(pro.rf.8) # Print out importance measures
varImpPlot(pro.rf.8) # Plot of importance measures; more interesting with more variables

# Plot partial effects (marginalized over all other variables)
#   Plot for X_j is average estimated mean when X_j = x, plotted against x

win.graph(h=15,w=12,pointsize=12)
par(mfrow=c(4,2))
partialPlot(pro.rf.8, pred.data=x.all, x.var=lcavol, ylim=c(-.5,6))
points(y=y.all, x=x.all[,1])
partialPlot(pro.rf.8, pred.data=x.all, x.var=lweight, ylim=c(-.5,6))
points(y=y.all, x=x.all[,2])
partialPlot(pro.rf.8, pred.data=x.all, x.var=lcp, ylim=c(-.5,6))
points(y=y.all, x=x.all[,6])
partialPlot(pro.rf.8, pred.data=x.all, x.var=svi, ylim=c(-.5,6))
points(y=y.all, x=x.all[,5])
partialPlot(pro.rf.8, pred.data=x.all, x.var=pgg45, ylim=c(-.5,6))
points(y=y.all, x=x.all[,8])
partialPlot(pro.rf.8, pred.data=x.all, x.var=gleason, ylim=c(-.5,6))
points(y=y.all, x=x.all[,7])
partialPlot(pro.rf.8, pred.data=x.all, x.var=age, ylim=c(-.5,6))
points(y=y.all, x=x.all[,3])
partialPlot(pro.rf.8, pred.data=x.all, x.var=lbph, ylim=c(-.5,6))
points(y=y.all, x=x.all[,4])

# Default plot method shows OOB error vs. number of trees.

win.graph(h=7,w=6,pointsize=12)
plot(pro.rf.8)

# Histogram of tree sizes
win.graph(h=7,w=6,pointsize=12)
hist(treesize(pro.rf.8))

# Number of times each variable is used in a split.  Not interesting here; should be 50/50
varUsed(pro.rf.8)

OOB.MSE.8 <- mean((y.all-pro.rf.8$predicted)^2)

summary(y.all)
