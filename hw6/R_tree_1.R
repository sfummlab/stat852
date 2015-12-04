# Regression trees using rpart 
prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(rpart)
####################################################################
## Default tree
####################################################################

control1 <- rpart.control(xval = 5)
prostate.tree <- rpart(data=prostate,lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, method="anova", 
                       control = control1,cp = 0)
prostate.tree
prostate.tree$cptable
summary(prostate.tree)

meanvar(prostate.tree) # Print out plot of variances vs means for terminal nodes



library(rpart.plot)
quartz(h=7, w=6, pointsize=11)
prp(prostate.tree, type=1, extra=1, main="Original full tree")


# Plot of the cross-validation for the complexity parameter.
quartz(h=7, w=8)
plotcp(prostate.tree)


# The code below shows how to select the tuning parameter using
#   either the +1SE or the true min CV error
cpt <- prostate.tree$cptable


sMSE <-  1.3187 * prostate.tree$cptable[,3]
MSPE <-  1.3187 * prostate.tree$cptable[,4]
# Find location of minimum error
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- cpt[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)

# Do pruning each way
prostate.prune.min <- prune(prostate.tree, cp=cp.min)
prostate.prune.1se <- prune(prostate.tree, cp=cp.1se)


library(rpart.plot)

quartz(h=10, w=12, pointsize=11)
par(mfrow=c(1,2))
prp(prostate.prune.min, type=1, extra=1, main="Larger tree pruned to Min CV Error")

prp(prostate.prune.1se, type=1, extra=1, main="Larger tree pruned to +1SE CV Error")