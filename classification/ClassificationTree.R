# Classification Trees
# Vehicle image data

vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")
#vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

library(rpart)
####################################################################
## Default tree
## Specifying method="class" for classification
##   Split criterion is Gini
##   Deviance is available through parms=list(split="information")
####################################################################

veh.tree <- rpart(data=set1, class ~ ., method="class", cp=0.001)
print(veh.tree, digits=3)
printcp(veh.tree)
# summary(veh.tree) #Lots of output

# See pdf of this---Note that it IS making splits that improve 
#   probabilities but do not change classes
library(rpart.plot)
x11(h=10, w=10)
prp(veh.tree, type=1, extra=1, main="Original full tree")

# Plot of the cross-validation for the complexity parameter.
##  NOTE: Very variable, depending on CV partitioning
quartz(h=7, w=10, pointsize=10)
plotcp(veh.tree)

veh.tree$cptable 

# Tuning instead to a validation set
# obj=full tree object, valid=validation data, y=class responses, 
#   grid=number of candidate cp values (all are 1/c, c=1,2,...,grid)
val.tune <- function(obj, valid, G, grid) {
  cp <- matrix(0, ncol=2, nrow=grid)
  for (x in c(1:grid)){
   cp[x,1] <- x/grid  
   pred <- predict(prune(obj, cp=x/grid), newdata=valid, type="class")
   cp[x,2] <- mean(ifelse(pred == G, yes=0, no=1))
}
cp
}
veh.valtree <- val.tune(obj=veh.tree, valid=set2, G=set2$class, grid=1000)
# Returns optimal cp and misclassification rate there.
veh.valtree[which.min(veh.valtree[,2]), ]

# Creating a pruned tree using a selected value of the CP by CV.
veh.prune.cv.1se <- prune(veh.tree, cp=0.035)
# Creating a pruned tree using a selected value of the CP by CV.
veh.prune.cv.min <- prune(veh.tree, cp=0.011)
# Creating a pruned tree using a selected value of the CP by validation.
veh.prune.val <- prune(veh.tree, cp=0.007)

# Plot the pruned trees
x11(h=10, w=18)
par(mfrow=c(1,3))
prp(veh.prune.cv.1se, type=1, extra=1, main="Pruned CV-1SE tree")
prp(veh.prune.cv.min, type=1, extra=1, main="Pruned CV-min tree")
prp(veh.prune.val, type=1, extra=1, main="Pruned Val tree")


# Predict results of classification. "Vector" means store class as a number
pred.test.cv.1se <- predict(veh.prune.cv.1se, newdata=set3, type="vector")
pred.test.cv.min <- predict(veh.prune.cv.min, newdata=set3, type="vector")
pred.test.val <- predict(veh.prune.val, newdata=set3, type="vector")
pred.test.full <- predict(veh.tree, newdata=set3, type="vector")

(misclass.test.cv.1se <- mean(ifelse(pred.test.cv.1se == set3$class, yes=0, no=1)))
(misclass.test.cv.min <- mean(ifelse(pred.test.cv.min == set3$class, yes=0, no=1)))
(misclass.test.val <- mean(ifelse(pred.test.val == set3$class, yes=0, no=1)))
(misclass.test.full <- mean(ifelse(pred.test.full == set3$class, yes=0, no=1)))

# Confusion Matrices
table(pred.test.cv.1se, set3$class,  dnn=c("Predicted","Observed"))
table(pred.test.cv.min, set3$class,  dnn=c("Predicted","Observed"))
table(pred.test.val, set3$class,  dnn=c("Predicted","Observed"))
table(pred.test.full, set3$class,  dnn=c("Predicted","Observed"))
