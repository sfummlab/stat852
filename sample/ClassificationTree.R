# Classification Trees
# Vehicle image data

vehdata <-  read.table("C:\\Users\\tloughin\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")
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

# Plotting results twice just to show different options.


win.graph(h=9, w=12, pointsize=10)
plot(veh.tree, branch=0, uniform=TRUE)
text(veh.tree, use.n=TRUE, digits=3, all=TRUE, xpd=TRUE)

# Plot of the cross-validation for the complexity parameter.
##  NOTE: Very variable, depending on CV partitioning
win.graph(h=7, w=10, pointsize=10)
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
veh.valtree <- val.tune(obj=veh.tree, val=set2, G=set2$class, grid=1000)
# Returns optimal cp and misclassification rate there.
veh.valtree[which.min(veh.valtree[,2]), ]

# Creating a pruned tree using a selected value of the CP by CV.
veh.prune.cv <- prune(veh.tree, cp=0.035)
# Creating a pruned tree using a selected value of the CP by validation.
veh.prune.val <- prune(veh.tree, cp=0.007)

# Plot the pruned trees
win.graph(h=7, w=10, pointsize=11)
plot(veh.prune.cv, branch=0, uniform=TRUE, main="Final CV-pruned tree")
text(veh.prune.cv, use.n=TRUE, digits=3, all=TRUE, xpd=TRUE)

win.graph(h=7, w=10, pointsize=11)
plot(veh.prune.val, branch=0, uniform=TRUE, main="Final Validation-pruned tree")
text(veh.prune.val, use.n=TRUE, digits=3, all=TRUE, xpd=TRUE)

# Predict results of classification. "Vector" means store class as a number
pred.test.cv <- predict(veh.prune.cv, newdata=set3, type="vector")
pred.test.val <- predict(veh.prune.val, newdata=set3, type="vector")
pred.test.full <- predict(veh.tree, newdata=set3, type="vector")

misclass.test.cv <- mean(ifelse(pred.test.cv == set3$class, yes=0, no=1))
misclass.test.val <- mean(ifelse(pred.test.val == set3$class, yes=0, no=1))
misclass.test.full <- mean(ifelse(pred.test.full == set3$class, yes=0, no=1))

# Confusion Matrices
table(pred.test.cv, set3$class,  dnn=c("Predicted","Observed"))
table(pred.test.val, set3$class,  dnn=c("Predicted","Observed"))
table(pred.test.full, set3$class,  dnn=c("Predicted","Observed"))
