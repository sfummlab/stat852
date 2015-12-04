# Random Forests for Classification
# Vehicle image data

vehdata <-  read.table("~stat852/data/vehicle3.txt",header=TRUE,sep=" ")
#vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

vehdata$class <- as.factor(vehdata$class)

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]


library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
## Default nodesize = 1
####################################################################

veh.rf.1 <- randomForest(data=set1, class~., 
                       importance=TRUE, ntree=1000, mtry=1, keep.forest=TRUE)
veh.rf.1             # more useful here

### Is 1000 enough trees???  Maybe not.  rerun with 1500.
win.graph(h=7,w=6,pointsize=12)
plot(veh.rf.1)

## No fear of overfitting.  Just taking more random samples.

veh.rf.1 <- randomForest(data=set1, class~., 
                         importance=TRUE, ntree=1500, mtry=1, keep.forest=TRUE)
veh.rf.1             # more useful here
### Is 1000 enough trees???  Maybe not.  rerun with 1500.
win.graph(h=7,w=6,pointsize=12)
plot(veh.rf.1)

round(importance(veh.rf.1),3) # Print out importance measures
win.graph(h=7,w=15)
varImpPlot(veh.rf.1) # Plot of importance measures; more interesting with more variables


# Predict results of classification. 
pred.rf.1.train <- predict(veh.rf.1, newdata=set1, type="response")
pred.rf.1.val <- predict(veh.rf.1, newdata=set2, type="response")
pred.rf.1.test <- predict(veh.rf.1, newdata=set3, type="response")
pred.rf.1.vtest <- predict(veh.rf.1, newdata=set3, type="vote")
pred.rf.1.vval <- predict(veh.rf.1, newdata=set2, type="vote")

(misclass.train.rf1 <- mean(ifelse(pred.rf.1.train == set1$class, yes=0, no=1)))
(misclass.val.rf1 <- mean(ifelse(pred.rf.1.val == set2$class, yes=0, no=1)))
(misclass.test.rf1 <- mean(ifelse(pred.rf.1.test == set3$class, yes=0, no=1)))

#################################################################
## Rerun, using sqrt(p)=4 vars/split.  Need more trees here.  trial and error
#################################################################

veh.rf.4 <- randomForest(data=set1, class~., 
                       importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4             # more useful here
# Default plot method shows OOB error vs. number of trees.
win.graph(h=7,w=6,pointsize=12)
plot(veh.rf.4)

round(importance(veh.rf.4),3) # Print out importance measures
win.graph(h=7,w=12)
varImpPlot(veh.rf.4) # Plot of importance measures; more interesting with more variables


# Predict results of classification. 
pred.rf.4.train <- predict(veh.rf.4, newdata=set1, type="response")
pred.rf.4.val <- predict(veh.rf.4, newdata=set2, type="response")
pred.rf.4.test <- predict(veh.rf.4, newdata=set3, type="response")

(misclass.train.4 <- mean(ifelse(pred.rf.4.train == set1$class, yes=0, no=1)))
(misclass.val.4 <- mean(ifelse(pred.rf.4.val == set2$class, yes=0, no=1)))
(misclass.test.4 <- mean(ifelse(pred.rf.4.test == set3$class, yes=0, no=1)))

# Tables of classification results
table(pred.rf.4.test, set3$class,  dnn=c("Predicted","Observed"))
table(pred.rf.4.val, set2$class,  dnn=c("Predicted","Observed"))

# These are HUGE trees!  Can we do better with smaller trees?
win.graph(h=7,w=6,pointsize=12)
hist(treesize(veh.rf.4))

veh.rf.4.80 <- randomForest(data=set1, class~., maxnodes=80,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4.60 <- randomForest(data=set1, class~., maxnodes=60,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4.40 <- randomForest(data=set1, class~., maxnodes=40,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4.20 <- randomForest(data=set1, class~., maxnodes=20,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4.10 <- randomForest(data=set1, class~., maxnodes=10,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)
veh.rf.4.5 <- randomForest(data=set1, class~., maxnodes=5,
                            importance=TRUE, ntree=2500, keep.forest=TRUE)

pred.rf.80.val <- predict(veh.rf.4.80, newdata=set2, type="response")
(misclass.val.80 <- mean(ifelse(pred.rf.80.val == set2$class, yes=0, no=1)))

pred.rf.60.val <- predict(veh.rf.4.60, newdata=set2, type="response")
(misclass.val.60 <- mean(ifelse(pred.rf.60.val == set2$class, yes=0, no=1)))

pred.rf.40.val <- predict(veh.rf.4.40, newdata=set2, type="response")
(misclass.val.40 <- mean(ifelse(pred.rf.40.val == set2$class, yes=0, no=1)))

pred.rf.20.val <- predict(veh.rf.4.20, newdata=set2, type="response")
(misclass.val.20 <- mean(ifelse(pred.rf.20.val == set2$class, yes=0, no=1)))

pred.rf.10.val <- predict(veh.rf.4.10, newdata=set2, type="response")
(misclass.val.10 <- mean(ifelse(pred.rf.10.val == set2$class, yes=0, no=1)))

pred.rf.5.val <- predict(veh.rf.4.5, newdata=set2, type="response")
(misclass.val.5 <- mean(ifelse(pred.rf.5.val == set2$class, yes=0, no=1)))


############### Could try other numbers of variables and sizes of trees ..............
