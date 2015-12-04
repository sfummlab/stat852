wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")


wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]

library(rpart)
wheat.tree <- rpart(data=set1, type  ~ ., method="class", cp=0.001)
print(wheat.tree, digits=3)
printcp(wheat.tree)


library(rpart.plot)
quartz(h=10, w=10)
prp(wheat.tree, type=1, extra=1, main="Original full tree")

cpt <- wheat.tree$cptable
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
wheat.prune.min <- prune(wheat.tree, cp=cp.min)
wheat.prune.1se <- prune(wheat.tree, cp=cp.1se)

library(rpart.plot)
quartz(h=6, w=12)
par(mfrow=c(1,2))
prp(wheat.prune.min, type=1, extra=1, main="Pruned Min tree")

prp(wheat.prune.1se, type=1, extra=1, main="Pruned 1se tree")

# Predict results of classification. "Vector" means store class as a number
pred.test.cv.1se <- predict(wheat.prune.1se, newdata=set2, type="class")
pred.test.cv.min <- predict(wheat.prune.min, newdata=set2, type="class")
pred.test.full <- predict(wheat.tree, newdata=set2, type="class")

(misclass.test.cv.1se <- mean(ifelse(pred.test.cv.1se == set2$type, yes=0, no=1)))
(misclass.test.cv.min <- mean(ifelse(pred.test.cv.min == set2$type, yes=0, no=1)))
(misclass.test.full <- mean(ifelse(pred.test.full == set2$type, yes=0, no=1)))

# Confusion Matrices
table(pred.test.cv.1se, set2$type,  dnn=c("Predicted","Observed"))
table(pred.test.cv.min, set2$type,  dnn=c("Predicted","Observed"))
table(pred.test.full,   set2$type,  dnn=c("Predicted","Observed"))









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
library(randomForest)
wheat.rf.1 <- randomForest(data=set1, type ~., 
                         importance=TRUE, ntree=10000, mtry=2.5, keep.forest=TRUE)
wheat.rf.1             # more useful here

### Is 1000 enough trees???  Maybe not.  rerun with 1500.
quartz(h=7,w=6,pointsize=12)
plot(wheat.rf.1)

round(importance(wheat.rf.1),3) # Print out importance measures
quartz(h=7,w=15)
varImpPlot(wheat.rf.1) # Plot of importance measures; more interesting with more variables


# Predict results of classification. 
pred.rf.1.train <- predict(wheat.rf.1, newdata=set1, type="response")
pred.rf.1.test <- predict(wheat.rf.1, newdata=set2, type="response")
pred.rf.1.vtest <- predict(wheat.rf.1, newdata=set2, type="vote")

(misclass.train.rf1 <- mean(ifelse(pred.rf.1.train == set1$type, yes=0, no=1)))
(misclass.test.rf1 <- mean(ifelse(pred.rf.1.test == set2$type, yes=0, no=1)))

# These are HUGE trees!  Can we do better with smaller trees?
quartz(h=7,w=6,pointsize=12)
par(mfrow=c(1,2))
hist(treesize(wheat.rf.4))
hist(treesize(wheat.rf.6))

wheat.rf.1  <-  randomForest(data=set1, type~.,mtry = 1,importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.2  <-  randomForest(data=set1, type~.,mtry = 2,importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.3  <-  randomForest(data=set1, type~.,mtry = 3,importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.4  <-  randomForest(data=set1, type~.,mtry = 4,importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.5  <-  randomForest(data=set1, type~.,mtry = 5,importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.6  <-  randomForest(data=set1, type~.,mtry = 6,importance=TRUE, ntree=5000, keep.forest=TRUE)

pred.rf.1 <- predict(wheat.rf.1, type="response")
(misclass.oob.1 <- mean(ifelse(pred.rf.1 == set1$type, yes=0, no=1)))
pred.rf.2 <- predict(wheat.rf.2, type="response")
(misclass.oob.2 <- mean(ifelse(pred.rf.2 == set1$type, yes=0, no=1)))
pred.rf.3 <- predict(wheat.rf.3, type="response")
(misclass.oob.3 <- mean(ifelse(pred.rf.3 == set1$type, yes=0, no=1)))
pred.rf.4 <- predict(wheat.rf.4, type="response")
(misclass.oob.4 <- mean(ifelse(pred.rf.4 == set1$type, yes=0, no=1)))
pred.rf.5 <- predict(wheat.rf.5, type="response")
(misclass.oob.5 <- mean(ifelse(pred.rf.5 == set1$type, yes=0, no=1)))
pred.rf.6 <- predict(wheat.rf.6, type="response")
(misclass.oob.6 <- mean(ifelse(pred.rf.6 == set1$type, yes=0, no=1)))






### Is 1000 enough trees???  Maybe not.  rerun with 1500.
quartz(h=7,w=6,pointsize=12)
plot(wheat.rf.3)

round(importance(wheat.rf.3),3) # Print out importance measures
quartz(h=7,w=15)
varImpPlot(wheat.rf.3) # Plot of importance measures; more interesting with more variables


# Predict results of classification. 
pred.rf.3.train <- predict(wheat.rf.3, newdata=set1, type="response")
pred.rf.3.test <- predict(wheat.rf.3, newdata=set2, type="response")
pred.rf.3.vtest <- predict(wheat.rf.3, newdata=set2, type="vote")

(misclass.train.rf3 <- mean(ifelse(pred.rf.3.train == set1$type, yes=0, no=1)))
(misclass.test.rf3 <- mean(ifelse(pred.rf.3.test == set2$type, yes=0, no=1)))





oob.mtry <- cbind(misclass.oob.1,misclass.oob.2,misclass.oob.3,misclass.oob.4,misclass.oob.5,misclass.oob.6)
############### Could try other numbers of variables and sizes of trees
wheat.rf.0  <-  randomForest(data=set1, type~.,mtry = 3,maxnodes = 1000, importance=TRUE, ntree=5000, keep.forest=TRUE)
pred.rf.0 <- predict(wheat.rf.0, type="response")
(misclass.oob.0 <- mean(ifelse(pred.rf.0 == set1$type, yes=0, no=1)))


################## Max Node Size , Reduce size to smaller Tree ################
wheat.rf.1  <-  randomForest(data=set1, type~.,maxnodes = 5 ,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.2  <-  randomForest(data=set1, type~.,maxnodes = 10,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.3  <-  randomForest(data=set1, type~.,maxnodes = 20,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.4  <-  randomForest(data=set1, type~.,maxnodes = 40,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.5  <-  randomForest(data=set1, type~.,maxnodes = 60,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)
wheat.rf.6  <-  randomForest(data=set1, type~.,maxnodes = 80,mtry=3, importance=TRUE, ntree=5000, keep.forest=TRUE)


pred.rf.1 <- predict(wheat.rf.1, type="response")
(misclass.oob.1 <- mean(ifelse(pred.rf.1 == set1$type, yes=0, no=1)))
pred.rf.2 <- predict(wheat.rf.2, type="response")
(misclass.oob.2 <- mean(ifelse(pred.rf.2 == set1$type, yes=0, no=1)))
pred.rf.3 <- predict(wheat.rf.3, type="response")
(misclass.oob.3 <- mean(ifelse(pred.rf.3 == set1$type, yes=0, no=1)))
pred.rf.4 <- predict(wheat.rf.4, type="response")
(misclass.oob.4 <- mean(ifelse(pred.rf.4 == set1$type, yes=0, no=1)))
pred.rf.5 <- predict(wheat.rf.5, type="response")
(misclass.oob.5 <- mean(ifelse(pred.rf.5 == set1$type, yes=0, no=1)))
pred.rf.6 <- predict(wheat.rf.6, type="response")
(misclass.oob.6 <- mean(ifelse(pred.rf.6 == set1$type, yes=0, no=1)))




oob.mtry <- cbind(misclass.oob.1,misclass.oob.2,misclass.oob.3,misclass.oob.4,misclass.oob.5,misclass.oob.6)



