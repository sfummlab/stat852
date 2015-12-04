#vehdata <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\R\\vehicle3.txt",header=TRUE,sep=" ")
vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

library(e1071)
###############################################################
## Naive Bayes is done in e1071::naiveBayes(). Unfortunately, 
##  the default is simply to assume a Normal density in each margin
##    (i.e. assume multivariate normality with no correlations).
##  This makes it a cheap version of LDA.
###############################################################

set1$class = as.factor(set1$class)
nb.0 <- naiveBayes(x=set1[,-19], y=set1[,19])


# Calculate in-sample and out-of-sample misclassification error
nb.pred.train <- predict(nb.0, newdata=set1[,-19], type="class")
table(predict(nb.0, set1[,-19]), set1[,19])

nb.pred.valid <- predict(nb.0, newdata=set2[,-19], type="class")
nb.pred.test <- predict(nb.0, newdata=set3[,-19], type="class")
(nbmisclass.train <- mean(ifelse(nb.pred.train == set1$class, yes=0, no=1)))
(nbmisclass.valid <- mean(ifelse(nb.pred.valid == set2$class, yes=0, no=1)))
(nbmisclass.test <- mean(ifelse(nb.pred.test == set3$class, yes=0, no=1)))

######################################################
## klaR::NaiveBayes is an experimental function that uses the 
##   e1071::naiveBayes() function, but does Gaussian Kernel Smothing
## ********predict() Gives error messages, but seems to work
######################################################

library(klaR)
NB <- NaiveBayes(x=set1[,-19], grouping=set1[,19], usekernel=TRUE)

quartz(h=7,w=6)
plot(NB, lwd=2)

NB.pred.train <- predict(NB, newdata=set1[,-19], type="class")
table(NB.pred.train$class, set1[,19], dnn=c("Predicted","Observed"))

NB.pred.valid <- predict(NB, newdata=set2[,-19], type="class")
table(NB.pred.valid$class, set2[,19], dnn=c("Predicted","Observed"))

NB.pred.test <- predict(NB, newdata=set3[,-19], type="class")
table(NB.pred.test$class, set3[,19], dnn=c("Predicted","Observed"))
warnings()
round(NB.pred.test$posterior)

# Error rates
(NBmisclass.train <- mean(ifelse(NB.pred.train$class == set1$class, yes=0, no=1)))
(NBmisclass.valid <- mean(ifelse(NB.pred.valid$class == set2$class, yes=0, no=1)))
(NBmisclass.test <- mean(ifelse(NB.pred.test$class == set3$class, yes=0, no=1)))

####################################################################
# Idea from Shengdong Zhang:Run PCA before Naive Bayes to decorrelate data
#   This is something that has been proposed in the literature
#   See Liwei Fan, Kim Leng Poh, 2007, A Comparative Study of PCA, ICA 
#   and Class-Conditional ICA for Na?ve Bayes Classifier.

pc <-  prcomp(x=set1[,-19], scale.=TRUE)

# Create the same transformations in all three data sets 
#   and attach the response variable at the end
#   predict() does this 
xi.1 <- data.frame(pc$x,class = as.factor(set1$class))
xi.2 <- data.frame(predict(pc, newdata=set2), class = as.factor(set2$class))
xi.3 <- data.frame(predict(pc, newdata=set3), class = as.factor(set3$class))

NB.pc <- NaiveBayes(x=xi.1[,-19], grouping=xi.1[,19], usekernel=TRUE)

quartz(h=7,w=6)
plot(NB.pc, lwd=2)

NBpc.pred.train <- predict(NB.pc, newdata=xi.1[,-19], type="class")
table(NBpc.pred.train$class, xi.1[,19], dnn=c("Predicted","Observed"))

NBpc.pred.valid <- predict(NB.pc, newdata=xi.2[,-19], type="class")
table(NBpc.pred.valid$class, xi.1[,19], dnn=c("Predicted","Observed"))

NBpc.pred.test <- predict(NB.pc, newdata=xi.3[,-19], type="class")
table(NBpc.pred.test$class, xi.1[,19], dnn=c("Predicted","Observed"))
warnings()
round(NBpc.pred.test$posterior)

# Error rates
(NBPCmisclass.train <- mean(ifelse(NBpc.pred.train$class == xi.1$class, yes=0, no=1)))
(NBPCmisclass.valid <- mean(ifelse(NBpc.pred.valid$class == xi.2$class, yes=0, no=1)))
(NBPCmisclass.test <- mean(ifelse(NBpc.pred.test$class == xi.3$class, yes=0, no=1)))

# It definitely helps!
