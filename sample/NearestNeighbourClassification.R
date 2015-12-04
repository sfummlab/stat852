# Classification by k-nearest neighbours
# Vehicle image data

#vehdata <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\R\\vehicle3.txt",header=TRUE,sep=" ")
vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 
vehdata$class <- factor(vehdata$class)
set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

library(FNN)

############
##Scaling to constant mean and SD, using set 1 statistics
############
scale.set1 <- function(x1,x2){
  mean <- apply(X=x1, MARGIN=2, FUN=mean)
  stdev <- apply(X=x1, MARGIN=2, FUN=sd)
  scale(x=x2, center=mean, scale=stdev)
}


x.1.unscaled <- as.matrix(set1[,-19])
x.1 <- scale.set1(x.1.unscaled,x.1.unscaled)
x.2.unscaled <- as.matrix(set2[,-19])
x.2 <- scale.set1(x.1.unscaled,x.2.unscaled)
x.3.unscaled <- as.matrix(set3[,-19])
x.3 <- scale.set1(x.1.unscaled,x.3.unscaled)

###########################################################################
# FNN
# The knn() function uses the X-values from the train=set as a supply of neighbours
#   from which to choose for each row in test= .  The true classes for the training 
#   set are given in cl= .  Obviously, k=  gives the number of neighbours.
# The output is a factor list containing the predicted values (no "predict()" needed).
###########################################################################
# Fit the 1-NN function
knnfit.1.2 <- knn(train=x.1, test=x.2, cl=set1[,19], k=1)
knnfit.1.2  # Not useful
plot(knnfit.1.2) # "Marginally" useful

table(knnfit.1.2, set2[,19],  dnn=c("Predicted","Observed"))
misclass.knn1.2 <- mean(ifelse(knnfit.1.2 == set2[,19], yes=0, no=1))

knnfit.1.3 <- knn(train=x.1, test=x.3, cl=set1[,19], k=1)
table(knnfit.1.3, set3[,19],  dnn=c("Predicted","Observed"))
misclass.knn1.3 <- mean(ifelse(knnfit.1.3 == set3[,19], yes=0, no=1))

# I created the steps below to fit a sequence of k  values to tune the knn.
#  Enter the maximum k as kmax.  Also need to change the data sets
#  in the two lines in the "runknn" function.
kmax <- 40
k <- matrix(c(1:kmax), nrow=kmax)
runknn <- function(x){
  knnfit <- knn(train=x.1, test=x.2, cl=set1[,19], k=x)
  mean(ifelse(knnfit == set2[,19], yes=0, no=1))
}

mis <- apply(X=k, MARGIN=1, FUN=runknn)
mis.se <- sqrt(mis*(1-mis)/nrow(set2))

# Plot like the CV plots, with 1SE bars and a horizontal line at the minimum.
win.graph(h=7,w=7,pointsize=12)
plot(x=k, y=mis, type="b", ylim=c(.20,.45)) 
for(ii in c(1:kmax)){
  lines(x=c(k[ii],k[ii]), y=c(mis[ii]-mis.se[ii], mis[ii]+mis.se[ii]), col=colors()[220])
}
abline(h=min(mis + mis.se), lty="dotted")

#Trying the value of k with the lowest validation error on test data set.
knnfit5.3 <- knn(train=x.1, test=x.3, cl=set1[,19], k=5)

table(knnfit5.3, set3[,19],  dnn=c("Predicted","Observed"))
misclass.3.knn5 <- mean(ifelse(knnfit5.3 == set3[,19], yes=0, no=1))

#Trying the 1-se value of k with the (largest k with validation error within 1 se).
knnfit9.3 <- knn(train=x.1, test=x.3, cl=set1[,19], k=9)

table(knnfit9.3, set3[,19],  dnn=c("Predicted","Observed"))
misclass.3.knn9 <- mean(ifelse(knnfit9.3 == set3[,19], yes=0, no=1))


# If you don't have a validation set, can use CV.  
# This drops out one y at a time and uses its neighbours to predict it.
# Can amend the tuning function above to use this instead of a validation set.
knncv.fit <- knn.cv(train=x.1, cl=set1[,19], k=1)
table(knncv.fit, set1[,19],  dnn=c("Predicted","Observed"))
misclass.cv <- mean(ifelse(knncv.fit == set1[,19], yes=0, no=1))
# After a k is chosen, can use that value in a knn() run to compute test error
