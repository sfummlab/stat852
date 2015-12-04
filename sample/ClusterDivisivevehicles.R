# Hierarchical Clustering
# Vehicle image data

vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")


## Need to convert data to a numerical matrix.

mydata <- data.matrix(vehdata[,-c(19,20)])
summary(mydata)

##################################################################
# Clustering functions are available in "cluster" package.
#  agnes() does agglomerative clustering
#  diana() does partitioning
###
#  Both have options for different dissimilarities.  
#  In diana(), there are no special linkage options (average is assumed).
#  "To divide the selected cluster, the algorithm first looks for 
#   its most disparate observation (i.e., which has the largest 
#   average dissimilarity to the other observations of the selected 
#   cluster). This observation initiates the 'splinter group'. In 
#   subsequent steps, the algorithm reassigns observations that are 
#   closer to the "splinter group" than to the 'old party'. The result 
#   is a division of the selected cluster into two new clusters."
###
##################################################################

# First, no standardization

div.avg <- diana(x=mydata, metric="euclidean")
div.avg
summary(div.avg)
win.graph(h=7, w=15, pointsize=8)
plot(div.avg, which.plots=2)

# Next, add standardization

div.avg.Z <- diana(x=mydata, metric="euclidean", stand=TRUE)
div.avg.Z
summary(div.avg.Z)
win.graph(h=7, w=15, pointsize=8)
plot(div.avg.Z, which.plots=2)

#JUST FOR FUN: Compare clusters to classes to see whether the method
#  picks up the same information as a classification method

cut.div.avg5 <- cutree(div.avg, k=5)
table(cut.div.avg5, vehdata$class,  dnn=c("Cluster","Class"))
cut.div.avg10 <- cutree(div.avg, k=10)
table(cut.div.avg10, vehdata$class,  dnn=c("Cluster","Class"))
cut.div.avgz5 <- cutree(div.avg.Z, k=5)
table(cut.div.avgz5, vehdata$class,  dnn=c("Cluster","Class"))
cut.div.avgz10 <- cutree(div.avg.Z, k=10)
table(cut.div.avgz10, vehdata$class,  dnn=c("Cluster","Class"))

