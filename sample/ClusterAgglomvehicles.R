# Hierarchical Clustering
# Vehicle image data

vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

## Need to convert data to a numerical matrix.

mydata <- data.matrix(vehdata[,-c(19,20)])
summary(mydata)

##################################################################
# Clustering functions are available in "cluster" package.
#  agnes() does agglomerative clustering
#  diana() does partitioning
###
#  Both have options for different dissimilarities.  
#  In agnes(), method=  specifies linkage method
#     Unless you have a reason to be certain about the nature of the clusters, try several.
###
#  There exist functions that can estimate the optimal number of clusters. 
#    One, in particular, computes p-values.  It is written by a paper author, 
#    and I haven't seen the paper recommended in HTF or IZ, so I don't know whether it is 
#    a good idea.  Read about the "pvclus"" package, see the citations, look them up, 
#    and decide for yourself!
##################################################################

library(cluster)
# First, we'll show average linkage and no standardization

agg.avg <- agnes(x=mydata, metric="euclidean", method="average")
agg.avg
# See help(agnes.object) to understand this better
aa <- summary(agg.avg)
names(aa)
aa$order
aa$height
aa$ac
# "ac = the agglomerative coefficient, measuring the clustering 
#      structure of the dataset.
# "For each observation i, denote by m(i) its dissimilarity to 
#   the first cluster it is merged with, divided by the dissimilarity 
#   of the merger in the final step of the algorithm. The ac is the 
#   average of all 1 - m(i). It can also be seen as the average width 
#   (or the percentage filled) of the banner plot. Because ac grows 
#   with the number of observations, this measure should not be used 
#   to compare datasets of very different sizes."
# In other words, the closer to 1 this value is, the smaller the 
#   (proportional) height at which average clustering takes place
aa$merge

# Plot: 1 = "banner" plot, 2 = dendogram
win.graph(h=7, w=15, pointsize=8)
plot(agg.avg, which.plots=2)
win.graph(h=7, w=15, pointsize=8)
plot(agg.avg, which.plots=1)

# Next, add standardization

agg.avg.Z <- agnes(x=mydata, metric="euclidean", method="average", stand=TRUE)
agg.avg.Z
summary(agg.avg.Z)
win.graph(h=7, w=15, pointsize=8)
plot(agg.avg.Z, which.plots=2)

# Now single linkage, without standardization
agg.sin <- agnes(x=mydata, metric="euclidean", method="single")
agg.sin
summary(agg.sin)
win.graph(h=7, w=15, pointsize=8)
plot(agg.sin, which.plots=2)

# Single linkage with standardization
agg.sin.Z <- agnes(x=mydata, metric="euclidean", method="single", stand=TRUE)
agg.sin.Z
summary(agg.sin.Z)
win.graph(h=7, w=15, pointsize=8)
plot(agg.sin.Z, which.plots=2)

# Complete linkage without standardization
agg.com <- agnes(x=mydata, metric="euclidean", method="complete")
agg.com
summary(agg.com)
win.graph(h=7, w=15, pointsize=8)
plot(agg.com, which.plots=2)

# Complete linkage with standardization
agg.com.Z <- agnes(x=mydata, metric="euclidean", method="complete", stand=TRUE)
agg.com.Z
summary(agg.com.Z)
win.graph(h=7, w=15, pointsize=8)
plot(agg.com.Z, which.plots=2)

#JUST FOR FUN: Compare clusters to classes to see whether the method
#  picks up the same information as a classification method

cut.agg.avg5 <- cutree(agg.avg, k=5)
table(cut.agg.avg5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.avg10 <- cutree(agg.avg, k=10)
table(cut.agg.avg10, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.avgz5 <- cutree(agg.avg.Z, k=5)
table(cut.agg.avgz5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.avgz10 <- cutree(agg.avg.Z, k=10)
table(cut.agg.avgz10, vehdata$class,  dnn=c("Cluster","Class"))

cut.agg.sin5 <- cutree(agg.sin, k=5)
table(cut.agg.sin5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.sin10 <- cutree(agg.sin, k=10)
table(cut.agg.sin10, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.sinz5 <- cutree(agg.sin.Z, k=5)
table(cut.agg.sinz5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.sinz10 <- cutree(agg.sin.Z, k=10)
table(cut.agg.sinz10, vehdata$class,  dnn=c("Cluster","Class"))

cut.agg.com5 <- cutree(agg.com, k=5)
table(cut.agg.com5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.com10 <- cutree(agg.com, k=10)
table(cut.agg.com10, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.comz5 <- cutree(agg.com.Z, k=5)
table(cut.agg.comz5, vehdata$class,  dnn=c("Cluster","Class"))
cut.agg.comz10 <- cutree(agg.com.Z, k=10)
table(cut.agg.comz10, vehdata$class,  dnn=c("Cluster","Class"))

# Look at cluster characteristics

for(j in 1:ncol(mydata)){
  x11()
  boxplot(mydata[,j] ~ cut.agg.comz5, 
          main=paste("Comparison of clusters for", colnames(mydata)[j]))
}
  





