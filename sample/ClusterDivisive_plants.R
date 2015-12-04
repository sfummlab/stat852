# Hierarchical Clustering
# Plant Trait Data from cluster package

# No data splitting!
# No pairwise scatterplot matrix (too many variables)

library(cluster)


### Note: Most of these data are factors, primarily binary, but a few are ordinal.  
###   I AM TREating them as numerical (0-1 for binary, scores 1...c for ordinal).
###   THIS IS PROBABLY INAPPROPRIATE!  But the data is nicder than others I've tried...
## Need to convert data to a numerical matrix.

mydata <- data.matrix(na.omit((plantTraits)))

##################################################################
# Clustering functions are available in "cluster" package.
#  agnes() does agglomerative clustering
#  diana() does partitioning
###
#  Both have options for different dissimilarities.  
#  In diana(), there are no special linkage options.
###
#  There exist functions that can estimate the optimal number of clusters. 
#    One, in particular, computes p-values.  It is written by a paper author, 
#    and I haven't seen the paper recommended in HTF or IZ, so I don't know whether it is 
#    a good idea.  Read about the "pvclus"" package, see the citations, look them up, 
#    and decide for yourself!
##################################################################

# First, we'll show average linkage and no standardization

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


