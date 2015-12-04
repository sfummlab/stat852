#vehdata <-  read.table("\\\\ais-fs1.sfu.ca\\home\\users\\tloughin\\documents\\Dropbox\\STAT 890\\R\\vehicle3.txt",header=TRUE,sep=" ")
vehdata <-  read.table("~/stat852/data/vehicle3.txt",header=TRUE,sep=" ")

head(vehdata)
dim(vehdata)

# Scatterplot
 quartz(h=15, w=15)
 pairs(x=vehdata)


# Create 3 sets again: 
# Permuting the numbers 1:n and using these to partition sets

set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

###############################################################
## Principal Component Analysis analysis via stats::prcomp()
## Uses singular value decomposition on X instead of eigen-decomposition
##   on X'X for efficiency.  [princomp() uses eigen on X'X]
##  Default is to work with un-standardized data; scale.=TRUE fixes this
###############################################################

pc <-  prcomp(x=set1[,-19], scale.=TRUE)
summary(pc)

# Default plot is a scree plot as a histogram (why???)
# I'll make my own using points
quartz(h=7,w=12)
par(mfrow=c(1,2))
plot(pc)

evals <- pc$sdev^2
plot(y=evals, x=c(1:18))
abline(a=0,b=0)
abline(a=1,b=0)

# Look at eigenvectors to see how variables contribute to PC's
pc$rotation

# Plotting a few of the PC's
#**********I Wasn't expecting to see these results!!!
quartz(h=15,w=12)
par(mfrow=c(3,2))
plot(x=pc$x[,1], y=pc$x[,2])
plot(x=pc$x[,2], y=pc$x[,3])
plot(x=pc$x[,1], y=pc$x[,3])
plot(x=pc$x[,4], y=pc$x[,5])
plot(x=pc$x[,1], y=pc$x[,5])
plot(x=pc$x[,9], y=pc$x[,10])


