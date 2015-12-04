# Partitioning methods (nonhierarchical clustering) 
# Plant Trait Data from cluster package


vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")


## Need to convert data to a numerical matrix.

mydata <- data.matrix(vehdata[,-c(19,20)])
summary(mydata)

mydata.z = scale(mydata)

##################################################################
# kmeans{stats} does k-means clustering.  centers= gives K (# clusters).
#     centers= can also be used to offer initial cluster centers 
#  Recommend trying several starts
##################################################################

# Uncentered data.  Can retry with externally centered data

# SHow a single k-means fit using 10 starts.  
#  (No idea how many is enough.  It is problem-dependent.)
km.fit5 <- kmeans(x=mydata, centers=5, nstart=10)
km.fit5
km.fit5$totss


# Run a function to compute "R-square" for many cluster sizes.
kvals <- c(1:20)
r.sq <- matrix(0,nrow=length(kvals), ncol=2)
row <- 0
for(k in kvals){
  row <- row+1
  fit <- kmeans(x=mydata, centers=k, nstart=5)
  r.sq[row,] <- c(k,fit$betweenss/fit$totss) 
}
r.sq
win.graph(h=7, w=6, pointsize=12)
plot(x=r.sq[,1], y=r.sq[,2], type="b")

#####################
# Repeat for standardized data
######################
  

# SHow a single k-means fit using 10 starts
km.fit5.z <- kmeans(x=mydata.z, centers=5, nstart=10)
km.fit5.z
km.fit5.z$totss

# Run a function to compute "R-square" for many cluster sizes.
kvals <- c(1:50)
r.sq.z <- matrix(0,nrow=length(kvals), ncol=2)
row <- 0
for(k in kvals){
  row <- row+1
  fit <- kmeans(x=mydata.z, centers=k, nstart=5)
  r.sq.z[row,] <- c(k,fit$betweenss/fit$totss) 
}

win.graph(h=7, w=6, pointsize=12)
plot(x=r.sq.z[,1], y=r.sq.z[,2], type="b")

####################################################
# Now show the more-robust Partitioning around medioids (PAM)
#  Naturally this is done in pam{cluster}
#  Mostly the same options as in diana()
####################################################


# First, try 2 clusters, for no reason but simplicity

pam.fit.2 <- pam(x=mydata, k=2, metric="euclidean")
pam.fit.2
summary(pam.fit.2)
win.graph(h=7, w=15, pointsize=12)
plot(pam.fit.2, which.plots=1)
win.graph(h=7, w=15, pointsize=12)
plot(pam.fit.2, which.plots=2)

# Next try 5, for no reason but to see what happens

pam.fit.5 <- pam(x=mydata, k=5, metric="euclidean")
pam.fit.5
summary(pam.fit.5)
win.graph(h=7, w=15, pointsize=10)
plot(pam.fit.5, which.plots=1)
win.graph(h=7, w=15, pointsize=10)
plot(pam.fit.5, which.plots=2)

# Next, add standardization

pam.fit.5.z <- pam(x=mydata, k=5, metric="euclidean", stand=TRUE)
pam.fit.5.z
summary(pam.fit.5.z)
win.graph(h=7, w=15, pointsize=10)
plot(pam.fit.5.z, which.plots=1)
win.graph(h=7, w=15, pointsize=10)
plot(pam.fit.5.z, which.plots=2)


# Run a function to compute average slihouette width for many cluster sizes.
# Takes a few minutes to run
kvals <- c(2:50)
sil.z <- matrix(0,nrow=length(kvals), ncol=2)
row <- 0
for(k in kvals){
  row <- row+1
  fit <- pam(x=mydata.z, k=k, metric="euclidean", stand=TRUE)
  sil.z[row,] <- c(k,fit$silinfo$avg.width) 
}

win.graph(h=7, w=6, pointsize=12)
plot(x=sil.z[,1], y=sil.z[,2], type="b")
