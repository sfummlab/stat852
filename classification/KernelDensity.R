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

###############################################################
#### The density() function does kernel density estimation. 
####   Bandwidth, bw=, is the SD of the kernel
#### Also, the package sm has a function that can be used to 
####   quickly compare densities across classes. 
###############################################################

kd1.1 <- density(set1[which(set1$class==1),1], kernel="gaussian", bw=1)
kd1.2 <- density(set1[which(set1$class==1),1], kernel="gaussian", bw=2)
kd1.4 <- density(set1[which(set1$class==1),1], kernel="gaussian", bw=4)
kd1.8 <- density(set1[which(set1$class==1),1], kernel="gaussian", bw=8)

quartz(h=7,w=6,pointsize=12)
plot(kd1.1, main="Gaussian Kernel", col=colors()[53], lwd=2)
lines(kd1.2, col=colors()[68], lwd=2)
lines(kd1.4, col=colors()[203], lwd=2)
lines(kd1.8, col=colors()[464], lwd=2)
rug(jitter(set1[which(set1$class==1),1]))
legend(x = 75, y = 0.06, legend = c("BW 1", "BW 2", "BW 4", "BW 8"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")

kd1.1e <- density(set1[which(set1$class==1),1], kernel="epanechnikov", bw=1)
kd1.2e <- density(set1[which(set1$class==1),1], kernel="epanechnikov", bw=2)
kd1.4e <- density(set1[which(set1$class==1),1], kernel="epanechnikov", bw=4)
kd1.8e <- density(set1[which(set1$class==1),1], kernel="epanechnikov", bw=8)

quartz(h=7,w=6,pointsize=12)
plot(kd1.1e, main="Epanechnikov Kernel", col=colors()[53], lwd=2)
lines(kd1.2e, col=colors()[68], lwd=2)
lines(kd1.4e, col=colors()[203], lwd=2)
lines(kd1.8e, col=colors()[464], lwd=2)
rug(jitter(set1[which(set1$class==1),1]))
legend(x = 75, y = 0.06, legend = c("BW 1", "BW 2", "BW 4", "BW 8"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")


kdc1.4 <- density(set1[which(set1$class==1),1], kernel="gaussian", bw=4)
kdc2.4 <- density(set1[which(set1$class==2),1], kernel="gaussian", bw=4)
kdc3.4 <- density(set1[which(set1$class==3),1], kernel="gaussian", bw=4)
kdc4.4 <- density(set1[which(set1$class==4),1], kernel="gaussian", bw=4)

quartz(h=7,w=6,pointsize=12)
plot(kdc1.4, main="Gaussian Kernel", col=colors()[53], ylim=c(0,0.08), lwd=2)
lines(kdc2.4, col=colors()[68], lwd=2)
lines(kdc3.4, col=colors()[203], lwd=2)
lines(kdc4.4, col=colors()[464], lwd=2)
legend(x = 70, y = 0.08, legend = c("Class 1", "Class 2", "Class 3", "Class 4"), lty = "solid",
       col=c(colors()[c(53,68,203,464)]), cex=0.8, bty="n")


###########################################################
### Density comparison across groups: automatic!
###########################################################
library(sm)
quartz(h=7,w=6,pointsize=12)
sm.density.compare(x=set1[,1], group=set1$class, lwd=2)
