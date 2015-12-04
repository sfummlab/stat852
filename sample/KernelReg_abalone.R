#############################################################################
# Fitting various univariate spline methods
#############################################################################

abalone <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\Homework\\abalone.csv", header=TRUE, sep=",", na.strings=" ")

# Arrange by X for future plotting
aba.ord <- abalone[order(abalone$Shell),]

# Plot data and add linear regression
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Loess Kernel Smoothers", col="gray")
legend(x=0.5, y=7, legend=c("Linear regression", "Loess 100% span", 
                             "Loess 75% span", "Loess 50% span"), 
       lty="solid", col=colors()[c(24,121,145,84)], lwd=2)

# Add linear reg to plot
linreg <- lm(data=aba.ord, Rings ~ Shell)
summary(linreg)
lines(x=aba.ord$Shell, y=predict(linreg, newdata=aba.ord), col=colors()[24], lwd=2)

# Now use Kernel Smoothers.  Start with LOESS.
#  Uses loess{stats}.  Default degree=2, span=.75, Tri-cube kernel
#Use all the data (span=proportion of data used in window)
lo.100 <- loess(data=aba.ord, Rings ~ Shell, span=1)
summary(lo.100)
lines(x=aba.ord$Shell, y=predict(lo.100, newdata=aba.ord), col=colors()[121], lwd=2)

# 75% span = default  DF spline
lo.75 <- loess(data=aba.ord, Rings ~ Shell)
summary(lo.75)
lines(x=aba.ord$Shell, y=predict(lo.75, newdata=aba.ord), col=colors()[145], lwd=2)

# 50% span = default  DF spline
lo.50 <- loess(data=aba.ord, Rings ~ Shell, span=.50)
summary(lo.50)
lines(x=aba.ord$Shell, y=predict(lo.50, newdata=aba.ord), col=colors()[84], lwd=2)

#############################################################################
# Repeat with different polynomial degrees and the default 75% span.
# Plot data 
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Loess Kernel Smoothers, 75% span", , col="gray")
legend(x=0.5, y=7, legend=c("degree=0", "degree=1", "degree=2"), 
       lty="solid", col=colors()[c(84,121,145)], lwd=2)

# Degree 1 local polynomial
lo.d1 <- loess(data=aba.ord, Rings ~ Shell, degree=0)
summary(lo.d1)
lines(x=aba.ord$Shell, y=predict(lo.d1, newdata=aba.ord), col=colors()[84], lwd=2)
# Degree 1 local polynomial
lo.d1 <- loess(data=aba.ord, Rings ~ Shell, degree=1)
summary(lo.d1)
lines(x=aba.ord$Shell, y=predict(lo.d1, newdata=aba.ord), col=colors()[121], lwd=2)
# Degree 2 local polynomial
lo.d2 <- loess(data=aba.ord, Rings ~ Shell, degree=2)
summary(lo.d2)
lines(x=aba.ord$Shell, y=predict(lo.d2, newdata=aba.ord), col=colors()[145], lwd=2)

#############################################################################
# Alternative specification of equivalent number of parameters
# Plot data 
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Loess Kernel Smoothers, Equiv. No. Params.", col="gray")
legend(x=0.5, y=7, legend=c("enp=3", "enp=5", "enp=9"), 
       lty="solid", col=colors()[c(121,145,84)], lwd=2)

# Equivalent number of parameters=3
lo.enp3 <- loess(data=aba.ord, Rings ~ Shell, enp.target=3)
summary(lo.enp3)
lines(x=aba.ord$Shell, y=predict(lo.enp3, newdata=aba.ord), col=colors()[121], lwd=2)

# Equivalent number of parameters=5
lo.enp5 <- loess(data=aba.ord, Rings ~ Shell, enp.target=5)
summary(lo.enp5)
lines(x=aba.ord$Shell, y=predict(lo.enp5, newdata=aba.ord), col=colors()[145], lwd=2)

# Equivalent number of parameters=9
lo.enp9 <- loess(data=aba.ord, Rings ~ Shell, enp.target=9)
summary(lo.enp9)
lines(x=aba.ord$Shell, y=predict(lo.enp9, newdata=aba.ord), col=colors()[84], lwd=2)


#############################################################################
# Repeat using Normal kernel via locpoly{KernSmooth}
#############################################################################
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings,  main="Normal Kernel Smoother", col="gray")
legend(x=0.5, y=7, legend=c("Bandwidth SD = 0.25", 
                           "Bandwidth SD = 0.1", "Bandwidth = Optimal"), 
       lty="solid", col=colors()[c(121,145,84)], lwd=2)
     
library(KernSmooth)
# bandwidth=variance of kernel.
#
lp.1.5 <- locpoly(x=aba.ord$Shell, y=aba.ord$Rings, bandwidth=.25, degree=1)
lines(lp.1.5, col=colors()[121], lwd=2)

lp.1.1 <- locpoly(x=aba.ord$Shell, y=aba.ord$Rings, bandwidth=.1, degree=1)
lines(lp.1.1, col=colors()[145], lwd=2)

# "Optimal" selection of bandwith 
lambda = dpill(x=aba.ord$Shell, y=aba.ord$Rings)
lambda
lp.1.opt <- locpoly(x=aba.ord$Shell, y=aba.ord$Rings, bandwidth=lambda, degree=1)
lines(lp.1.opt, col=colors()[84], lwd=2)
