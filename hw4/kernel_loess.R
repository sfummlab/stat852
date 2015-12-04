library(datasets)
library(reshape2)
disc <- data.frame(Y=as.matrix(discoveries), date=time(discoveries))
disc.ord <- disc[order(disc$date),]
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc.ord$Y, main="Kernel smooth regression", col="gray")

legend(x=1910, y=11, legend=c("Linear regression", "Loess 100% span", 
                              "Loess 75% span", "Loess 50% span"), lty="solid", col=colors()[c(24,121,145,84)], lwd=2)



# Add linear reg to plot
linreg <- lm(data=disc.ord, Y ~ date)
summary(linreg)
lines(x=disc.ord$date, y=predict(linreg, newdata=disc.ord), col=colors()[24])

# Now use Kernel Smoothers.  Start with LOESS.
#  Uses loess{stats}.  Default degree=2, span=.75, Tri-cube kernel
#Use all the data (span=proportion of data used in window)
lo.100 <- loess(data=disc.ord, Y ~ date, span=1)
summary(lo.100)
lines(x=disc.ord$date, y=predict(lo.100, newdata=disc.ord), col=colors()[121])
# 75% span = default  DF spline
lo.75 <- loess(data=disc.ord, Y ~ date)
summary(lo.75)
lines(x=disc.ord$date, y=predict(lo.75, newdata=disc.ord), col=colors()[145])
# 50% span = default  DF spline
lo.50 <- loess(data=disc.ord, Y ~ date, span=.50)
summary(lo.50)
lines(x=disc.ord$date, y=predict(lo.50, newdata=disc.ord), col=colors()[84])
#############################################################################
# Repeat with different polynomial degrees and the default 75% span.
# Plot data 
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc$Y, main="Loess Kernel Smoothers, 25% span",
     xlim=c(1860,1959), ylim=c(0,15))
legend(x=1920, y=10, legend=c("degree=1", "degree=2"), 
       lty="solid", col=colors()[c(121,145)])

# Degree 1 local polynomial
lo.d1 <- loess(data=disc.ord, Y ~ date, degree=1, span = 0.25)
summary(lo.d1)
lines(x=disc.ord$date, y=predict(lo.d1, newdata=disc.ord), col=colors()[121])

# Degree 2 local polynomial
lo.d2 <- loess(data=disc.ord, Y ~ date, degree=2, span = 0.25)
summary(lo.d2)
lines(x=disc.ord$date, y=predict(lo.d2, newdata=disc.ord), col=colors()[145])

#############################################################################
# Alternative specification of equivalent number of parameters
# Plot data 
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc$Y,
main="Loess Kernel Smoothers, Equiv. No. Params.",
     xlim=c(1860,1959), ylim=c(0,15))
legend(x=1860, y=10, legend=c("enp=3", "enp=5", "enp=9"), 
       lty="solid", col=colors()[c(121,145,84)])

# Equivalent number of parameters=3
lo.enp3 <- loess(data=disc.ord, Y ~ date, enp.target=3)
summary(lo.enp3)
lines(x=disc.ord$date, y=predict(lo.d2, newdata=disc.ord), col=colors()[121])

# Equivalent number of parameters=5
lo.enp5 <- loess(data=disc.ord, Y ~ date, enp.target=5)
summary(lo.enp5)
lines(x=disc.ord$date, y=predict(lo.d2, newdata=disc.ord), col=colors()[145])

# Equivalent number of parameters=9
lo.enp9 <- loess(data=disc.ord, Y ~ date, enp.target=9)
summary(lo.enp9)
lines(x=disc.ord$date, y=predict(lo.d2, newdata=disc.ord), col=colors()[84])


#############################################################################
# Repeat using Normal kernel via locpoly{KernSmooth}
#############################################################################
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc$Y,  main="Normal Kernel Smoother",
     lim=c(1860,1959), ylim=c(0,15))
legend(x=1920, y=10, legend=c("Bandwidth SD = .5", 
                           "Bandwidth SD = 1", "Bandwidth = Optimal"), 
       lty="solid", col=colors()[c(121,145,84)])

library(KernSmooth)
# bandwidth=variance of kernel.
#
lp.1.5 <- locpoly(x = disc.ord$date, y = disc.ord$Y,  bandwidth=.25, degree=1)
lines(lp.1.5, col=colors()[121])

lp.1.1 <- locpoly(x = disc.ord$date, y = disc.ord$Y , bandwidth=1, degree=1)
lines(lp.1.1, col=colors()[145])

# "Optimal" selection of bandwith 
lambda = dpill(x=disc.ord$date, y=disc.ord$Y)
lambda
lp.1.opt <- locpoly(x=disc.ord$date, y=disc.ord$Y, kernel = "normal", bandwidth=lambda, degree=1)
lines(lp.1.opt, col=colors()[84])
