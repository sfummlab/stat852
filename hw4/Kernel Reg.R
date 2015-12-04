#############################################################################
# Fitting various univariate spline methods
#############################################################################

carord <- mtcars[order(mtcars$wt),]

# Plot data and add linear regression
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Loess Kernel Smoothers",
     xlim=c(1,6), ylim=c(5,40))
legend(x=3.5, y=40, legend=c("Linear regression", "Loess 100% span", 
                             "Loess 75% span", "Loess 50% span"), 
       lty="solid", col=colors()[c(24,121,145,84)])

# Add linear reg to plot
linreg <- lm(data=carord, mpg ~ wt)
summary(linreg)
lines(x=carord$wt, y=predict(linreg, newdata=carord), col=colors()[24])

# Now use Kernel Smoothers.  Start with LOESS.
#  Uses loess{stats}.  Default degree=2, span=.75, Tri-cube kernel
#Use all the data (span=proportion of data used in window)
lo.100 <- loess(data=carord, mpg ~ wt, span=1)
summary(lo.100)
lines(x=carord$wt, y=predict(lo.100, newdata=carord), col=colors()[121])

# 75% span = default  DF spline
lo.75 <- loess(data=carord, mpg ~ wt)
summary(lo.75)
lines(x=carord$wt, y=predict(lo.75, newdata=carord), col=colors()[145])

# 50% span = default  DF spline
lo.50 <- loess(data=carord, mpg ~ wt, span=.50)
summary(lo.50)
lines(x=carord$wt, y=predict(lo.50, newdata=carord), col=colors()[84])

#############################################################################
# Repeat with different polynomial degrees and the default 75% span.
# Plot data 
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Loess Kernel Smoothers, 75% span",
     xlim=c(1,6), ylim=c(5,40))
legend(x=3.5, y=40, legend=c("degree=1", "degree=2"), 
       lty="solid", col=colors()[c(121,145)])

# Degree 1 local polynomial
lo.d1 <- loess(data=carord, mpg ~ wt, degree=1)
summary(lo.d1)
lines(x=carord$wt, y=predict(lo.d1, newdata=carord), col=colors()[121])

# Degree 2 local polynomial
lo.d2 <- loess(data=carord, mpg ~ wt, degree=2)
summary(lo.d2)
lines(x=carord$wt, y=predict(lo.d2, newdata=carord), col=colors()[145])

#############################################################################
# Alternative specification of equivalent number of parameters
# Plot data 
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Loess Kernel Smoothers, Equiv. No. Params.",
     xlim=c(1,6), ylim=c(5,40))
legend(x=3.5, y=40, legend=c("enp=3", "enp=5", "enp=9"), 
       lty="solid", col=colors()[c(121,145,84)])

# Equivalent number of parameters=3
lo.enp3 <- loess(data=carord, mpg ~ wt, enp.target=3)
summary(lo.enp3)
lines(x=carord$wt, y=predict(lo.enp3, newdata=carord), col=colors()[121])

# Equivalent number of parameters=5
lo.enp5 <- loess(data=carord, mpg ~ wt, enp.target=5)
summary(lo.enp5)
lines(x=carord$wt, y=predict(lo.enp5, newdata=carord), col=colors()[145])

# Equivalent number of parameters=9
lo.enp9 <- loess(data=carord, mpg ~ wt, enp.target=9)
summary(lo.enp9)
lines(x=carord$wt, y=predict(lo.enp9, newdata=carord), col=colors()[84])


#############################################################################
# Repeat using Normal kernel via locpoly{KernSmooth}
#############################################################################
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg,  main="Normal Kernel Smoother",
 xlim=c(1,6), ylim=c(5,40))
legend(x=3, y=40, legend=c("Bandwidth SD = .5", 
                           "Bandwidth SD = 1", "Bandwidth = Optimal"), 
       lty="solid", col=colors()[c(121,145,84)])
     
library(KernSmooth)
# bandwidth=variance of kernel.
#
lp.1.5 <- locpoly(x=carord$wt, y=carord$mpg, bandwidth=.25, degree=1)
lines(lp.1.5, col=colors()[121])

lp.1.1 <- locpoly(x=carord$wt, y=carord$mpg, bandwidth=1, degree=1)
lines(lp.1.1, col=colors()[145])

# "Optimal" selection of bandwith 
lambda = dpill(x=carord$wt, y=carord$mpg)
lambda
lp.1.opt <- locpoly(x=carord$wt, y=carord$mpg, bandwidth=lambda, degree=1)
lines(lp.1.opt, col=colors()[84])
