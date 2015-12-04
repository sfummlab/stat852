# Fitting various univariate spline methods

aaa <- data.frame(pass=AirPassengers, time=seq(1949, 1960.91666666667, 1/12))

win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(2,2))
#plot(y=aaa$pass, x=aaa$time, main="Natural splines", xlim=c(1949,1961))
#legend(x=3, y=40, legend=c("Natural Cubic Spline 3 df", 
#                           "Natural Cubic Spline 7 df", "Natural Cubic Spline, 9 df"), 
#       lty="solid", col=colors()[c(24, 121,145,84)])

library(splines)

# Natural cubic regression spline using ns() to create the smoothing matrix S
# 5 DF spline
nat.spl.5 <- lm(data=aaa, pass ~ ns(time,df=5))
summary(nat.spl.5)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 5df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.5, newdata=aaa), col=colors()[84])

# 10 DF spline
nat.spl.10 <- lm(data=aaa, pass ~ ns(time,df=10))
summary(nat.spl.10)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 10df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.10, newdata=aaa), col=colors()[84])

# 15 DF spline
nat.spl.15 <- lm(data=aaa, pass ~ ns(time,df=15))
summary(nat.spl.15)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 15df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.15, newdata=aaa), col=colors()[84])

# 20 DF spline
nat.spl.20 <- lm(data=aaa, pass ~ ns(time,df=20))
summary(nat.spl.20)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 20df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.20, newdata=aaa), col=colors()[84])

win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(2,2))

# 30 DF spline
nat.spl.30 <- lm(data=aaa, pass ~ ns(time,df=30))
summary(nat.spl.30)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 30df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.30, newdata=aaa), col=colors()[84])

# 40 DF spline
nat.spl.40 <- lm(data=aaa, pass ~ ns(time,df=40))
summary(nat.spl.40)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 40df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.40, newdata=aaa), col=colors()[84])

# 50 DF spline
nat.spl.50 <- lm(data=aaa, pass ~ ns(time,df=50))
summary(nat.spl.50)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 50df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.50, newdata=aaa), col=colors()[84])

# 60 DF spline
nat.spl.60 <- lm(data=aaa, pass ~ ns(time,df=60))
summary(nat.spl.60)
plot(y=aaa$pass, x=aaa$time, main="Natural splines 60df", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(nat.spl.60, newdata=aaa), col=colors()[84])


# Next try smoothing splines

win.graph(h=7,w=6)
plot(y=aaa$pass, x=aaa$time, main="Optimal Smoothing Spline", xlim=c(1949,1961), col=colors()[24])

# Smoothing spline using smooth.spline()
# Note: Can specify DF.  
# If not specified, Generalized Crossvalidation is used to find "best" lambda 
#   and estimate equivalent DF,

# Default spline
sm.spl <- smooth.spline(x=aaa$time, y=aaa$pass)
sm.spl
lines(sm.spl, col=colors()[84])




# Now use Kernel Smoothers.  Start with LOESS.
#  Uses loess{stats}.  Default degree=2, span=.75, Tri-cube kernel
#Use all the data (span=proportion of data used in window)


win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(4,1))

lo.100 <- loess(data=aaa, pass ~ time, span=1)
summary(lo.100)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 100% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.100, newdata=aaa), col=colors()[84])

# 75% span = default  DF spline
lo.75 <- loess(data=aaa, pass ~ time, span=.75)
summary(lo.75)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 75% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.75, newdata=aaa), col=colors()[84])

# 50% span = default  DF spline
lo.50 <- loess(data=aaa, pass ~ time, span=.5)
summary(lo.50)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 50% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.50, newdata=aaa), col=colors()[84])


lo.25 <- loess(data=aaa, pass ~ time, span=.25)
summary(lo.25)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 25% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.25, newdata=aaa), col=colors()[84])



win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(4,1))


lo.20 <- loess(data=aaa, pass ~ time, span=.2)
summary(lo.20)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 20% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.20, newdata=aaa), col=colors()[84])

lo.15 <- loess(data=aaa, pass ~ time, span=.15)
summary(lo.15)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 15% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.15, newdata=aaa), col=colors()[84])

lo.10 <- loess(data=aaa, pass ~ time, span=.1)
summary(lo.10)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 10% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.10, newdata=aaa), col=colors()[84])

lo.05 <- loess(data=aaa, pass ~ time, span=.05)
summary(lo.05)
plot(y=aaa$pass, x=aaa$time, main="LOESS Deg 2, 05% span", xlim=c(1949,1961), col=colors()[24])
lines(x=aaa$time, y=predict(lo.05, newdata=aaa), col=colors()[84])


#############################################################################
# Repeat using Normal kernel via locpoly{KernSmooth}
#############################################################################

library(KernSmooth)
# bandwidth=variance of kernel.


win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(4,1))
#
lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=4, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=4", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=1, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=1", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.5, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=.5", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.25, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=.25", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])


win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(4,1))
#
lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=4, degree=1)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 1, BW=4", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=1, degree=1)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 1, BW=1", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.5, degree=1)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 1, BW=.5", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.25, degree=1)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 1, BW=.25", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])



win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(4,1))
#
lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=4, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=4", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=1, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=1", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.5, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=.5", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.5, col=colors()[84])

lp.1.1 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=.25, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=.25", xlim=c(1949,1961), col=colors()[24])
lines(lp.1.1, col=colors()[84])


win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(5,1))
#
plot(AirPassengers)

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=2/12, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=2/12", xlim=c(1949,1961), col=colors()[235])
lines(lp.1.5, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=1/12, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2, BW=1/12", xlim=c(1949,1961), col=colors()[235])
lines(lp.1.5, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=2/12, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=2/12", xlim=c(1949,1961), col=colors()[235])
lines(lp.1.5, col=colors()[84])

lp.1.5 <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=1/12, degree=3)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 3, BW=1/12", xlim=c(1949,1961), col=colors()[235])
lines(lp.1.5, col=colors()[84])

win.graph(h=16, w=16, pointsize=12)
par(mfrow=c(2,1))
#
plot(AirPassengers)
# "Optimal" selection of bandwith 
lambda = dpill(x=aaa$time, y=aaa$pass)
lambda
lp.1.opt <- locpoly(x=aaa$time, y=aaa$pass, bandwidth=lambda, degree=2)
plot(y=aaa$pass, x=aaa$time, main="Kernel Deg 2 Optimal", xlim=c(1949,1961), col=colors()[235])
lines(lp.1.opt, col=colors()[84])
