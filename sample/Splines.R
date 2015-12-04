# Fitting various univariate spline methods

carord <- mtcars[order(mtcars$wt),]

# Plot of fuel economy vs. weight of vehicle
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Linear and Quadratic Regression", 
     xlim=c(1,6), ylim=c(5,40))
legend(x=3.5, y=40, legend=c("Linear reg", "Quadratic reg"), lty="solid",
       col=colors()[c(24,121)])

# Add linear reg to plot
linreg <- lm(data=carord, mpg ~ wt)
summary(linreg)
lines(x=carord$wt, y=predict(linreg, newdata=carord), col=colors()[24])
# Add Quadratic Reg to plot
quadreg <- lm(data=carord, mpg ~ wt + I(wt^2))
summary(quadreg)
lines(x=carord$wt, y=predict(quadreg, newdata=carord), col=colors()[121])

# Now use splines.  Will try three different basis (cubic) splines:
#   5df (2 knots), 7df (4 knots), and 9df (6 knots)
# First preparing a new plot
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Cubic regression splines",
     xlim=c(1,6), ylim=c(5,40))
legend(x=3.5, y=40, legend=c("Linear regression", "Cubic Spline 5 df", 
                             "Cubic Spline 7 df", "Cubic Spline, 9 df"), 
       lty="solid", col=colors()[c(24,121,145,84)])
lines(x=carord$wt, y=predict(linreg, newdata=carord), col=colors()[24])

# Now start fitting splines.  
library(splines)
# Cubic regression spline using bs() to create the smoothing matrix S
# 5 DF spline
bs(carord$wt,df=5)
cub.spl.5 <- lm(data=carord, mpg ~ bs(carord$wt,df=5))
summary(cub.spl.5)
lines(x=carord$wt, y=predict(cub.spl.5, newdata=carord), col=colors()[121])

# 7 DF spline
bs(carord$wt,df=7)
cub.spl.7 <- lm(data=carord, mpg ~ bs(carord$wt,df=7))
summary(cub.spl.7)
lines(x=carord$wt, y=predict(cub.spl.7, newdata=carord), col=colors()[145])

# 9 DF spline
bs(carord$wt,df=9)
cub.spl.9 <- lm(data=carord, mpg ~ bs(carord$wt,df=9))
summary(cub.spl.9)
lines(x=carord$wt, y=predict(cub.spl.9, newdata=carord), col=colors()[84])

# Repeat using natural splines
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg,  main="Natural splines",
 xlim=c(1,6), ylim=c(5,40))
legend(x=3, y=40, legend=c("Linear Regression", "Natural Cubic Spline 5 df", 
                           "Natural Cubic Spline 7 df", "Natural Cubic Spline, 9 df"), 
       lty="solid", col=colors()[c(24, 121,145,84)])
lines(x=carord$wt, y=predict(linreg, newdata=carord), col=colors()[24])

# Natural cubic regression spline using ns() to create the smoothing matrix S
# 5 DF spline
ns(carord$wt,df=5)
nat.spl.5 <- lm(data=carord, mpg ~ ns(carord$wt,df=5))
summary(nat.spl.5)
lines(x=carord$wt, y=predict(nat.spl.5, newdata=carord), col=colors()[121])

# 7 DF spline
ns(carord$wt,df=7)
nat.spl.7 <- lm(data=carord, mpg ~ ns(carord$wt,df=7))
summary(nat.spl.7)
lines(x=carord$wt, y=predict(nat.spl.7, newdata=carord), col=colors()[145])

# 9 DF spline
ns(carord$wt,df=9)
nat.spl.9 <- lm(data=carord, mpg ~ ns(carord$wt,df=9))
summary(nat.spl.9)
lines(x=carord$wt, y=predict(nat.spl.9, newdata=carord), col=colors()[84])

# Next try smoothing splines

win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg,  main="Smoothing splines",
 xlim=c(1,6), ylim=c(5,40))
legend(x=3, y=40, legend=c("Linear Regression", "Smoothing Spline 5 df", 
                           "Smoothing Spline 3 df", "Smoothing Spline, 7 df"), 
       lty="solid", col=colors()[c(24, 121,145,84)])
lines(x=carord$wt, y=predict(linreg, newdata=carord), col=colors()[24])

# Smoothing spline using smooth.spline()
# Note: Can specify DF.  
# If not specified, Generalized Crossvalidation is used to find "best" lambda 
#   and estimate equivalent DF,

# 5 DF spline
sm.spl.5 <- smooth.spline(x=carord$wt, y=carord$mpg, df=5)
sm.spl.5
lines(sm.spl.5, col=colors()[121])

# 3 DF spline
sm.spl.3 <- smooth.spline(x=carord$wt, y=carord$mpg, df=3)
sm.spl.3
lines(sm.spl.3, col=colors()[145])

# 7 DF spline
sm.spl.7 <- smooth.spline(x=carord$wt, y=carord$mpg, df=7)
sm.spl.7
lines(sm.spl.7, col=colors()[84])

# Optimal Spline.  
#   "cv=TRUE" uses N-fold CV.  NOT RECOMMENDED IF DUPLICATE VALUES OF X EXIST
#   "CV=FALSE" uses generalized CV (GCV)

# IN THIS EXAMPLE, GCV FAILS!  N-Fold does fine.
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg,  main="Comparison of 'Optimum' Smoothing splines",
 xlim=c(1,6), ylim=c(5,40))
legend(x=3, y=40, legend=c("N-Fold CV", "Generalized CV"), 
       lty="solid", col=colors()[c(121,91)])
sm.spl.opt <- smooth.spline(x=carord$wt, y=carord$mpg, cv=TRUE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[121])

sm.spl.opt <- smooth.spline(x=carord$wt, y=carord$mpg, cv=FALSE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[91])



# Comparison of 5df Splines
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$mpg, main="Comparison of splines with 5df", xlim=c(1,6), ylim=c(5,40))
legend(x=3, y=40, legend=c("Quadratic Regression", "Cubic reg Spline", "Natural Spline", 
                             "Smoothing Spline"), 
       lty="solid", col=colors()[c(24, 121,145,84)])
lines(x=carord$wt, y=predict(quadreg, newdata=carord), col=colors()[24])
lines(x=carord$wt, y=predict(cub.spl.5, newdata=carord), col=colors()[121])
lines(x=carord$wt, y=predict(nat.spl.5, newdata=carord), col=colors()[145])
lines(sm.spl.5, col=colors()[84])


# Sometimes "old-school" methods still work well.  
#  Here we use an inverse transformation, converting to liters per 100 KM.
# Even the splines are more reliable now

carord$lp100 <- 235.214589/carord$mpg
win.graph(h=7,w=6)
plot(x=carord$wt,y=carord$lp100, xlim=c(1,6), ylim=c(0,25))
legend(x=3.5, y=40, legend=c("Linear reg", "Quadratic reg", "Cubic Spline 5df"), lty="solid",
       col=colors()[c(24,121)])

# Add linear reg to plot
linreg.inv <- lm(data=carord, lp100 ~ wt)
summary(linreg.inv)
lines(x=carord$wt, y=predict(linreg.inv, newdata=carord), col=colors()[24])
# Add Quadratic Reg to plot
quadreg.inv <- lm(data=carord, lp100 ~ wt + I(wt^2))
summary(quadreg.inv)
lines(x=carord$wt, y=predict(quadreg.inv, newdata=carord), col=colors()[121])
# 5 DF spline
bs(carord$wt,df=5)
cub.spl.5.inv <- lm(data=carord, lp100 ~ bs(carord$wt,df=5))
summary(cub.spl.5.inv)
lines(x=carord$wt, y=predict(cub.spl.5.inv, newdata=carord), col=colors()[145])



