# Fitting various univariate spline methods

abalone <-  read.table("C:/Users/Administrator/Downloads/reking-stat852-a4c82c6b9d87/data/abalone.data", header=TRUE, sep=",", na.strings=" ")
colnames(abalone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
# Arrange by X for future plotting
aba.ord <- abalone[order(abalone$Shell),]

# Plot of Rings vs. Shell Weight
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Linear and Quadratic Regression", col="gray")
legend(x=0.5, y=7, legend=c("Linear reg", "Quadratic reg"), lty="solid",
       col=colors()[c(24,121)], lwd=2)

# Add linear reg to plot (1 df model)
linreg <- lm(data=aba.ord, Rings ~ Shell)
summary(linreg)
lines(x=aba.ord$Shell, y=predict(linreg, newdata=aba.ord), col=colors()[24], lwd=2)
# Add Quadratic Reg to plot (2 df mode)
quadreg <- lm(data=aba.ord, Rings ~ Shell + I(Shell^2))
summary(quadreg)
lines(x=aba.ord$Shell, y=predict(quadreg, newdata=aba.ord), col=colors()[121], lwd=2)
# Add Cubic Reg to plot (2 df mode)
cubreg <- lm(data=aba.ord, Rings ~ Shell + I(Shell^2)+ I(Shell^3))
summary(cubreg)
lines(x=aba.ord$Shell, y=predict(cubreg, newdata=aba.ord), col=colors()[145], lwd=2)

# Now use splines.  Will try three different basis (cubic) splines:
#   5df (2 knots), 7df (4 knots), and 9df (6 knots)
# First preparing a new plot
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Cubic regression splines", col="gray")
legend(x=0.5, y=7, legend=c("Linear regression", "Cubic Spline 5 df", 
                             "Cubic Spline 7 df", "Cubic Spline, 9 df"), 
       lty="solid", col=colors()[c(24,121,145,84)], lwd=2)
lines(x=aba.ord$Shell, y=predict(linreg, newdata=aba.ord), col=colors()[24], lwd=2)

# Now start fitting splines.  
library(splines)
# Cubic regression spline using bs() to create the smoothing matrix S
# 5 DF spline
bs(aba.ord$Shell,df=5)
cub.spl.5 <- lm(data=aba.ord, Rings ~ bs(aba.ord$Shell,df=5))
summary(cub.spl.5) #Doesn't mean much
lines(x=aba.ord$Shell, y=predict(cub.spl.5, newdata=aba.ord), col=colors()[121], lwd=2)

# 7 DF spline
bs(aba.ord$Shell,df=7)
cub.spl.7 <- lm(data=aba.ord, Rings ~ bs(aba.ord$Shell,df=7))
summary(cub.spl.7)
lines(x=aba.ord$Shell, y=predict(cub.spl.7, newdata=aba.ord), col=colors()[145], lwd=2)

# 9 DF spline
bs(aba.ord$Shell,df=9)
cub.spl.9 <- lm(data=aba.ord, Rings ~ bs(aba.ord$Shell,df=9))
summary(cub.spl.9)
lines(x=aba.ord$Shell, y=predict(cub.spl.9, newdata=aba.ord), col=colors()[84], lwd=2)

# Repeat using natural splines
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings,  main="Natural splines", col="gray")
legend(x=0.5, y=7, legend=c("Linear Regression", "Nat Cub Spline 5 df", 
                           "Nat Cub Spline 7 df", "Nat Cub Spline, 9 df"), 
       lty="solid", col=colors()[c(24, 121,145,84)], lwd=2)
lines(x=aba.ord$Shell, y=predict(linreg, newdata=aba.ord), col=colors()[24], lwd=2)

# Natural cubic regression spline using ns() to create the smoothing matrix S
# 5 DF spline
ns(aba.ord$Shell,df=5)
nat.spl.5 <- lm(data=aba.ord, Rings ~ ns(aba.ord$Shell,df=5))
summary(nat.spl.5)
lines(x=aba.ord$Shell, y=predict(nat.spl.5, newdata=aba.ord), col=colors()[121], lwd=2)

# 7 DF spline
ns(aba.ord$Shell,df=7)
nat.spl.7 <- lm(data=aba.ord, Rings ~ ns(aba.ord$Shell,df=7))
summary(nat.spl.7)
lines(x=aba.ord$Shell, y=predict(nat.spl.7, newdata=aba.ord), col=colors()[145], lwd=2)

# 9 DF spline
ns(aba.ord$Shell,df=9)
nat.spl.9 <- lm(data=aba.ord, Rings ~ ns(aba.ord$Shell,df=9))
summary(nat.spl.9)
lines(x=aba.ord$Shell, y=predict(nat.spl.9, newdata=aba.ord), col=colors()[84], lwd=2)

# Next try smoothing splines

x11(h=7,w=6,pointsize=11)
plot(x=aba.ord$Shell,y=aba.ord$Rings,  main="Smoothing splines", col="gray")
legend(x=0.5, y=7, legend=c("Linear Regression", "Smoothing Spline 5 df", 
                           "Smoothing Spline 3 df", "Smoothing Spline, 7 df"), 
       lty="solid", col=colors()[c(24, 121,145,84)], lwd=2)
lines(x=aba.ord$Shell, y=predict(linreg, newdata=aba.ord), col=colors()[24], lwd=2)

# Smoothing spline using smooth.spline()
# Note: Can specify DF.  
# If not specified, Generalized Crossvalidation is used to find "best" lambda 
#   and estimate equivalent DF,

# 5 DF spline
sm.spl.5 <- smooth.spline(x=aba.ord$Shell, y=aba.ord$Rings, df=5)
sm.spl.5
lines(sm.spl.5, col=colors()[121], lwd=2)

# 3 DF spline
sm.spl.3 <- smooth.spline(x=aba.ord$Shell, y=aba.ord$Rings, df=3)
sm.spl.3
lines(sm.spl.3, col=colors()[145], lwd=2)

# 7 DF spline
sm.spl.7 <- smooth.spline(x=aba.ord$Shell, y=aba.ord$Rings, df=7)
sm.spl.7
lines(sm.spl.7, col=colors()[84], lwd=2)

# Optimal Spline.  
#   "cv=TRUE" uses N-fold CV.  NOT RECOMMENDED IF DUPLICATE VALUES OF X EXIST
#   "CV=FALSE" uses generalized CV (GCV)

# IN THIS EXAMPLE, GCV Doesn't work well.  N-Fold *seems* to do something reasonable.
x11(h=7,w=6)
plot(x=aba.ord$Shell,y=aba.ord$Rings, col="gray",  main="Comparison of 'Optimum' Smoothing splines")
legend(x=0.5, y=7, legend=c("N-Fold CV", "Generalized CV"), 
       lty="solid", col=colors()[c(121,91)], lwd=2)
sm.spl.opt <- smooth.spline(x=aba.ord$Shell, y=aba.ord$Rings, cv=TRUE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[121], lwd=2)

sm.spl.opt2 <- smooth.spline(x=aba.ord$Shell, y=aba.ord$Rings, cv=FALSE)
sm.spl.opt2
lines(sm.spl.opt2, col=colors()[91], lwd=2)



# Comparison of 5df Splines
x11(h=7,w=6, pointsize=11)
plot(x=aba.ord$Shell,y=aba.ord$Rings, main="Comparison of splines with 5df", col="gray")
legend(x=0.5, y=7, legend=c("Quadratic Regression", "Cubic reg Spline", "Natural Spline", 
                           "Smoothing Spline"), 
       lty="solid", col=colors()[c(24, 121,145,84)], lwd=2)
lines(x=aba.ord$Shell, y=predict(quadreg, newdata=aba.ord), col=colors()[24], lwd=2)
lines(x=aba.ord$Shell, y=predict(cub.spl.5, newdata=aba.ord), col=colors()[121], lwd=2)
lines(x=aba.ord$Shell, y=predict(nat.spl.5, newdata=aba.ord), col=colors()[145], lwd=2)
lines(sm.spl.5, col=colors()[84], lwd=2)



