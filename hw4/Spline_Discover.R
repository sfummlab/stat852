library(datasets)
library(reshape2)
disc <- data.frame(Y=as.matrix(discoveries), date=time(discoveries))
disc.ord <- disc[order(disc$date),]
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc.ord$Y, main="natural regression splines", col="gray")

legend(x=1940, y=11, legend=c("df5","df10","df15","df20","df40","df99"), lty="solid", col=colors()[c(24,121,145,80,160,100)], lwd=2)
library(splines)
ns(disc$date,df=5)
nat.spl.5 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=5))
summary(nat.spl.5)
lines(x=disc.ord$date, y=predict(nat.spl.5, newdata=disc.ord), col=colors()[24], lwd=2)


ns(disc$date,df=10)
nat.spl.10 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=10))
summary(nat.spl.10)
lines(x=disc.ord$date, y=predict(nat.spl.10, newdata=disc.ord), col=colors()[121], lwd=2)


ns(disc$date,df=15)
nat.spl.15 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=15))
summary(nat.spl.15)
lines(x=disc.ord$date, y=predict(nat.spl.15, newdata=disc.ord), col=colors()[145], lwd=2)

ns(disc$date,df=20)
nat.spl.20 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=20))
summary(nat.spl.20)
lines(x=disc.ord$date, y=predict(nat.spl.20, newdata=disc.ord), col=colors()[80], lwd=2)

ns(disc$date,df=40)
nat.spl.40 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=40))
summary(nat.spl.40)
lines(x=disc.ord$date, y=predict(nat.spl.40, newdata=disc.ord), col=colors()[160], lwd=2)

ns(disc$date,df=99)
nat.spl.99 <- lm(data=disc.ord, disc.ord$Y ~ ns(disc.ord$date,df=99))
summary(nat.spl.99)
lines(x=disc.ord$date, y=predict(nat.spl.99, newdata=disc.ord), col=colors()[100], lwd=2)


quartz(h=7,w=6,pointsize=11)
plot(x=disc.ord$date,y=disc.ord$Y, main="Smooth regression splines", col="gray")

legend(x=1940, y=11, legend=c("df1","df2","df3","df4","df5"), lty="solid", col=colors()[c(24,121,145,84)], lwd=2)

# Smoothing spline using smooth.spline()
# Note: Can specify DF.  
# If not specified, Generalized Crossvalidation is used to find "best" lambda 
#   and estimate equivalent DF,

sm.spl.3 <- smooth.spline(x=disc.ord$date, y=disc.ord$Y, df=3)
sm.spl.3
lines(sm.spl.3, col=colors()[121], lwd=2)


sm.spl.5 <- smooth.spline(x=disc.ord$date, y=disc.ord$Y, df=5)
sm.spl.5
lines(sm.spl.5, col=colors()[121], lwd=2)

sm.spl.10 <- smooth.spline(x=disc.ord$date, y=disc.ord$Y, df=10)
sm.spl.5
lines(sm.spl.10, col=colors()[121], lwd=2)



# Optimal Spline.  
#   "cv=TRUE" uses N-fold CV.  NOT RECOMMENDED IF DUPLICATE VALUES OF X EXIST
#   "CV=FALSE" uses generalized CV (GCV)

# IN THIS EXAMPLE, GCV Doesn't work well.  N-Fold *seems* to do something reasonable.
quartz(h=7,w=6)
plot(x=disc.ord$date,y=disc.ord$Y, main="Comparisons of 'Optimum' Smoothing splines", col="gray")

legend(x=1910, y=11, legend=c("N-fold","Generalized CV"), lty="solid", col=colors()[c(91,121)], lwd=2)
sm.spl.opt <- smooth.spline(x=disc.ord$date, y=disc.ord$Y,  cv=TRUE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[91], lwd=2)

sm.spl.opt2 <- smooth.spline(x=disc.ord$date, y=disc.ord$Y,  cv=FALSE)
sm.spl.opt2
lines(sm.spl.opt2, col=colors()[121], lwd=2)



