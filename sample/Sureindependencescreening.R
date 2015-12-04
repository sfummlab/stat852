vehdata <-  read.table("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Create 3 sets again: 
vehdata$class <- as.factor(vehdata$class)
set.seed(46685326)
perm <- sample(x=nrow(vehdata))
set1 <- vehdata[which(perm<=nrow(vehdata)/2),-20]
set2 <- vehdata[which(nrow(vehdata)/2<perm & perm<=3*nrow(vehdata)/4),-20]
set3 <- vehdata[which(perm>3*nrow(vehdata)/4),-20]

x1 <- set1[,1:18]



###############################################################
## Sure independence screening for a classification problem
##   using multinomial regression. 
##   
###############################################################
library(nnet)
library(car)

# screen is the matrix that will hold the LR test statistics and the p-values.  

screen <- matrix(data=NA, nrow=ncol(x1), ncol=2)

#Don't see how to suppress output. trace=FALSE should do this, 
#  but it doesn't work in the loop.  Why not?

for(j in 1:ncol(x1)){
  logit.fit <- multinom(set1[,19] ~ scale(x1[,j]), Hess = TRUE, trace=FALSE)
  a <- Anova(logit.fit)
  screen[j,] <- c(a$"LR Chisq", a$"Pr(>Chisq)") 
}
summary(screen)

x11()
stripchart(x = screen[,1], method = "jitter")
x11()
stripchart(x = screen[,2], method = "jitter")

# Transform p-values by log to spread them out more; values above 0 are p-values >0.5
logp <- log(2*screen[,2])
x11()
stripchart(x = logp, method = "jitter")

(select.vars <- which(screen[,2] < 0.5))
newx1 <- x1[,select.vars]

# Now I can use the newx1 in building a classifier, but it won't be much different 
#   from the old ones, since only one variable was removed.

##############################################################
# Better example: The class project data!
##############################################################

dat <- read.csv("C:\\Users\\Tom\\Dropbox\\852 Modern Applied Methods\\Project\\Data2014.csv")

x1 <- dat[,-21]

screen <- matrix(data=NA, nrow=ncol(x1), ncol=2)

for(j in 1:ncol(x1)){
  fit <- lm(dat$y ~ x1[,j])
  a <- anova(fit)
  screen[j,] <- c(a$"F value"[1], a$"Pr(>F)"[1]) 
}
summary(screen)

x11() 
stripchart(x = screen[,1], method = "jitter")
x11()
stripchart(x = screen[,2], method = "jitter")

(select.vars <- which(screen[,2] < 0.5))
newx1 <- x1[,select.vars]

library(randomForest)

rf1 <- randomForest(x=x1, y=dat$y)
rf2 <- randomForest(x=newx1, y=dat$y)


write.table(x=predict(rf1, newdata=test), file="C:\\Users\\tloughin\\Dropbox\\852 Modern Applied Methods\\Project\\2014 results\\RFnoscreen.csv", 
            sep=",", row.names = FALSE, col.names = FALSE)
write.table(x=predict(rf2, newdata=test), file="C:\\Users\\tloughin\\Dropbox\\852 Modern Applied Methods\\Project\\2014 results\\RFscreen.csv", 
            sep=",", row.names = FALSE, col.names = FALSE)

#  Try tuning this.

set.seed(61361687)
reps=20 

pval <- c(1:5,10)/10
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MS <- matrix(NA, ncol = reps+1, nrow = length(pval))
MS[,1] <- pval

# Start data resampling loop
for(r in 1:reps){
  # This changes for train/test vs. Boot
  resamp <- sample.int(n=nrow(dat), size=nrow(dat), replace=TRUE)
  datr <- dat[resamp,]
  datp <- dat[-resamp,]

  # Do analysis for each critical value
  x1r <- datr[,-21]
  
  qq <- 1
  for(crit in pval){
    screen <- matrix(data=NA, nrow=ncol(x1r), ncol=2)
    for(j in 1:ncol(x1r)){
      fit <- lm(datr$y ~ x1r[,j])
      a <- anova(fit)
      screen[j,] <- c(a$"F value"[1], a$"Pr(>F)"[1]) 
    }
    newx1r <- x1r[,which(screen[,2] < crit)]
    
    rf <- randomForest(x=newx1r, y=datr$y)  
    MS[qq,r+1]<- mean((datp$y - predict(rf, newdata=datp))^2)
    # Increment counter for next row
    qq <- qq + 1
  }
}

MSmin <- apply(X = MS[,-1], MARGIN=2, FUN = min)
RMSPE <- t(MS[,-1]) / MSmin

x11()
boxplot.matrix(x=MS[,-1], use.cols=FALSE, names=MS[,1])

x11()
boxplot.matrix(x=RMSPE, names=MS[,1])

###################################################
# Screening doesn't seem to help much for the RF.
# create 100 extra worthless variables and add to data
# The resampling loop runs for about 5 minutes.

noisevars <- matrix(rnorm(n=50000), nrow=500)

dat2 <- cbind(noisevars, dat)


x2 <- dat2[,-121]

screen <- matrix(data=NA, nrow=ncol(x2), ncol=2)

for(j in 1:ncol(x2)){
  fit <- lm(dat$y ~ x2[,j])
  a <- anova(fit)
  screen[j,] <- c(a$"F value"[1], a$"Pr(>F)"[1]) 
}
summary(screen)

x11() 
stripchart(x = screen[,1], method = "jitter")
x11()
stripchart(x = screen[,2], method = "jitter")

# Try tuning again
reps=20 

pval <- c(0.1,0.5,1:5,10)/10
# Set up matrices to hold results. First two columns are parameter values.
#  Each column after that is a rep.
MS2 <- matrix(NA, ncol = reps+1, nrow = length(pval))
MS2[,1] <- pval

# Start data resampling loop
for(r in 1:reps){
  # This changes for train/test vs. Boot
  resamp <- sample.int(n=nrow(dat2), size=nrow(dat2), replace=TRUE)
  datr <- dat2[resamp,]
  datp <- dat2[-resamp,]
  
  # Do analysis for each critical value
  x2r <- datr[,-121]
  
  qq <- 1
  for(crit in pval){
    screen <- matrix(data=NA, nrow=ncol(x2r), ncol=2)
    for(j in 1:ncol(x2r)){
      fit <- lm(datr$y ~ x2r[,j])
      a <- anova(fit)
      screen[j,] <- c(a$"F value"[1], a$"Pr(>F)"[1]) 
    }
    newx2r <- x2r[,which(screen[,2] < crit)]
    
    rf <- randomForest(x=newx2r, y=datr$y)  
    MS2[qq,r+1]<- mean((datp$y - predict(rf, newdata=datp))^2)
    # Increment counter for next row
    qq <- qq + 1
  }
}

MS2min <- apply(X = MS2[,-1], MARGIN=2, FUN = min)
RMSPE2 <- t(MS2[,-1]) / MS2min

x11()
boxplot.matrix(x=MS2[,-1], use.cols=FALSE, names=MS2[,1])

x11()
boxplot.matrix(x=RMSPE2, names=MS2[,1])

# Seems like the 0.05 threshold for p-values is best
# Next I could either rerun the RF on the full data 
#   set with screening at 0.05 to train the final model, 
#   or run 
