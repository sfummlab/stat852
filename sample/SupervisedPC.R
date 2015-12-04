# Supervised Principal Components Regression Analysis
#   Using package superPC
#   Detergent data from Izenman
#   12 observations of 5 response variables and 1168 explanatories.

library(MMST)

data(detergent)
dim(detergent)
y1 <- detergent[,1:5]
x1 <- detergent[,-c(1:5)]

library(randomForest)

rf0 <- randomForest(x=x1, y=y1[,1])
x11()
plot(x=y1[,1], y=predict(rf0), xlim=c(12,28), ylim=c(12,28))
abline(a=0, b=1)

(oob0 <- mean((predict(rf0) - y1[,1])^2))


library(glmnet)

las0 <- cv.glmnet(x=as.matrix(x1), y=y1[,1], nfolds=nrow(x1))

x11()
plot(las0) # Plot CV-MSPE
coef(las0)[1:5] 
cbind(coef(las0)@i, coef(las0)@x)

x11()
plot(x=y1[,1], y=predict(las0, newx=as.matrix(x1)), xlim=c(12,28), ylim=c(12,28))
abline(a=0, b=1)

(cvlas0 <- las0$cvm[which(las0$lambda==las0$lambda.1se)])


#####
# First look at the PCs just for fun
#
pc <-  prcomp(x=x1, scale.=TRUE)
summary(pc)

# Scree Plot
win.graph(h=7,w=7)
evals <- pc$sdev^2
plot(y=evals, x=c(1:min(ncol(x1), nrow(x1))))
abline(a=0,b=0)
abline(a=1,b=0)

# Looks like 4 or 6

# Try *unsupervised* PC to see what happens

x1pc <- pc$x

# Refit RF
rf1.4 <- randomForest(x=x1pc[,1:4], y=y1[,1])
x11()
plot(x=y1[,1], y=predict(rf1.4), xlim=c(12,28), ylim=c(12,28))
abline(a=0, b=1)

(oob1.4 <- mean((predict(rf1.4) - y1[,1])^2))


# Refit LASSO
las1.4 <- cv.glmnet(x=as.matrix(x1pc[,1:4]), y=y1[,1], nfolds=nrow(x1pc))

x11()
plot(las1.4) # Plot CV-MSPE
cbind(coef(las1.4)@i, coef(las1.4)@x)

x11()
plot(x=y1[,1], y=predict(las1.4, newx=as.matrix(x1pc[,1:4])), xlim=c(12,28), ylim=c(12,28))
abline(a=0, b=1)

which(las1.4$lambda==las1.4$lambda.1se)

(cvlas <- las1.4$cvm[which(las1.4$lambda==las1.4$lambda.1se)])

######################################################################
# Since I couldn't get superpc to work for me, trying my own script

# Sets of p-value thresholds and principal components to try
pval <- c(1,5,10,20,40)/40
pcno <- 2:6

MS <- matrix(NA, ncol = 4, nrow = length(pval)*length(pcno))

# Start CV loop

qq <- 1
# Loop over all combinations of thresholds and numbers of PCs
for(crit in pval){
  for(p in pcno){
    # Set the prediction errors to zero
    msrfi <- 0
    mslasi <- 0
    # Begin CV loop
    for(i in 1:nrow(x1)){
      # Remove observation i
      x1i <- x1[-i,]
      y1i <- y1[-i,1]
      # Begin sure independence screening
      screen <- matrix(data=NA, nrow=ncol(x1i), ncol=2)
      # Compute p-values for linear regressions
      for(j in 1:ncol(x1i)){
        fit <- lm(y1i ~ x1i[,j])
        a <- anova(fit)
        screen[j,] <- c(a$"F value"[1], a$"Pr(>F)"[1]) 
      }
      # Keep variables below the threshold and compute PCs
      newx1i <- x1i[,which(screen[,2] < crit)]
      pci <-  prcomp(x=newx1i, scale.=TRUE)
      # Run RF and LASSO on different numbers of PCs and 
      #   compute prediction error on omitted observation.  
      #   Add squared error to existing sum.
      rfi <- randomForest(x=as.matrix(pci$x[,1:p]), y=y1i)
      msrfi <- msrfi + (y1[i,1] - predict(rfi, newdata = predict(pci, newdata=x1[i,])[1:p]))^2
      lasi <- cv.glmnet(x=as.matrix(pci$x[,1:p]), y=y1i, nfolds=nrow(x1i))
      mslasi <- mslasi + (y1[i,1] - predict(lasi, newx = matrix(predict(pci, newdata=x1[i,])[1:p],nrow=1)))^2
    }
    # Store total error for that combination
    MS[qq,]<- c(crit, p, msrfi, mslasi)
    # Increment counter for next row
    qq <- qq + 1
  }
}

MS
# Results: (probably need to lower the threshhold)

#> MS
#        [,1] [,2]     [,3]       [,4]
#  [1,] 0.025    2 120.6654  83.694788
#  [2,] 0.025    3 142.9440  68.147223
#  [3,] 0.025    4 147.6049  20.570507
#  [4,] 0.025    5 167.1877   8.550158
#  [5,] 0.025    6 129.2403   5.587625

#  [6,] 0.125    2 178.0495 120.914458
#  [7,] 0.125    3 181.8408  70.084623
#  [8,] 0.125    4 170.6964  22.780594
#  [9,] 0.125    5 153.1057  16.609730
# [10,] 0.125    6 138.2403   8.884421

# [11,] 0.250    2 178.6058 164.534479
# [12,] 0.250    3 161.6603  97.848436
# [13,] 0.250    4 155.6121  34.852449
# [14,] 0.250    5 149.4509  28.702178
# [15,] 0.250    6 148.5638   9.925039

# [16,] 0.500    2 180.0313 205.697147
# [17,] 0.500    3 136.8118 140.181563
# [18,] 0.500    4 134.3698  84.077944
# [19,] 0.500    5 140.5728  34.247886
# [20,] 0.500    6 138.2017  18.071447

# [21,] 1.000    2 233.4781 223.568250
# [22,] 1.000    3 162.5811 230.930415
# [23,] 1.000    4 155.4357  88.947283
# [24,] 1.000    5 143.1888  31.978196
# [25,] 1.000    6 136.1939  25.180225
#> 
  
# Now can use the chosen tuning parameters (number of PCs and p-value threshold) 
#    for further analysis with the corresponding procedure 

