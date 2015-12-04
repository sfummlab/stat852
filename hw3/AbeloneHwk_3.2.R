# Bayesian model averaging via BIC using BMA package
abelone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

colnames(abelone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abelone$sex.m = ifelse(abelone$Sex=="M", yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex=="F", yes=1, no=0)

# Scatterplot matrix of all but nominal variable

quartz(h=12, w=12)
pairs(x=abelone[,-1])

# Add interactions with sex and the log of each variable
abelone$sexm.len  <- abelone$sex.m * abelone$Length
abelone$sexm.diam <- abelone$sex.m * abelone$Diameter
abelone$sexm.hgt  <- abelone$sex.m * abelone$Height
abelone$sexm.whol <- abelone$sex.m * abelone$Whole
abelone$sexm.shuc <- abelone$sex.m * abelone$Shucked
abelone$sexm.vic  <- abelone$sex.m * abelone$Viscera
abelone$sexm.shel <- abelone$sex.m * abelone$Shell
abelone$sexf.len  <- abelone$sex.f * abelone$Length
abelone$sexf.diam <- abelone$sex.f * abelone$Diameter
abelone$sexf.hgt  <- abelone$sex.f * abelone$Height
abelone$sexf.whol <- abelone$sex.f * abelone$Whole
abelone$sexf.shuc <- abelone$sex.f * abelone$Shucked
abelone$sexf.vic  <- abelone$sex.f * abelone$Viscera
abelone$sexf.shel <- abelone$sex.f * abelone$Shell
abelone$loglen  <- log(abelone$Length)
abelone$logdiam <- log(abelone$Diameter)
abelone$loghgt <- log(abelone$Height)
abelone$logwhol<- log(abelone$Whole)
abelone$logshuc<- log(abelone$Shucked)
abelone$logvic <- log(abelone$Viscera)
abelone$logshel<- log(abelone$Shell)

#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test

set.seed(29003092)
U <- runif(n=nrow(abelone))
abelone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))
abelone <- abelone[,-1] # Dump old Sex variable
abelone <- abelone[is.finite(rowSums(abelone)),]
#head(abelone)

x.1 <- as.matrix(abelone[which(abelone$set==1),-8])
y.1 <- as.matrix(abelone[which(abelone$set==1),8])
x.23 <- as.matrix(abelone[which(abelone$set!=1),-8])
y.23 <- as.matrix(abelone[which(abelone$set!=1),8])

# Model averaging a linear model is done with bicreg().  
# Other functions can handle other regression models
#  Calculations are based on leaps() all-subsets regression with BIC on each model
#  Probabilities are found as shown in lecture notes
# 

library(BMA)

mod.avg1 <- bicreg(x = x.1, y = y.1, strict = FALSE, OR = 80)

summary(mod.avg1)

print(mod.avg1)

quartz(h=15, w=12, pointsize=12)
par(mfrow=c(2,2))
plot(mod.avg1)


quartz(h=7, w=6, pointsize=12)
imageplot.bma(mod.avg1)
