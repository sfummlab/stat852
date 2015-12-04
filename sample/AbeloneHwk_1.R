abelone <-  read.table("C:\\Users\\Tom Loughin\\Dropbox\\STAT 890\\abelone.csv", header=TRUE, sep=",", na.strings=" ")

abelone$sex.m = ifelse(abelone$Sex==1, yes=1, no=0)
abelone$sex.f = ifelse(abelone$Sex==2, yes=1, no=0)

# Scatterplot matrix of all but nominal variable

win.graph(h=12, w=12)
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

head(abelone)

x.1 <- as.matrix(abelone[which(abelone$set==1),-8])
y.1 <- as.matrix(abelone[which(abelone$set==1),8])
x.2 <- as.data.frame(abelone[which(abelone$set==2),-8])
y.2 <- as.data.frame(abelone[which(abelone$set==2),8])
x.3 <- as.data.frame(abelone[which(abelone$set==3),-8])
y.3 <- as.data.frame(abelone[which(abelone$set==3),8])

initial.1 <- lm(data=abelone[which(abelone$set==1),], 
                formula=Rings~ 1)
final.1 <- lm(data=abelone[which(abelone$set==1),], 
              formula=Rings ~ .)

step1 <- step(object=initial.1, scope=list(upper=final.1), direction="forward",
     k = log(nrow(abelone[which(abelone$set==1),])))
summary(step1)



library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub2 <- regsubsets(x=x.1, y=y.1, nbest=2, nvmax=30)
summary(allsub2)

# Plot of results in a special form
win.graph(h=15, w=12, pointsize=12)
plot(allsub2, main="All Subsets on abelone data")

library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub5 <- regsubsets(x=x.1, y=y.1, nbest=5, nvmax=30)
order.bic <- order(summary(allsub5)$bic)
top5 <- summary(allsub5)$which[order.bic[c(1:5)],]
coef(allsub5,id=order.bic[c(1:5)])
summary(allsub5)$rss[order.bic[c(1:5)]]/nrow(y.1)

# predict data from validation set
mod.step <- lm(formula = Rings ~ Shell + Shucked + logshel + logshuc + logwhol + 
    Viscera + loghgt + sexm.shel + sexm.vic + sex.f + sexf.vic + 
    sexm.hgt, data = abelone[which(abelone$set == 1), ])
sMSE.step <- summary(mod.step)$sigma^2
extractAIC(mod.step, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.step <- predict(mod.step, newdata=x.2)
MSPE.step <- mean((pred2.step - y.2)^2)
c(sMSE.step,MSPE.step)

mod.all1 <- lm(formula = Rings ~ Length + Viscera + Shell + sexm.hgt + sexm.shuc + 
    sexf.len +  sexf.shuc + sexf.shel + loghgt + logwhol + logshuc, 
               data = abelone[which(abelone$set == 1), ])
sMSE.all1 <- summary(mod.all1)$sigma^2
extractAIC(mod.all1, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.all1 <- predict(mod.all1, newdata=x.2)
MSPE.all1 <- mean((pred2.all1 - y.2)^2)
c(sMSE.all1,MSPE.all1)

mod.all2 <- lm(formula = Rings ~ Length + Viscera + Shell + sexm.hgt + sexm.shuc + 
    sexf.len +  sexf.shuc + sexf.shel + logwhol + logshuc, 
               data = abelone[which(abelone$set == 1), ])
sMSE.all2 <- summary(mod.all2)$sigma^2
extractAIC(mod.all2, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.all2 <- predict(mod.all2, newdata=x.2)
MSPE.all2 <- mean((pred2.all2 - y.2)^2)
c(sMSE.all2,MSPE.all2)

mod.all3 <- lm(formula = Rings ~ sex.f + Viscera + Shell + sexm.hgt + sexm.shuc + 
    loghgt +  sexf.shuc + sexf.shel + logwhol + logshuc, 
               data = abelone[which(abelone$set == 1), ])
sMSE.all3 <- summary(mod.all3)$sigma^2
extractAIC(mod.all3, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.all3 <- predict(mod.all3, newdata=x.2)
MSPE.all3 <- mean((pred2.all3 - y.2)^2)
c(sMSE.all3,MSPE.all3)

mod.all4 <- lm(formula = Rings ~ sex.f + Viscera + Shell + sexm.hgt + sexm.shuc + 
    loghgt +   sexf.shel + logwhol + logshuc, 
               data = abelone[which(abelone$set == 1), ])
sMSE.all4 <- summary(mod.all4)$sigma^2
extractAIC(mod.all4, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.all4 <- predict(mod.all4, newdata=x.2)
MSPE.all4 <- mean((pred2.all4 - y.2)^2)
c(sMSE.all4,MSPE.all4)

mod.all5 <- lm(formula = Rings ~ Length +  Viscera + Shell + sexm.hgt + sexm.shuc + 
    loghgt +   sexf.shel + logwhol + logshuc + sexf.diam + sexf.shuc, 
               data = abelone[which(abelone$set == 1), ])
sMSE.all5 <- summary(mod.all5)$sigma^2
extractAIC(mod.all5, k=log(nrow(abelone[which(abelone$set==1),])))
pred2.all5 <- predict(mod.all5, newdata=x.2)
MSPE.all5 <- mean((pred2.all5 - y.2)^2)
c(sMSE.all5,MSPE.all5)

# Model 5 is best.  Evaluate its MSPE on set 3
pred2.all5.set3 <- predict(mod.all5, newdata=x.3)
MSPE.all5.set3 <- mean((pred2.all5.set3 - y.3)^2)
MSPE.all5.set3
