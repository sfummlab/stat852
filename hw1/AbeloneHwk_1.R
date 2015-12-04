abalone <-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")

head(abalone)

colnames(abalone) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
#abalone$sex <- ifelse(abalone$Sex == "M", yes=1, no=2)
library(car)


head(abalone)
abalone$sex.m = ifelse(abalone$Sex=="M", yes=1, no=0)
abalone$sex.f = ifelse(abalone$Sex=="F", yes=1, no=0)

# Scatterplot matrix of all but nominal variable

quartz(h=12, w=12)
#pairs(x=abalone[,-1])
#scatterplotMatrix(abalone[,2:9], spread=FALSE)
# Add interactions with sex and the log of each variable
abalone$sexm.len  <- abalone$sex.m * abalone$Length
abalone$sexm.diam <- abalone$sex.m * abalone$Diameter
abalone$sexm.hgt  <- abalone$sex.m * abalone$Height
abalone$sexm.whol <- abalone$sex.m * abalone$Whole
abalone$sexm.shuc <- abalone$sex.m * abalone$Shucked
abalone$sexm.vic  <- abalone$sex.m * abalone$Viscera
abalone$sexm.shel <- abalone$sex.m * abalone$Shell
abalone$sexf.len  <- abalone$sex.f * abalone$Length
abalone$sexf.diam <- abalone$sex.f * abalone$Diameter
abalone$sexf.hgt  <- abalone$sex.f * abalone$Height
abalone$sexf.whol <- abalone$sex.f * abalone$Whole
abalone$sexf.shuc <- abalone$sex.f * abalone$Shucked
abalone$sexf.vic  <- abalone$sex.f * abalone$Viscera
abalone$sexf.shel <- abalone$sex.f * abalone$Shell
abalone$loglen  <- log(abalone$Length)
abalone$logdiam <- log(abalone$Diameter)
abalone$loghgt <- log(abalone$Height)
abalone$logwhol<- log(abalone$Whole)
abalone$logshuc<- log(abalone$Shucked)
abalone$logvic <- log(abalone$Viscera)
abalone$logshel<- log(abalone$Shell)

#Split data into 3 sets:
#  1=training
#  2=validation
#  3=test

set.seed(29003092)
U <- runif(n=nrow(abalone))
abalone$set <- ifelse(U<0.5, yes=1, no=ifelse(U>.75, yes=3, no=2))
abalone <- abalone[,-1] # Dump old Sex variable


#Remove NA entries in Data Matrix
#abalone[complete.cases(abalone),]



#abalone <- na.omit(abalone)
abalone <- abalone[is.finite(rowSums(abalone)),]
#abalone[apply(abalone, 1, Compose(is.finite, all)),]


x.1 <- as.matrix(abalone[which(abalone$set==1),-8])
y.1 <- as.matrix(abalone[which(abalone$set==1),8])
x.2 <- as.data.frame(abalone[which(abalone$set==2),-8])
y.2 <- as.data.frame(abalone[which(abalone$set==2),8])
x.3 <- as.data.frame(abalone[which(abalone$set==3),-8])
y.3 <- as.data.frame(abalone[which(abalone$set==3),8])


initial.1 <- lm(data=abalone[which(abalone$set==1),], 
                formula=Rings~ 1)
final.1 <- lm(data=abalone[which(abalone$set==1),], 
              formula=Rings ~ Length + Diameter + Height + Whole + Shucked + Viscera + Shell + sex.m + sex.f)

step1 <- step(object=initial.1, scope=list(upper=final.1), direction="forward",
              k = log(nrow(abalone[which(abalone$set==1),])))
summary(step1)



initial.2 <- lm(data=abalone[which(abalone$set==1),], 
                formula=Rings~ 1)
final.2 <- lm(data=abalone[which(abalone$set==1),], 
formula=Rings~ Length + Diameter + Height + Whole + Shucked + Viscera + Shell + sex.m + sex.f +
  sexm.len + sexm.diam + sexm.hgt  +sexm.whol + sexm.shuc + sexm.vic + sexm.shel + 
  sexf.len + sexf.diam + sexf.hgt + sexf.whol + sexf.shuc + sexf.vic + sexf.shel + 
  loglen + logdiam + loghgt + logwhol + logshuc + logvic + logshel)

step2 <- step(object=initial.2, scope=list(upper=final.2), direction="forward",
              k = log(nrow(abalone[which(abalone$set==1),])))
summary(step2)


library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub1 <- regsubsets(x=x.1[,1:9], y=y.1, nbest=2, nvmax=30)
summary(allsub1)
order.bic.sub1 <- order(summary(allsub1)$bic)
top.allsub1<- summary(allsub1)$which[order.bic.sub1[c(1)],]

sublist1 <- top.allsub1[which(top.allsub1 == TRUE)]

# Plot of results in a special form
quartz(h=15, w=12, pointsize=12)
plot(allsub1, main="All Subsets on abalone original 9 expalnatory variables")



library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
allsub2 <- regsubsets(x=x.1, y=y.1, nbest=2, nvmax=30)
summary(allsub2)
order.bic.sub2 <- order(summary(allsub2)$bic)
top.allsub2<- summary(allsub2)$which[order.bic.sub2[c(1)],]

sublist2 <- top.allsub2[which(top.allsub2 == TRUE)]


# Plot of results in a special form
quartz(h=15, w=12, pointsize=12)
plot(allsub2, main="All Subsets on abalone full pool of data")


stepwise1 <- lm(formula = Rings ~ Shell + Shucked + Height + Whole + Diameter + 
     Viscera + sex.m + sex.f, data = abalone[which(abalone$set == 1), ])


stepwise2 <- lm(formula = Rings ~ logshel + logshuc + logwhol + Shell + Viscera + 
                   Height + Shucked + Whole + loglen, data = abalone[which(abalone$set == 1), ])

sub1 <- lm(formula = Rings ~ Diameter+Height+  Whole+ Shucked+ Viscera+Shell+ sex.m + sex.f,
            data = abalone[which(abalone$set == 1), ])

sub2 <- lm(formula = Rings ~ Length + Heigh + Whole + Viscera+sexm.len + sexm.shuc + sexf.len+sexf.shuc +
            logwhol + logshuc + logshel, data = abalone[which(abalone$set == 1), ])


sMSE.sub1 <- summary(sub1)$sigma^2
pred.sub1 <- predict(sub1, newdata=x.2)
MSPE.sub1 <- mean((pred.sub1-y.2)^2)

c(sMSE.sub1,MSPE.sub1)



sMSE.sub2<- summary(sub2)$sigma^2
pred.sub2 <- predict(sub2, newdata=x.2)
MSPE.sub2 <- mean((pred.sub2-y.2)^2)

c(sMSE.sub2,MSPE.sub2)



sMSE.step1 <- summary(stepwise1)$sigma^2
pred.step1 <- predict(stepwise1, newdata=x.2)
MSPE.step1 <- mean((pred.step1-y.2)^2)

c(sMSE.step1,MSPE.step1)


sMSE.step2 <- summary(stepwise2)$sigma^2
pred.step2 <- predict(stepwise2, newdata=x.2)
MSPE.step2 <- mean((pred.step2-y.2)^2)

c(sMSE.step2,MSPE.step2)

sMSE.com <- c(sMSE.step1, sMSE.step2, sMSE.sub1, sMSE.sub2)

MSPE.com <- c(MSPE.step1, MSPE.step2, MSPE.sub1, MSPE.sub2)

letters <- c("step 9 variables"," step full pool", "subset 9 variables", "subset full pool")
quartz(h=7, w=10, pointsize=12)
par(mfrow=c(1,2))
plot(sMSE.com, xlab = "Model", ylab= "Sample MSE",xlim=c(0, 5))
text(sMSE.com,letters,cex = 0.6, pos = 4, col = "red")
#axis(1, at=1:4, labels=letters[1:4])
#lines(sMSE.com,type='o')
plot(MSPE.com, xlab = "Model", ylab= "Validation MSE",xlim=c(0, 5))
#lines(MSPE.com,type='o')
text(MSPE.com,letters,cex = 0.6, pos = 4, col = "red")



# Refinementon on Best Model 


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
                 sexm.hgt, data = abalone[which(abalone$set == 1), ])
sMSE.step <- summary(mod.step)$sigma^2
extractAIC(mod.step, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.step <- predict(mod.step, newdata=x.2)
MSPE.step <- mean((pred2.step - y.2)^2)
c(sMSE.step,MSPE.step)

mod.all1 <- lm(formula = Rings ~ Length + Viscera + Shell + sexm.hgt + sexm.shuc + 
                 sexf.len +  sexf.shuc + sexf.shel + loghgt + logwhol + logshuc, 
               data = abalone[which(abalone$set == 1), ])
sMSE.all1 <- summary(mod.all1)$sigma^2
extractAIC(mod.all1, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.all1 <- predict(mod.all1, newdata=x.2)
MSPE.all1 <- mean((pred2.all1 - y.2)^2)
c(sMSE.all1,MSPE.all1)

mod.all2 <- lm(formula = Rings ~ Length + Viscera + Shell + sexm.hgt + sexm.shuc + 
                 sexf.len +  sexf.shuc + sexf.shel + logwhol + logshuc, 
               data = abalone[which(abalone$set == 1), ])
sMSE.all2 <- summary(mod.all2)$sigma^2
extractAIC(mod.all2, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.all2 <- predict(mod.all2, newdata=x.2)
MSPE.all2 <- mean((pred2.all2 - y.2)^2)
c(sMSE.all2,MSPE.all2)

mod.all3 <- lm(formula = Rings ~ sex.f + Viscera + Shell + sexm.hgt + sexm.shuc + 
                 loghgt +  sexf.shuc + sexf.shel + logwhol + logshuc, 
               data = abalone[which(abalone$set == 1), ])
sMSE.all3 <- summary(mod.all3)$sigma^2
extractAIC(mod.all3, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.all3 <- predict(mod.all3, newdata=x.2)
MSPE.all3 <- mean((pred2.all3 - y.2)^2)
c(sMSE.all3,MSPE.all3)

mod.all4 <- lm(formula = Rings ~ sex.f + Viscera + Shell + sexm.hgt + sexm.shuc + 
                 loghgt +   sexf.shel + logwhol + logshuc, 
               data = abalone[which(abalone$set == 1), ])
sMSE.all4 <- summary(mod.all4)$sigma^2
extractAIC(mod.all4, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.all4 <- predict(mod.all4, newdata=x.2)
MSPE.all4 <- mean((pred2.all4 - y.2)^2)
c(sMSE.all4,MSPE.all4)

mod.all5 <- lm(formula = Rings ~ Length +  Viscera + Shell + sexm.hgt + sexm.shuc + 
                 loghgt +   sexf.shel + logwhol + logshuc + sexf.diam + sexf.shuc, 
               data = abalone[which(abalone$set == 1), ])
sMSE.all5 <- summary(mod.all5)$sigma^2
extractAIC(mod.all5, k=log(nrow(abalone[which(abalone$set==1),])))
pred2.all5 <- predict(mod.all5, newdata=x.2)
MSPE.all5 <- mean((pred2.all5 - y.2)^2)
c(sMSE.all5,MSPE.all5)

# Model 5 is best.  Evaluate its MSPE on set 3
pred2.all5.set3 <- predict(mod.all5, newdata=x.3)
MSPE.all5.set3 <- mean((pred2.all5.set3 - y.3)^2)
MSPE.all5.set3
