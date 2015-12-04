pro <-  read.table("~/stat852/data/Data2015.csv", header=TRUE, sep=",", na.strings=" ")
pro.test <-  read.table("~/stat852/data/Data2015test.csv", header=TRUE, sep=",", na.strings=" ")
#colnames(pro) <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

pro$X21 <- as.numeric(pro$X21)

pro.test$X21 <- as.numeric(pro.test$X21)

rice <- runif(1,0,1)
set.seed(rice * 10000000) 
pro$set <- ifelse(runif(n=nrow(pro))>0.75, yes=2, no=1)



x.3 <- as.matrix(pro[which(pro$set==1),1:21])
y.3 <- as.vector(pro[which(pro$set==1),22])

x.4 <- as.matrix(pro[which(pro$set==2),1:21])
y.4 <- as.matrix(pro[which(pro$set==2),22])


library(mixtools)
mix <- regmixEM(y.3, x.3, lambda = NULL, beta = NULL, sigma = NULL, k = 2,
         addintercept = TRUE, arbmean = TRUE, arbvar = TRUE,
         epsilon = 1e-08, maxit = 10000, verb = FALSE)

