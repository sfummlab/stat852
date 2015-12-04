library(car)
abalone<-  read.table("~/stat852/data/abalone.data", header=TRUE, sep=",", na.strings=" ")
head(abalone)

abalone$sex <- ifelse(abalone$M == "M", yes=2, no=1)

abalone$M <- NULL

scatterplotMatrix(abalone, spread=TRUE)