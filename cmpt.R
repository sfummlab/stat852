cmpt <-read.table("~/Downloads/grades.csv", header=TRUE, sep=",", na.strings=" ")

head(cmpt)
# Splitting data in half using random uniform selection to make two "set"s. 120401002

set.seed(9267926) 

cmpt <- cmpt[sample(85),]

          