nnet_val <-  read.table("~/stat852/hw10/nn_val.csv", header=TRUE, sep=",", na.strings=" ")


gbm_val.1 <- read.table("~/stat852/hw10/gbm_val.csv", header=TRUE, sep=",", na.strings=" ")

gbm_val.2 <- read.table("~/stat852/hw10/gbm_val_2.csv", header=TRUE, sep=",", na.strings=" ")


svm_val <- read.table("~/stat852/hw10/svm_val.csv", header=TRUE, sep=",", na.strings=" ")

svm_val <- svm_val[,-1]

nnet_val <- nnet_val[,-1]

gbm_val <- cbind(gbm_val.1[,2:7],gbm_val.2[,6:8])



gbm_mean <- rowMeans(gbm_val[,-c(1,2,3,4)])
gbm_index <- which.min(gbm_mean)
gbm_para <- gbm_val[gbm_index,1:4]


svm_mean <- rowMeans(svm_val[,-c(1,2,3,4)])
svm_index <- which.min(svm_mean)
svm_para <- svm_val[svm_index,1:4]


nnet_mean <- rowMeans(nnet_val[,-c(1,2)])
nnet_index <- which.min(nnet_mean)
nnet_para <- nnet_val[nnet_index,1:2]