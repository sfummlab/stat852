phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")

phoneme$g <- as.factor(phoneme$g)
kernels <- cbind("aa","ao","dcl", "iy", "sh")

num<- cbind(0.2,0.6,0.8,1)
Tree.siz <- cbind(1,5,10)
Shrink <- cbind(0.001,0.01,0.1)
sample.ratio <- cbind(0.25,0.5,0.75)

iter=3
phoneme.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

library(caret)
library(gbm)

library(nnet)


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = c(500,1000,1500,2000),
                        shrinkage = c(0.001,0.01,0.1))

nnetGrid <- expand.grid(size = c(1, 5, 10,20,50),
                        decay=c(0.001,0.01,0.1))


svmGrid <- expand.grid(degree = c(1,2,3),
                       scale = 10^(-3:2),
                       cost = c(1,5,10,100))


gbmControl <- trainControl(## 10-fold CV
  method = "boot",
  allowParallel = TRUE
  )

nnetControl <- trainControl(## 10-fold CV
  method = "boot",
  allowParallel = TRUE
)

svmControl <- trainControl(## 10-fold CV
  method = "boot",
  allowParallel = TRUE
)



gbmfit <- train(g ~ ., data = phoneme[,2:258],
                 method = "gbm",
                 trControl = gbmControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

nnetfit <- train(g ~ ., data = phoneme[2:258],
                method = "nnet",
                trControl = nnetControl,
                verbose = FALSE,
                tuneGrid = nnetGrid)


svmfit <- train(g ~ ., data = phoneme[2:258],
                method = "gbm",
                trControl = svmControl,
                verbose = FALSE,
                tuneGrid = svmGrid)




