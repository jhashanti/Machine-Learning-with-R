data(iris)
library(randomForest)
#cross validation
partition <- function(numRow,seed) {
  set.seed(seed)
  index    <- sample(x = numRow,size = trunc(numRow*(2/3)),replace = FALSE)
}

trainIndex <- partition(numRow = nrow(iris),seed = 123)
training   <- train[trainIndex,]
testing    <- train[-trainIndex,]
model      <- randomForest(training$Sepal.Length ~ ., data = training, ntree = 100 )
temp       <- predict(model, testing[,-1])

#K fold cross validation

predicted  <- vector(mode = "numeric",length = nrow(iris))
actual     <- vector(mode = "numeric",length = nrow(iris))
k          <- 10
iris       <- iris[sample(x = nrow(iris),size = nrow(iris),replace = FALSE),]
folds      <- cut(seq(1,nrow(iris)),breaks=k,labels=FALSE)

for(i in 1:k){
  testIndex <- which(folds == i)
  testing   <- iris[testIndex,]
  training  <- iris[-testIndex,]
  model     <- randomForest(training$Sepal.Length ~ ., data = training, ntree = 100 )
  temp      <- as.data.frame(predict(model, testing[,-1]))
  predicted <- c(predicted, temp)
  actual    <- c(actual, testing[,1])
}

accuracy    <- summary(abs(actual - predicted))

#leave one out validation
#k           <- nrow(iris)