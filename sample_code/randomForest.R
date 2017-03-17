library(randomForest)
attach(iris)

set.seed(1500)

# random sampling
n <- nrow(iris)
s <- sample(n, n * 0.5)
## 半分を学習、半分を評価に使う
iris.train <- iris[s,]
iris.test <- iris[-s,]

