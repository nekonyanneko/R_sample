source("clear.R")
library(randomForest)
attach(iris)

set.seed(1500)

# random sampling
n <- nrow(iris)
s <- sample(n, n * 0.5)
## 半分を学習、半分を評価に使う
iris.train <- iris[s,]
iris.test <- iris[-s,]

## ランダムフォレストで学習
model <- randomForest(Species ~ ., data=iris.train, ntree=500, proximity=TRUE)
print(model)

## テストデータで評価
pred.model <- predict(model, newdata=iris.test, type='class')
print(pred.model)

# 混合行列の表示
print("Confusion Matrix:")
print(table(pred.model, iris.test[,5]))
# モデルのTree構造を表示
print(getTree(model, 1, labelVar=TRUE))
# 特徴量の重要度を出力（ジニ係数）
print(model$importance)
varImpPlot(model)
# 多次元尺度法で分類を可視化
MDSplot(model, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))

