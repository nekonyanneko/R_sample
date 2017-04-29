source("clear.R")
library(randomForest)
attach(iris)

set.seed(2100)

# random sampling
n <- nrow(iris)
s <- sample(n, n * 0.7)
## 半分を学習、半分を評価に使う
iris.train <- iris[s,]
iris.test <- iris[-s,]

## ランダムフォレストで学習
model <- randomForest(Species ~ ., data=iris.train, mtry=4, ntree=500, proximity=TRUE)
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

# 特徴量と分類の関係グラフ（どれだけ寄与しているか）
par(mfrow = c(sqrt(nrow(model$importance)), sqrt(nrow(model$importance)))) 
partialPlot(model, iris, Petal.Length, 'setosa')
partialPlot(model, iris, Petal.Length, 'versicolor')
partialPlot(model, iris, Petal.Length, 'virginica')

## ベストなmtryを探し最善の結果を出力
tune <- tuneRF(iris.train[,-5],iris.train[,5],doBest=T)
# テストデータで評価
pred.tune <- predict(tune, newdata=iris.test, type='class')
print(pred.tune)

# modelとtuneのConfution Matrixを比較
print("********* compare *********")
print("[model] Confusion Matrix:")
print(table(pred.model, iris.test[,5]))
print("[tune] Confusion Matrix:")
print(table(pred.tune, iris.test[,5]))
