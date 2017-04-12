source("clear.R")

library(dplyr)
library(BBmisc)
library(ggplot2)
library(ParamHelpers)
library(mlr)
library(gbm)
library(clue)
data(iris)

#################
# 利用の流れ
#################
# タスクの定義
task = makeClassifTask(id = "tutorial", data = iris, target = "Species")
# 学習器の定義
lrn = makeLearner("classif.lda")
# リサンプリング法の定義
rdesc = makeResampleDesc(method = "CV", stratify = TRUE)
# 実行
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)
# 誤分類率
r$aggr %>% print()

#################
# タスクの生成
#################
# 回帰
data(BostonHousing, package = "mlbench")
regr.task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")
regr.task %>% print()

# 分類
data(BreastCancer, package = "mlbench")
df = BreastCancer
df$Id = NULL
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
# positiveなクラスを自分で決めたい場合
# classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", positive = "malignant")
classif.task %>% print()

# クラスタリング
data(mtcars, package = "datasets")
cluster.task = makeClusterTask(id = "mtcars", data = mtcars)
cluster.task %>% print()

#################
# タスクへのアクセス
#################
# タスクの概要
getTaskDesc(regr.task) %>% print()
# タスクで使うデータ
str(getTaskData(classif.task)) %>% print()
# データの件数
getTaskSize(classif.task) %>% print()
# 特徴量の数
getTaskNFeats(cluster.task) %>% print()
# 特徴量の名前
getTaskFeatureNames(cluster.task) %>% print()
# 目的変数の名前
getTaskTargetNames(classif.task) %>% print()
# 目的変数の値
head(getTaskTargets(regr.task)) %>% print()
# コスト行列
getTaskCosts(classif.task) %>% print()
getTaskFormula(classif.task) %>% print()

#################
# タスクの修正や変更
#################
# データの一部だけ使う
subsetTask(cluster.task, subset = 4:17) %>% print()
# 定数値の特徴量を捨てる
removeConstantFeatures(cluster.task) %>% print()
# 定数値の特徴量を捨てる
removeConstantFeatures(cluster.task) %>% print()
# 変数の正規化
## （method="range"は値が区間[0, 1]に収まるようスケールする）
summary(getTaskData(normalizeFeatures(cluster.task, method = "range"))) %>% print()
## （method="standardize"は平均0，分散1に正規化する）
summary(getTaskData(normalizeFeatures(regr.task, method = "standardize"))) %>% print()

#################
# 学習器の生成
#################
# ランダムフォレストで確率を出力
# （fix.factors.prediction=TRUEはファクタの水準数が学習データとテストデータで異なる場合に生じる問題をうまく処理してくれる）
classif.lrn = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn %>% print()
# 勾配ブースティングでハイパーパラメータを指定
regr.lrn = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))
regr.lrn %>% print()
# クラスタ数5でK-means
cluster.lrn = makeLearner("cluster.kmeans", centers = 5)
cluster.lrn %>% print()

#################
# 学習器へのアクセス
#################
# 設定済みハイパーパラメータ
getHyperPars(cluster.lrn) %>% print()
# 設定可能なハイパーパラメータ一覧
getParamSet(cluster.lrn) %>% print()
# ハイパーパラメータ一覧はLearnerを作成しなくても学習器名から取得できる
getParamSet("classif.randomForest") %>% print()

#################
# 学習器の修正や変更
#################
## 学習器の識別子を設定
surv.lrn = setLearnerId(classif.lrn, "CoxModel") %>% print()
## 出力を確率からクラスラベルに変更
classif.lrn = setPredictType(classif.lrn, "response") %>% print()
## ハイパーパラメータの値を変更
cluster.lrn = setHyperPars(cluster.lrn, centers = 4) %>% print()
## ハイパーパラメータの値をデフォルト値にする
regr.lrn %>% print()
regr.lrn = setHyperPars(regr.lrn, interaction.depth = 4, n.trees = 1000)
regr.lrn %>% print()
regr.lrn = removeHyperPars(regr.lrn, c("n.trees", "interaction.depth"))
regr.lrn %>% print()


