source("clear.R")

library(dplyr)
library(BBmisc)
library(ggplot2)
library(ParamHelpers)
library(mlr)
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
str(getTaskData(classif.task))







