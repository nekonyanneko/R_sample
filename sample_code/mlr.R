source("clear.R")

library(dplyr)
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

