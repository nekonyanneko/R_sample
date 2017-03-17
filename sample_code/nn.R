source("clear.R")
library(nnet)
attach(rock)  # R標準のデータサンプルを使用

## データの生成
area1 <- area/10000  # 岩の面積
peri1 <- peri/10000  # 岩の周辺長
rock1 <- data.frame(perm, area=area1, peri=peri1, shape)

# NNの学習とモデル作成
rock.nn <- 
  nnet(log(perm) ~ area + peri + shape, rock1,
       size=3, decay=1e-3, linout=T, skip=T, maxit=1000, Hess=T)

# NNの可視化
source("nn_plot.R")
plot.nn(rock.nn)
