source("clear.R")
library(nnet)
attach(rock)

## データの生成
area1 <- area/10000  # 岩の面積
peri1 <- peri/10000  # 岩の周辺長
rock1 <- data.frame(perm, area=area1, peri=peri1, shape)

