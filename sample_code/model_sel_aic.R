source("clear.R")
library(MuMIn)

## シミュレーションデータの作成
set.seed(0)
N <- 100                        # サンプルサイズ
Intercept <- 5                  # 切片
B1 <- 10                        # 係数1
B2 <- 5                         # 係数2
x1 <- sort(rnorm(N, sd=2))      # 説明変数1
x2 <- rnorm(N, sd=2)            # 説明変数2
e <- rnorm(n=N, sd=3)           # 誤差
# データ作成
y <- Intercept + B1*x1 + B2*x2 + e

## 重回帰モデルの作成
# Intercept + B1*x1 + e のモデル生成
model1 <- lm(y ~ x1)
# Intercept + B1*x1 + B2*x2 + e
model2 <- lm(y ~ x1 + x2)
# Intercept + B1*x1 + B2*x2 + B1*B2*x3 + e
model3 <- lm(y ~ x1*x2)

## AICを計算
print("AIC:")
print(AIC(model1))
print(AIC(model2))
print(AIC(model3))

## MuMInで最適モデルの計算
options(na.action = "na.fail")
result.AIC <- dredge(model3, rank="AIC")  #変数の多いモデルを渡すこと
print(result.AIC)

## 最適モデルの抽出
best.model <- get.models(result.AIC, subset = 1)[1]
print(best.model)
