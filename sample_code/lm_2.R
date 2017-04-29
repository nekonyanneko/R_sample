source("clear.R")

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
print(model1)
# Intercept + B1*x1 + B2*x2 + e
model2 <- lm(y ~ x1 + x2)
print(model2)
# Intercept + B1*x1 + B2*x2 + B3*x1*x2 + e
model3 <- lm(y ~ x1*x2)
print(model3)