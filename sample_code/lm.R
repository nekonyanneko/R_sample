## データ作成
N  <-100
b0 <-5
b1 <-3
x  <-rnorm(N)
e  <-rnorm(n=N,sd=2)
y  <-b0+b1*x+e

plot(x,y)

## 線形回帰
model<-lm(y~x)
print(model)