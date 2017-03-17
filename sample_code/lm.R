source("clear.R") # 作業場キレイキレイ

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

## 区間推定
new<-data.frame(x=seq(min(x),max(x),0.1))

A<-predict(model,new,se.fit=T,interval="confidence")    #推定平均の95%推定区間
B<-predict(model,new,se.fit=T,interval="prediction")    #推定データの95%推定区間

lines(as.matrix(new),A$fit[,1])
lines(as.matrix(new),A$fit[,2],col="red")
lines(as.matrix(new),A$fit[,3],col="red")

lines(as.matrix(new),B$fit[,2],col="blue")
lines(as.matrix(new),B$fit[,3],col="blue")

