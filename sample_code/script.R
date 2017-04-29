# R library's import
source("clear.R")
library(randomForest)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ROCR)
library(e1071)

# This's loading TRAIN and TEST data
train  <- read.csv("./../../Kaggle_data/titanic/train.csv", stringsAsFactors = F, na.strings = "NULL")
test   <- read.csv("./../../Kaggle_data/titanic/test.csv",  stringsAsFactors = F, na.strings = "NULL")
train_ <- train
gender <- read.csv("./../../Kaggle_data/titanic/gender_submission.csv", stringsAsFactors = F, na.strings = "NULL")
test <- merge(gender,test)

train$Fare <- matrix(train$Fare) %>% impute(what = "median")
test$Fare  <- matrix(test$Fare)  %>% impute(what = "median")
train$Age  <- matrix(train$Age)  %>% impute(what = "mean")
test$Age   <- matrix(test$Age)   %>% impute(what = "mean")

train <- train[,-which (colnames(train) %in% 
                          c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Pclass"))]
test  <- test[,-which  (colnames(test)  %in% 
                          c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Pclass"))]

train[,"Survived"] <- as.factor(train[,"Survived"])
test[,"Survived"]  <- as.factor(test[,"Survived"])
train[,"Sex"] <- as.factor(train[,"Sex"])
test[,"Sex"]  <- as.factor(test[,"Sex"])

num <- sample(nrow(train), nrow(train)*0.7, replace = F)
train1 <- train[num,]
train2 <- train[-num,]

# randomforest tuneRF
tune <- tuneRF(train1[,-1], train1[,1], doBest=T, ntreeTry = 500, stepFactor = T)
varImpPlot(tune)
print("[tune-train1] Confusion Matrix:")
print(tune$confusion)

## train1
# predict
pred.tune <- predict(tune, newdata=train1, type='class')
prob.tune <- predict(tune, newdata=train1, type='prob')
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = train1[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-train1]AUC")
print(auc)

## train2
# predict
pred.tune <- predict(tune, newdata=train2, type='class')
prob.tune <- predict(tune, newdata=train2, type='prob')
# modelとtuneのConfution Matrixを比較
print("[tune-train2] Confusion Matrix:")
print(table(pred.tune, train2[,1]))
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = train2[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-train2]AUC")
print(auc)

# all train
tune <- tuneRF(train[,-1], train[,1], doBest=T, ntreeTry = 500, stepFactor = T)
varImpPlot(tune)
print("[tune-test] Confusion Matrix:")
print(tune$confusion)
# test predict
pred.tune <- predict(tune, newdata=test, type='class')
prob.tune <- predict(tune, newdata=test, type='prob')
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = test[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-test]AUC")
print(auc)
