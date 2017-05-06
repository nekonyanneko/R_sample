# R library's import
source("clear.R")
library(randomForest)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ROCR)
library(e1071)

set.seed(2100)

data_crensing <- function(input_data){
  input_data$Fare <- matrix(input_data$Fare) %>% impute(what = "median")
  input_data$Age  <- matrix(input_data$Age)  %>% impute(what = "mean")

  input_data <- input_data[,-which (colnames(input_data) %in% 
                            c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Pclass"))]

  input_data[,"Survived"] <- as.factor(input_data[,"Survived"])
  input_data[,"Sex"]      <- as.factor(input_data[,"Sex"])

  return(input_data)
}

randomForest_TUNE <- function(train){
  # randomforest tuneRF
  tune <- tuneRF(train[,-1], train[,1], doBest=T, ntreeTry = 500, stepFactor = T)
  varImpPlot(tune)
  
  return(tune)
}

predict_randomForest <- function(tune,data){
  # data predict
  pred.tune <- predict(tune, newdata=data, type='class')
  prob.tune <- predict(tune, newdata=data, type='prob')
  print("[data] Confusion Matrix:")
  table(pred.tune, data$Survived) %>% print()
  
  # ROC and AUC
  pred <- prediction(predictions = prob.tune[,2], labels = data[,1])  #確率値、正解ラベル
  perf <- performance(pred, "tpr", "fpr")
  plot(perf)
  auc.tmp <- performance(pred,"auc")
  auc <- as.numeric(auc.tmp@y.values)
  print("[tune-data] AUC:ß")
  print(auc)
}

main <- function(){
  # This's loading TRAIN and TEST data
  train  <- read.csv("./../../Kaggle_data/titanic/train.csv", stringsAsFactors = F, na.strings = "NULL")
  test   <- read.csv("./../../Kaggle_data/titanic/test.csv",  stringsAsFactors = F, na.strings = "NULL")
  gender <- read.csv("./../../Kaggle_data/titanic/gender_submission.csv", stringsAsFactors = F, na.strings = "NULL")
  test <- merge(gender,test)
  tmp  <- rbind(train,test)
  num   <- sample(nrow(tmp), nrow(tmp)*0.7, replace = F)
  train <- tmp[num,]
  test  <- tmp[-num,]
  
  # Data crensing
  train <- data_crensing(train)
  test  <- data_crensing(test)
  
  # Training by randomForest
  tune <- randomForest_TUNE(train)
  
  # predict for train data
  predict_randomForest(tune,train)
  
  # predict for test data
  predict_randomForest(tune,test)
}
