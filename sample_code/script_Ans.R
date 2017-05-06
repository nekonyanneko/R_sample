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

  # Data Processing (Add Title)
  input_data$Title <- gsub('(.*, )|(\\..*)', '', input_data$Name)
  input_data$Miss[input_data$Title == "Miss"] <- 1; input_data$Miss[is.na(input_data$Miss)] <- 0
  input_data$Mr[input_data$Title == "Mr"]     <- 1; input_data$Mr[is.na(input_data$Mr)]     <- 0
  input_data$Mrs[input_data$Title == "Mrs"]   <- 1; input_data$Mrs[is.na(input_data$Mrs)]   <- 0
  input_data$Embarked_S[input_data$Embarked == "S"] <- 1; input_data$Embarked_S[is.na(input_data$Embarked_S)] <- 0
  input_data$SibSpAndParch <- input_data$SibSp + input_data$Parch
  input_data$Cabin2 <- gsub('([A-Z].*)', '1', input_data$Cabin)
  input_data$Cabin2[input_data$Cabin2 == ""] <- 0

  # Fare/Age log scale
  input_data$Age <- as.integer(input_data$Age)
  input_data$Age[0  <= input_data$Age & input_data$Age < 10] <- 0
  input_data$Age[10 <= input_data$Age & input_data$Age < 20] <- 10
  input_data$Age[20 <= input_data$Age & input_data$Age < 30] <- 20
  input_data$Age[30 <= input_data$Age & input_data$Age < 40] <- 30
  input_data$Age[40 <= input_data$Age] <- 40
  input_data$Fare <- as.integer(input_data$Fare)
  input_data$Fare <- log(input_data$Fare+1) # +1 is not -Inf

  for(i in 0:9){
    for(j in 0:9){
      for(z in 0:9){
        for(l in 0:9){
          target <- paste(c(as.character(i), as.character(j), as.character(z),as.character(l)), collapse = "")
          label  <- target
          target <- paste(c(as.character(target), ".*"), collapse = "")
          input_data$Tiket2[regexpr(target, input_data$Ticket) != -1] <- label
        }
      }
    }
  }
  input_data$Tiket2[is.na(input_data$Tiket2)] <- 0

  # Delete Data (PassengerId,Name)
  input_data <- input_data[,-which (colnames(input_data) %in% 
                            c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Title"
                              ,"Mr","Miss","Embarked_S"))]
  input_data[,"Survived"] <- as.factor(input_data[,"Survived"])
  input_data[,"Sex"] <- as.factor(input_data[,"Sex"])

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
  print("[tune-data] AUC:")
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
