# R library's import
library(randomForest)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ROCR)
library(e1071)

# This's loading TRAIN and TEST data
train  <- read.csv("./../../Kaggle_data/titanic/train.csv", stringsAsFactors = F, na.strings = "NULL")

# Processing missing values
train$Fare <- matrix(train$Fare) %>% impute(what = "median")
train$Age  <- matrix(train$Age)  %>% impute(what = "mean")

# This temporary is used for conversion for Hist Gram
hist_tmp <- train

# ->>>>>> Attention male/female
hist_tmp$Sex[hist_tmp$Sex == "male"]   <- 0
hist_tmp$Sex[hist_tmp$Sex == "female"] <- 1
hist(as.integer(hist_tmp$Sex[hist_tmp$Survived == 0]), breaks=0:2,
     xlab ="Sex", ylab = "Survive", main = "Sex" , col = "#ff00ff50", labels = T, ylim = c(0,600))
hist(as.integer(hist_tmp$Sex[hist_tmp$Survived == 1]), breaks=0:2,
     xlab ="Sex", ylab = "Survive", main = "Sex" , col = "#00000050", labels = T, add = T)
# ->>>>>> Attention male/female
# ->>>>>> Attention Master Miss Mr Mrs
hist_tmp$Title <- gsub('(.*, )|(\\..*)', '', hist_tmp$Name)
table(hist_tmp$Sex, hist_tmp$Title)
#    Capt Col Don  Dr Jonkheer Lady Major Master Miss Mlle Mme  Mr Mrs  Ms Rev Sir the Countess
# 0    1   2   1   6        1    0     2     40    0    0   0 517   0   0   6   1            0
# 1    0   0   0   1        0    1     0      0  182    2   1   0 125   1   0   0            1
hist_tmp$Title[hist_tmp$Title == "Master"] <- 1
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "1"]), breaks=2,
     xlab ="Master", ylab = "Survive", main = "Master" , col = "#ff00ff50", labels = T, ylim = c(0,25))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "1"]), breaks=2,
     xlab ="Master", ylab = "Survive", main = "Master" , col = "#00000050", labels = T, add = T)
# ->>>>>> It isn't a good result
hist_tmp$Title[hist_tmp$Title == "Miss"] <- 2
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "2"]), breaks=2,
     xlab ="Miss", ylab = "Survive", main = "Miss" , col = "#ff00ff50", labels = T, ylim = c(0,150))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "2"]), breaks=2,
     xlab ="Miss", ylab = "Survive", main = "Miss" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a good result
hist_tmp$Title[hist_tmp$Title == "Mr"] <- 3
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "3"]), breaks=2,
     xlab ="Mr", ylab = "Survive", main = "Mr" , col = "#ff00ff50", labels = T, ylim = c(0,520))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "3"]), breaks=2,
     xlab ="Mr", ylab = "Survive", main = "Mr" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a very good result
hist_tmp$Title[hist_tmp$Title == "Mrs"] <- 4
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "4"]), breaks=2,
     xlab ="Mrs", ylab = "Survive", main = "Mrs" , col = "#ff00ff50", labels = T, ylim = c(0,130))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "4"]), breaks=2,
     xlab ="Mrs", ylab = "Survive", main = "Mrs" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a very good result
# ->>>>>> Attention Master Miss Mr Mrs
# ->>>>>> Attention Emarked
hist_tmp$Embarked[hist_tmp$Embarked == "C"] <- 1
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "1"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "C" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "1"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "C" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is not a very good result
hist_tmp$Embarked[hist_tmp$Embarked == "S"] <- 2
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "2"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "S" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "2"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "S" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is a good result
hist_tmp$Embarked[hist_tmp$Embarked == "Q"] <- 3
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "3"]), breaks=2,
     xlab ="Q", ylab = "Survive", main = "Q" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "3"]), breaks=2,
     xlab ="Q", ylab = "Survive", main = "Q" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is not a very good result
# ->>>>>> Attention Emarked
# ->>>>>> Attention SibSp/Parch
hist(as.integer(hist_tmp$SibSp[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="SibSp", ylab = "Survive", main = "SibSp" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$SibSp[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="SibSp", ylab = "Survive", main = "SibSp" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$SibSp)
hist(as.integer(hist_tmp$Parch[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="Parch", ylab = "Survive", main = "Parch" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$Parch[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="Parch", ylab = "Survive", main = "Parch" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$Parch)
hist_tmp$SibSpAndParch <- hist_tmp$SibSp + hist_tmp$Parch
hist(as.integer(hist_tmp$SibSpAndParch[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="SibSpAndParch", ylab = "Survive", main = "SibSpAndParch" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$SibSpAndParch[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="SibSpAndParch", ylab = "Survive", main = "SibSpAndParch" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$SibSpAndParch)
# ->>>>>> Attention SibSp/Parch
# ->>>>>> Attention Cabin
hist_tmp$CabinA <- gsub('(A.*)', '0', hist_tmp$Cabin)
hist_tmp$CabinB <- gsub('(B.*)', '1', hist_tmp$CabinA)
hist_tmp$CabinC <- gsub('(C.*)', '2', hist_tmp$CabinB)
hist_tmp$CabinD <- gsub('(D.*)', '3', hist_tmp$CabinC)
hist_tmp$CabinE <- gsub('(E.*)', '4', hist_tmp$CabinD)
hist_tmp$CabinF <- gsub('(F.*)', '5', hist_tmp$CabinE)
hist_tmp$Cabin2 <- gsub('([G-Z].*)', '6', hist_tmp$CabinF)
hist(as.integer(hist_tmp$Cabin2[hist_tmp$Survived == 0]), breaks=6,
     xlab ="Cabin2", ylab = "Survive", main = "Cabin2" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$Cabin2[hist_tmp$Survived == 1]), breaks=6,
     xlab ="Cabin2", ylab = "Survive", main = "Cabin2" , col = "#00000050", labels = T, add = T)
# ->>>>>> last tuning
# ->>>>>> Attention Cabin
# ->>>>>> Attention Ticket
table_tmp <- as.data.frame(table(hist_tmp$Survived,hist_tmp$Ticket))
# ->>>>>> Attention Ticket
# ->>>>>> Attention Pclass
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "1"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "1"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "2"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "2"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "3"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "3"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
# ->>>>>> uum
# ->>>>>> Attention Pclass
# ->>>>>> Attention Pclass/Age
hist_tmp$Age <- as.integer(hist_tmp$Age)
hist_tmp$Age[0  <= hist_tmp$Age & hist_tmp$Age <= 10] <- 10
hist_tmp$Age[11 <= hist_tmp$Age & hist_tmp$Age <= 20] <- 20
hist_tmp$Age[21 <= hist_tmp$Age & hist_tmp$Age <= 30] <- 30
hist_tmp$Age[31 <= hist_tmp$Age & hist_tmp$Age <= 40] <- 40
hist_tmp$Age[41 <= hist_tmp$Age] <- 50
table(hist_tmp$Survived,hist_tmp$Age)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0]), breaks=0:5*10,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#ff00ff50", labels = T, ylim = c(0,300))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1]), breaks=0:5*10,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#00000050", labels = T, add = T)

hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 1]), breaks = 0:5*10,
     xlab ="Pclass1", ylab = "Survive", main = "Pclass1" , col = "#ff00ff50", labels = T, ylim = c(0,50))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 1]), breaks = 0:5*10,
     xlab ="Pclass1", ylab = "Survive", main = "Pclass1" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 2]), breaks = 0:5*10,
     xlab ="Pclass2", ylab = "Survive", main = "Pclass2" , col = "#ff00ff50", labels = T, ylim = c(0,300))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 2]), breaks = 0:5*10,
     xlab ="Pclass2", ylab = "Survive", main = "Pclass2" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 3]), breaks = 0:5*10,
     xlab ="Pclass3", ylab = "Survive", main = "Pclass3" , col = "#ff00ff50", labels = T, ylim = c(0,250))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 3]), breaks = 0:5*10,
     xlab ="Pclass3", ylab = "Survive", main = "Pclass3" , col = "#00000050", labels = T, add = T)
# ->>>>>> Attention Pclass/Age
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0]), breaks=0:5*10,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#ff00ff50", labels = T, ylim = c(0,300))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1]), breaks=0:5*10,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#00000050", labels = T, add = T)

