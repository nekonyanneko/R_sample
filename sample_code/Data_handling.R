source("clear.R")

# ## データのダウンロードと解凍
# library(R.utils)
# download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2","./2008.csv.bz2")
# bunzip2("./2008.csv.bz2")
# 
# ## テキストファイルの読み込み
# library(readr)
# system.time(al.2008.readr <- read_csv("./2008.csv"))
# 
# # データの削除
# remove("./2008.csv")

## 
library(dplyr)
library(nycflights13)
flights %>% class() %>% print()  # データセットの形式を確認
flights %>% print()  # データの表示
# データの抽出
flights %>% filter(month == 12 , day == 31) %>% print()  # AND
flights %>% filter(month == 12 | day == 31) %>% print()  # OR
# データ列の抽出
flights %>% select(year, month, day) %>% print()
flights %>% select(year:day) %>% print()
flights %>% select(-(year:day)) %>% print()
# 列の追加
flights %>% select(-(year:dest)) %>% print()
flights %>% mutate(gain = flights$arr_delay - flights$dep_delay,
                   gain_per_hour = gain/(flights$air_time/60)) %>%
  select(-(year:dest)) %>% print()
# 行の順番の並び替え
flights %>% print()
flights %>% arrange(month, arr_delay) %>% print() #month,arr_delayの昇順
flights %>% arrange(desc(arr_delay)) %>% print()  #arr_delayの降順

# 機体番号ごとのグループ化,機体ごとの平均移動距離の算出
planes <- flights %>% group_by(tailnum)
planes %>% print()  #Groupsの表示に注目
planes %>% summarise(Dist = mean(distance, na.rm = T)) %>% print()

# 年、月、日ごとに平均移動距離の算出
planes <- flights %>% group_by(year, month, day)
planes %>% print()
planes %>% summarise(Dist = mean(distance, na.rm =T)) %>% print()
# 日にちの集計のためにい以下のグループ化では意図していない結果になるため注意が必要
planes <- flights %>% group_by(day)
planes %>% summarise(Dist = mean(distance, na.rm =T)) %>% print()

# 移動距離と出発遅延時間の平均値・中央値・標準偏差を求める。
flights %>% select(distance, dep_delay) %>% summarise_each(
  funs(mean   = mean(., na.rm = T),
       median = median(., na.rm = T),
       sd     = sd(., na.rm = T))) %>% print()
# もう一つの書き方->select関数の省略(部分一致するものを取ってくるため注意)
flights %>% summarise_each(
  funs(mean   = mean(., na.rm = T),
       median = median(., na.rm = T),
       sd     = sd(., na.rm = T)),
  c(matches("distance"), matches("dep_delay"))) %>% print()

# 正規化(各値を平均値で引いた後に標準偏差で割る)
flights %>% select(distance, dep_delay) %>% print()
flights %>% mutate_each(funs(scale),
                        c(matches("distance"),matches("dep_delay"))) %>% select(distance, dep_delay) %>% print()