"
Data preprocessing 
Data Mining Techniques 
Assignment 1 
Task 1 B
"

remove(list = ls())
options(warn=-1)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)

ODI = read.csv("./data/ODI/ODI-2021_clean.csv",header = T, sep =";") %>% as_tibble()
ODI <- ODI %>% dplyr::mutate(
  MLCourse=as.factor(MLCourse),
  IRCourse=as.factor(IRCourse),
  StatCourse=as.factor(StatCourse),
  DBCourse=as.factor(DBCourse),
  Gender=as.factor(Gender),
  Chocolate=as.factor(Chocolate),
  Reward=as.numeric(Reward),
  RandomNo=as.numeric(RandomNo),
  Bedtime= factor(ODI$Bedtime, levels =c("19","20","21","22","23","0","1","2","3","4","5"),  ordered = F)
)

# Define comparison metric
metric <- "Accuracy"

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
set.seed(123)
model_nb <- train(Gender~Chocolate+Reward+StatCourse, data=ODI, trControl=train_control, method="nb", na.action = na.omit)
# Fit KNN
set.seed(123)
model_knn <- train(Gender~Chocolate+Reward+StatCourse, data=ODI, trControl=train_control, method="knn", na.action = na.omit)

# Summarise Results
results <- resamples(list(nb=model_nb,knn=model_knn))
summary(results)