"
Classification 
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
  MLCourse=factor(MLCourse),
  IRCourse=factor(IRCourse),
  StatCourse=factor(StatCourse),
  DBCourse=factor(DBCourse),
  Gender=factor(Gender),
  Chocolate=factor(Chocolate),
  Reward=as.numeric(Reward),
  RandomNo=as.numeric(RandomNo),
  Bedtime= factor(ODI$Bedtime, levels =c("19","20","21","22","23","0","1","2","3","4","5"),  ordered = F)
)
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(ODI$Gender, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- ODI[-validation_index,]
# use the remaining 80% of data to training and testing the models
ODI <- ODI[validation_index,]

# Define comparison metric
metric <- "Accuracy"
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
#Fit LDA
set.seed(123)
model_lda <- train(Gender~finalgd+Chocolate+Programme+Stresslevel, data=ODI,  method="lda",metric = metric, trControl=train_control, na.action = na.omit)
#decisiontree - Random Forest
set.seed(123)
model_rf <- train(Gender~finalgd+Chocolate+Programme+Stresslevel, data=ODI,  method="rf", metric = metric, trControl=train_control,na.action = na.omit)
# Fit SVM Model
set.seed(123)
model_svm <- train(Gender~finalgd+Chocolate+Programme+Stresslevel, data=ODI,  method="svmRadial",metric = metric, trControl=train_control, na.action = na.omit)
# # Fit KNN
set.seed(123)
model_knn <- train(Gender~finalgd+Chocolate+Programme+Stresslevel, data=ODI,  method="knn", metric = metric, trControl=train_control, na.action = na.omit)

# Summarise accuracy of models
results <- resamples(list(lda = model_lda, rf = model_rf, svm=model_svm,knn=model_knn))
summary(results)
dotplot(results)
#summarize best result
print(model_svm)
# estimate skill of LDA on the validation dataset
predictions <- predict(model_svm, validation)
confusionMatrix(predictions, validation$Gender)
