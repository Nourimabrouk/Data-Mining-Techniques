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

ODI = read_csv("./data/ODI/ODI-2021_clean.csv") %>% as_tibble() %>% slice(-1)



# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# Summarise Results
print(model)