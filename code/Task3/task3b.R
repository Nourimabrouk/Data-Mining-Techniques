"
MAE vs. MSE analysis
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

df = read.csv("./data/economy_asia/economy_asia.csv",header = T, sep =",") %>% as_tibble()
dGDP <- df %>% filter(Indicator.Code=="NGDP_R_PC_PP_PT") %>% select(c("Country" = "Country.Code","GDP"="X2017"))
dCPI <- df %>% filter(Indicator.Code=="PCPI_PC_PP_PT") %>% select(c("Country" = "Country.Code","CPI"="X2017"))
dExport <- df %>% filter(Indicator.Code=="TX_R_PC_PP_PT") %>% select(c("Country" = "Country.Code","Export"="X2017"))

dGDP$CPI <- dCPI$CPI
train_data <- inner_join(dGDP,dExport,by = "Country")


# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)

#Fit Linear regression
set.seed(123)
model_lm <- train(CPI~GDP+Export, data = train_data, method="lm", trControl=train_control, na.action = na.omit)
model_lm$results$MAE
model_lm$results$RMSE^2
# print(model_lm)

#Fit Lasso regression
set.seed(123)
model_lasso <- train(CPI~GDP+Export, data = train_data, method="lasso", trControl=train_control, na.action = na.omit)
model_lasso$results$MAE[[1]]
model_lasso$results$RMSE[[1]]^2
# print(model_lasso)

# fit gaussian process with polynomial kernel regression
set.seed(123)
model_gausspoly <- train(CPI~GDP+Export, data = train_data, method="gaussprPoly", trControl=train_control, na.action = na.omit)
model_gausspoly$results$MAE[[6]]
model_gausspoly$results$RMSE[[6]]^2
# print(model_gausspoly)

# fit bayesian glm
set.seed(123)
model_bayesglm <- train(CPI~GDP+Export, data = train_data, method="bayesglm", trControl=train_control, na.action = na.omit)
model_bayesglm$results$MAE
model_bayesglm$results$RMSE^2
# print(model_bayesglm)



