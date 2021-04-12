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
dGDP <- df %>% filter(Indicator.Code=="NGDP_R_PC_PP_PT")
dCPI <- df %>% filter(Indicator.Code=="PCPI_PC_PP_PT")