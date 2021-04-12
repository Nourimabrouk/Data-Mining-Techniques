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

ODI = read.csv("./data/ODI/ODI-2021_clean.csv",header = T, sep =";") %>% as_tibble()
