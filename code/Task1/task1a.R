"
Data preprocessing 
Data Mining Techniques 
Assignment 1 
Task 1 A
"
options(warn=-1)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)

colnames(ODI)
dim(ODI)
ODI %>% head()

names = c("DateTime", "Programme", "MLCourse", "IRCourse", "StatCourse", "DBCourse", "Gender", "Chocolate",
          "Birthdate", "Neighbours", "Standup", "Stresslevel", "Reward", "RandomNo", "Bedtime", "Goodday1",
          "Goodday2")
raw = read_csv(here('data','ODI', 'ODI-2021.csv'), col_names = names) %>% as_tibble() %>% slice(-1)

ODI = raw %>% 
  separate(DateTime, sep = " ", into = c("Date", "Time")) %>% 
  mutate(MLCourse = as.integer(MLCourse == "yes"),
         IRCourse = na_if(IRCourse, "unknown"), IRCourse = as.integer(IRCourse == "1"),
         StatCourse = na_if(StatCourse, "unknown"), StatCourse = as.integer(StatCourse == "mu"),
         DBCourse = na_if(DBCourse, "unknown"), DBCourse = as.integer(DBCourse == "ja"),
         Gender = recode(Gender, unknown = 0, male = 1, female = -1),
         Chocolate = as.numeric(as.factor(Chocolate)), # [1 :"fat" 2 :"I have no idea what you are talking about" 3:"neither" 4 :"slim" 5 :"unknown"]
         Neighbours = na_if(as.integer(Neighbours), "NaN"), # Drop non numeric
         Stresslevel = as.integer(ifelse(str_detect(Stresslevel, regex('over', ignore_case = T)), NA, Stresslevel)) # Remove non numerical 
         ) %>% 
  select(-c('Standup')) # Drop standup

ODI[,'Neighbours'][ODI[,'Neighbours'] > 10] = NA # Replace higher than 10 (unreasonable values) by NA
ODI[,'Stresslevel'][ODI[,'Stresslevel'] < 0 | ODI[,'Stresslevel'] > 100] = NA # Remove outside range (0,100)

ODI %>% select(Stresslevel) %>% View()
ODI %>% View

TODO:

# Programme  
  # Make clusters based on regex conditions "If contains: ... "Assign group = 1:7)
# Birthdate
  # Extract Years
# Reward
# RandomNo
  # Drop str_detect "E"
  # Drop str_detect "character"
  # Amount of Digits? OR Histogram?
# Bedtime
  # Assign groups
# Gday1/2
