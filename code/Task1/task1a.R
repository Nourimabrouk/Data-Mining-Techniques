"
Data preprocessing 
Data Mining Techniques 
Assignment 1 
Task 1 A
"
remove(list = ls())
options(warn=-1)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plyr)

names = c("DateTime", "Programme", "MLCourse", "IRCourse", "StatCourse", "DBCourse", "Gender", "Chocolate",
          "Birthdate", "Neighbours", "Standup", "Stresslevel", "Reward", "RandomNo", "Bedtime", "Goodday1",
          "Goodday2")
#raw = read_csv(here('data','ODI', 'ODI-2021.csv'), col_names = names) %>% as_tibble() %>% slice(-1)
raw = read_csv("./data/ODI/ODI-2021.csv", col_names = names) %>% as_tibble() %>% slice(-1)
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

colnames(ODI)
dim(ODI)
ODI %>% head()

ODI[,'Neighbours'][ODI[,'Neighbours'] > 10] = NA # Replace higher than 10 (unreasonable values) by NA
ODI[,'Stresslevel'][ODI[,'Stresslevel'] < 0 | ODI[,'Stresslevel'] > 100] = NA # Remove outside range (0,100)

Programme_cluster =
    ifelse(grepl("\\bBA\\b|\\bBusiness\\b", ODI$Programme, ignore.case=T), "BUS",
    ifelse(grepl("\\bAI\\b|\\bartificial\\b", ODI$Programme, ignore.case=T), "AI",
    ifelse(grepl("\\bEconometrics\\b|\\bEDS\\b|\\bEOR\\b|\\bOR\\b", ODI$Programme, ignore.case=T), "ECO",
    ifelse(grepl("Bio", ODI$Programme, ignore.case=T), "BIO",
    ifelse(grepl("\\bFinance\\b|\\bDuisenberg\\b|\\bQRM\\b|\\bquant|\\bFin|\\bF&T\\b", ODI$Programme, ignore.case=T), "FIN",
    ifelse(grepl("\\bCS\\b|\\bComputer\\b|\\bComputational\\b|\\binformation|\\bdata", ODI$Programme, ignore.case=T), "CS",
    "Other"))))))

goodday1_cluster =
  ifelse(grepl("sun| weather| sun|  wheather| rain| spring| summer",
               ODI$Goodday1, ignore.case=T), "Weather",
         
  ifelse(grepl("coff|\\bfood\\b| cofee| coffe| drink | tea| wine| beer|
               rice| pasta| meal| breakfast| lunch| dinner| brunch| stomach| cream"
               , ODI$Goodday1, ignore.case=T), "F&B",
                
  ifelse(grepl("do| done| doing| finish| deadline| task| study| work| course|
               get| plan|  learn| efficien| control|
               productive| motivat| zoom| project| grade| win"
               , ODI$Goodday1, ignore.case=T), "Goals",    
                       
  ifelse(grepl("family| social| friend| talk| hanging| meeting| going| act| company|
               party| corona| talk| people"
               , ODI$Goodday1, ignore.case=T), "Social",
                              
  ifelse(grepl("workout| exercise| jogging| running| out| sport"
               , ODI$Goodday1, ignore.case=T), "Exercise",
                                     
  ifelse(grepl(" relax| chill| sleep| free| music| no| walk| sex| 
               mood |rest| tired| stress| not", 
               ODI$Goodday1, ignore.case=T), "Resting",
  "Other"))))))

goodday2_cluster =
  ifelse(grepl("sun| weather| sun|  wheather| rain| spring| summer",
              ODI$Goodday2, ignore.case=T), "Weather",
         
  ifelse(grepl("coff|\\bfood\\b| cofee| coffe| drink | tea| wine| beer|
              rice| pasta| meal| breakfast| lunch| dinner| brunch| stomach| cream"
              , ODI$Goodday2, ignore.case=T), "F&B",
                
  ifelse(grepl("do| done| doing| finish| deadline| task| study| work| course|
              get| plan|  learn| efficien| control|
              productive| motivat| zoom| project| grade| win"
              , ODI$Goodday2, ignore.case=T), "Goals",    
                       
  ifelse(grepl("family| social| friend| talk| hanging| meeting| going| act| company|
               party| corona| talk| people"
              , ODI$Goodday2, ignore.case=T), "Social",
                              
  ifelse(grepl("workout| exercise| jogging| running| out| sport"
              , ODI$Goodday2, ignore.case=T), "Exercise",
                                     
  ifelse(grepl(" relax| chill| sleep| free| music| no| walk| sex| 
               mood |rest| tired| stress| not", 
         ODI$Goodday2, ignore.case=T), "Resting",
         "Other"))))))


ODI <- ODI %>% 
  mutate(
    Programme = Programme_cluster,
    Date = mdy(Date),
    Time = hms(Time))
ODI$gd1 = goodday1_cluster
ODI$gd2 = goodday2_cluster

final_goodday = ifelse(ODI$gd1 == "Other", ODI$gd2, ODI$gd1)

ODI$finalgd = final_goodday
#unique(ODI$Programme)

#####Plotting#####
##Stacked plot's data
df = ODI[,3:7]
colnames(df) <- c("Programme","ML", "IR","St","DB")

#testing the match
# correctmajor = read_csv("./data/ODI/programme.csv")
# test = data.frame(correctmajor,df$Programme)
# 
# test$test = test$Class == test$df.Programme
# count(test$test)

#Wide to long for plotting
meltd <- melt(df, id.vars ="Programme",na.rm = T)
#Order the column's value for stacked plot
d <- with(meltd, meltd[order(Programme, variable, value),])
#standardise binary value
d$value = factor(d$value, levels = c("1","0","mu","sigma","ja","nee"), labels = c(1,0,1,0,1,0))
#create a column with value of 1 for the y axis
d$count = rep(1)
#plot
t1p1 <- ggplot(data=d, aes(x=variable, y=count, fill=value)) +
  geom_bar(stat="identity") +
  facet_grid(~Programme) +
  labs(title="Student Academic Background Info", x="Course", y="Count", fill= "participation") +
  theme(plot.title = element_text(size=16, margin=margin(t=20, b=20)), axis.text.x = element_text(size = 7)) +
  scale_fill_hue(direction = -1,labels = c("Yes", "No"))
# ggsave(t1p1, file="plots/Background_info.eps")

TODO


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

