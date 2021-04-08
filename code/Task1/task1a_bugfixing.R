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
library(stringr)

names = c("DateTime", "Programme", "MLCourse", "IRCourse", "StatCourse", "DBCourse", "Gender", "Chocolate",
          "Birthdate", "Neighbours", "Standup", "Stresslevel", "Reward", "RandomNo", "Bedtime", "Goodday1",
          "Goodday2")
#raw = read_csv(here('data','ODI', 'ODI-2021.csv'), col_names = names) %>% as_tibble() %>% slice(-1)
raw = read_csv("~/Studie/VU/MSc/Data_Mining_Techniques/Data-Mining-Techniques/data/ODI/ODI-2021.csv", col_names = names) %>% as_tibble() %>% slice(-1)
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

ODI$Programme[grepl("AI|artificial", ODI$Programme, ignore.case=TRUE)] <- "AI"
ODI$Programme[grepl("\\bCS\\b|Computer|Computational", ODI$Programme, ignore.case=TRUE)] <- "CS"
ODI$Programme[grepl("Bio", ODI$Programme, ignore.case=TRUE)] <- "BIO"
ODI$Programme[grepl("Finance|Duisenberg|QRM|Risk", ODI$Programme, ignore.case=TRUE)] <- "FIN"
ODI$Programme[grepl("Econometrics|EDS|EOR", ODI$Programme, ignore.case=TRUE)] <- "ECO"
ODI$Programme[grepl("BA|Business", ODI$Programme, ignore.case=TRUE)] <- "BUS"

count(ODI$Programme=="ECO")


str_count(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE))

count(str_subset(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)))
length(str_subset(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)))
replace(str_subset(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)), values = "ECO")

ODI$Programme <- ODI$Programme %>% 
  replace(str_subset(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)), values = "ECO")




str_replace_all(ODI$Programme,regex("AI|artificial", ignore_case = TRUE), "AI")
  str_view(regex("CS|Computer|Computational", ignore_case = TRUE))
  str_view(regex("Bio", ignore_case = TRUE))
  str_view(regex("Finance|Duisenberg|QRM|Risk", ignore_case = TRUE)) 
str_count(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)) 
  str_view(regex("BA|Business", ignore_case = TRUE))
  
Programme_cluster <- ODI$Programme %>% 
  str_view(regex("AI|artificial", ignore_case = TRUE), "AI") %>% 
  str_view(regex("CS|Computer|Computational", ignore_case = TRUE), "CS") %>%
  str_view(regex("Bio", ignore_case = TRUE), "BIO") %>% 
  str_view(regex("Finance|Duisenberg|QRM|Risk", ignore_case = TRUE), "FIN") %>% 
  str_view(regex("Econometrics|EDS|EOR", ignore_case = TRUE), "ECO") %>% 
  str_view(regex("BA|Business", ignore_case = TRUE), "BUS")

Programme_cluster <- rep(0,313)
for (i in (1:313)){
  if (str_detect(ODI$Programme[i],regex("AI|artificial", ignore_case = TRUE))){
    Programme_cluster[i] = "AI"
  } else if (str_detect(ODI$Programme[i],regex("CS|Computer|Computational", ignore_case = TRUE))){
    Programme_cluster[i] = "CS"
  } else if (str_detect(ODI$Programme[i],regex("Bio", ignore_case = TRUE))){
    Programme_cluster[i] = "BIO"
  } else if (str_detect(ODI$Programme[i],regex("Finance|Duisenberg|QRM|Risk", ignore_case = TRUE))){
    Programme_cluster[i] = "FIN"
  } else if (str_detect(ODI$Programme[i],regex("Econometrics|EDS|EOR", ignore_case = TRUE))){
    Programme_cluster[i] = "ECO"
  } else if (str_detect(ODI$Programme[i],regex("BA|Business", ignore_case = TRUE))){
    Programme_cluster[i] = "BUS"
  }
}
Programme_cluster
ODI$Programme

str_count(Programme_cluster,"ECO")
Programme_cluster =
    ifelse(str_detect(ODI$Programme,regex("AI|artificial", ignore_case = TRUE)), "AI",
    ifelse(str_detect(ODI$Programme,regex("CS|Computer|Computational", ignore_case = TRUE)), "CS",
    ifelse(str_detect(ODI$Programme,regex("Bio", ignore_case = TRUE)), "BIO",
    ifelse(str_detect(ODI$Programme,regex("Finance|Duisenberg|QRM|Risk", ignore_case = TRUE)), "FIN",
    ifelse(str_detect(ODI$Programme,regex("Econometrics|EDS|EOR", ignore_case = TRUE)), "ECO",
    ifelse(str_detect(ODI$Programme,regex("BA|Business", ignore_case = TRUE)), "BUS",
    "Other"))))))

# Programme_cluster =
#   ifelse(grepl(paste(c("AI","artificial"), collapse = "|"), ODI$Programme, ignore.case=T), "AI",
#   ifelse(grepl(paste(c("CS","Computer","Computational"), collapse = "|"), ODI$Programme, ignore.case=T), "CS",
#   ifelse(grepl("Bio", ODI$Programme, ignore.case=T), "BIO",
#   ifelse(grepl(paste(c("Finance","Duisenberg","QRM","Risk"), collapse = "|"), ODI$Programme, ignore.case=T), "FIN",
#   ifelse(grepl(paste(c("Econometrics","EDS","EOR"), collapse = "|"), ODI$Programme, ignore.case=T), "ECO",
#   ifelse(grepl(paste(c("BA","Business"), collapse = "|"), ODI$Programme, ignore.case=T), "BIZ",
#   "Other"))))))



ODI <- ODI %>% 
  mutate(
    Programme = Programme_cluster,
    Date = mdy(Date),
    Time = hms(Time))
#unique(ODI$Programme)

#####Plotting#####
##Stacked plot's data
df = ODI[,3:7]
colnames(df) <- c("Programme","ML", "IR","St","DB")
#Wide to long for plotting
meltd <- melt(df, id.vars ="Programme",na.rm = T)
#Order the column's value for stacked plot
d <- with(meltd, meltd[order(Programme, variable, value),])
#standardise binary value
d$value = factor(d$value, levels = c("1","0","mu","sigma","ja","nee"), labels = c(1,0,1,0,1,0))
#create a column with value of 1 for the y axis
d$count = rep(1)
#plot
ggplot(data=d, aes(x=variable, y=count, fill=value)) + 
  geom_bar(stat="identity") + 
  facet_grid(~Programme) +
  labs(title="Student Academic Background Info", x="Course", y="Count", fill="Participation") + 
  theme(plot.title = element_text(size=25, margin=margin(t=20, b=20)))

sum(d$Programme =="ECO")
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

