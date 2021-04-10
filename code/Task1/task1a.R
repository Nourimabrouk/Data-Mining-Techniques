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
library(anytime)

#### self-built functions ####
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####Import dataset#####
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
         Stresslevel = as.integer(ifelse(str_detect(Stresslevel, regex('over', ignore_case = T)), NA, Stresslevel)), # Remove non numerical 
         Reward = ifelse(as.numeric(Reward)>100,NA,as.numeric(Reward)), # non numeric entries become NA and specify that reward can't be more than the initial ???100
         RandomNo = as.numeric(RandomNo) # non numeric entries become NA and remove outliers
         ) %>% 
  select(-c('Standup')) # Drop standup

hist(ODI$RandomNo)
boxplot(ODI$RandomNo)

colnames(ODI)
dim(ODI)
ODI %>% head()

ODI[,'Neighbours'][ODI[,'Neighbours'] > 10] = NA # Replace higher than 10 (unreasonable values) by NA
ODI[,'Stresslevel'][ODI[,'Stresslevel'] < 0 | ODI[,'Stresslevel'] > 100] = NA # Remove outside range (0,100)

##### program ####
Programme_cluster =
    ifelse(grepl("\\bBA\\b|\\bBusiness\\b", ODI$Programme, ignore.case=T), "BUS",
    ifelse(grepl("\\bAI\\b|\\bartificial\\b", ODI$Programme, ignore.case=T), "AI",
    ifelse(grepl("\\bEconometrics\\b|\\bEDS\\b|\\bEOR\\b|\\bOR\\b", ODI$Programme, ignore.case=T), "ECO",
    ifelse(grepl("Bio", ODI$Programme, ignore.case=T), "BIO",
    ifelse(grepl("\\bFinance\\b|\\bDuisenberg\\b|\\bQRM\\b|\\bquant|\\bFin|\\bF&T\\b", ODI$Programme, ignore.case=T), "FIN",
    ifelse(grepl("\\bCS\\b|\\bComputer\\b|\\bComputational\\b|\\binformation|\\bdata", ODI$Programme, ignore.case=T), "CS",
    "Other"))))))


###### birthdates ######
birthdates <- dmy(ODI$Birthdate, truncated = 2)
birthdates <- as.numeric(format(birthdates, format="%Y"))
birthdates[is.na(birthdates)|birthdates==0|birthdates>2005|birthdates<1950] <- median(birthdates, na.rm = T)

#####Bed time#####
bed1 <- hm(ODI$Bedtime) 
bed1 <- bed1@hour
bed1 <- ifelse(nchar(as.character(bed1))>2,str_extract(as.character(bed1), "^.{2}"), as.character(bed1))
bed1 <- ifelse(as.numeric(bed1)>24,0,as.numeric(bed1))
bed1[bed1==7] <- 19
bed1[bed1==8] <- 20
bed1[bed1==9] <- 21
bed1[bed1==10] <- 22
bed1[bed1==11] <- 23
bed1[bed1==12|bed1==24] <- 0
bed1[bed1>5 & bed1<19] <- NA
bed1[is.na(bed1)] <- getmode(na.exclude(bed1))


#####Goodday######
goodday1_cluster =
  ifelse(grepl("sun|weather|sun|wheather|rain|spring|summer",
               ODI$Goodday1, ignore.case=T), "Weather",
  ifelse(grepl("coff|\\bfood\\b|cofee|coffe|drink|\\btea\\b|wine|beer|rice|pasta|meal|breakfast|lunch|dinner|brunch|stomach|cream",
               ODI$Goodday1, ignore.case=T), "F&B",
  ifelse(grepl("do|done|doing|finish|deadline|task|study|work|course|get|plan|learn|efficien|control|productive|motivat|zoom|project|grade|win",
               ODI$Goodday1, ignore.case=T), "Goals",    
  ifelse(grepl("family|social|friend|talk|hanging|meeting|going|act|company|party|corona|talk|people",
               ODI$Goodday1, ignore.case=T), "Social",
  ifelse(grepl("workout|exercise|jogging|running|out|sport|soccer|swim",
               ODI$Goodday1, ignore.case=T), "Exercise",
  ifelse(grepl("relax|chill|sleep|free|music|no|walk|sex|mood|rest|tired|stress|not", 
               ODI$Goodday1, ignore.case=T), "Resting",
  "Other"))))))

goodday2_cluster =
  ifelse(grepl("sun|weather|sun|wheather|rain|spring|summer",
               ODI$Goodday2, ignore.case=T), "Weather",
  ifelse(grepl("coff|\\bfood\\b|cofee|coffe|drink|\\btea\\b|wine|beer|rice|pasta|meal|breakfast|lunch|dinner|brunch|stomach|cream",
               ODI$Goodday2, ignore.case=T), "F&B",
  ifelse(grepl("do|done|doing|finish|deadline|task|study|work|course|get|plan|learn|efficien|control|productive|motivat|zoom|project|grade|win",
               ODI$Goodday2, ignore.case=T), "Goals",
  ifelse(grepl("family|social|friend|talk|hanging|meeting|going|act|company|party|corona|talk|people",
               ODI$Goodday2, ignore.case=T), "Social",
  ifelse(grepl("workout|exercise|jogging|running|out|sport|soccer|swim",
               ODI$Goodday2, ignore.case=T), "Exercise",
  ifelse(grepl("relax|chill|sleep|free|music|no|walk|sex|mood|rest|tired|stress|not",
               ODI$Goodday2, ignore.case=T), "Resting",
  "Other"))))))

#####dataset mutate####
ODI <- ODI %>% 
  mutate(
    Programme = Programme_cluster,
    Date = mdy(Date),
    Time = hms(Time),
    Bedtime = bed1,
    Birthdate = birthdates)
ODI$gd1 = goodday1_cluster
ODI$gd2 = goodday2_cluster

final_goodday = ifelse(ODI$gd1 == "Other", ODI$gd2, ODI$gd1)

ODI$finalgd = final_goodday


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



