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
library(GGally)
library(ggpubr)

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
         Reward = ifelse(as.numeric(Reward)>100,100,as.numeric(Reward)), # non numeric entries become NA and specify that reward can't be more than the initial €100
         RandomNo = ifelse(abs(as.numeric(RandomNo))>100,NA,as.numeric(RandomNo)) # non numeric entries become NA and remove outliers
         ) %>% 
  select(-c('Standup')) # Drop standup

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

#### Clean missing values in numerical variables ####
ODI$Neighbours[is.na(ODI$Neighbours)] <- getmode(na.exclude(ODI$Neighbours)) 
ODI$Reward[is.na(ODI$Reward)] <- getmode(na.exclude(ODI$Reward))
ODI$RandomNo[is.na(ODI$RandomNo)] <- getmode(na.exclude(ODI$RandomNo))
ODI$Stresslevel[is.na(ODI$Stresslevel)] <- 50 # because stress level could be between 0-100

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
# bed1[bed1==19] <- -11
# bed1[bed1==20] <- -10
# bed1[bed1==21] <- -9
# bed1[bed1==22] <- -8
# bed1[bed1==23] <- -7
# bed1[bed1==0] <- -6
# bed1[bed1==1] <- -5
# bed1[bed1==2] <- -4
# bed1[bed1==3] <- -3
# bed1[bed1==4] <- -2
# bed1[bed1==5] <- -1

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

# ODI$Bedtime = factor(ODI$Bedtime, levels =c("19","20","21","22","23","0","1","2","3","4","5"),  ordered = T)
ODI$gd1 = goodday1_cluster
ODI$gd2 = goodday2_cluster

final_goodday = ifelse(ODI$gd1 == "Other", ODI$gd2, ODI$gd1)

ODI$finalgd = final_goodday


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
t1p1 <- ggplot(data=d, aes(x=variable, y=count, fill=value)) +
  geom_bar(stat="identity") +
  facet_grid(~Programme) +
  labs(x="Course", y="Count", fill= "participation") +
  theme(plot.title = element_text(size=16, margin=margin(t=20, b=20)), axis.text.x = element_text(size = 7)) +
  scale_fill_hue(direction = -1,labels = c("Yes", "No"))
t1p1
# ggsave(t1p1, file="plots/Background_info.eps")


##plot for nominal data
df2_choco = data.frame(raw$Gender, raw$Chocolate)
colnames(df2_choco) <- c("Gender","Chocolate")
df2_gd = data.frame(raw$Gender, ODI$finalgd)
colnames(df2_gd) <- c("Gender","Goodday")
df2_choco$Chocolate =
  ifelse(grepl("idea",df2_choco$Chocolate, ignore.case=T),"No Idea",raw$Chocolate)

meltdf2_choco <- melt(df2_choco, id.vars ="Gender",na.rm = T)
d2_choco <- with(meltdf2_choco, meltdf2_choco[order(Gender, variable, value),])
meltdf2_gd <- melt(df2_gd, id.vars ="Gender",na.rm = T)
d2_gd <- with(meltdf2_gd, meltdf2_gd[order(Gender, variable, value),])

#add a count col
d2_choco$count = rep(1)
d2_gd$count = rep(1)
#plot
t1p2_choco <- ggplot(data=d2_choco, aes(x=value, y=count, fill=Gender)) +
  geom_bar(stat="identity") +
  facet_grid(~variable)+
  labs(x="", y="Count", fill= "Gender") +
  theme(plot.title = element_text(size=16, margin=margin(t=20, b=20)), axis.text.x = element_text(size = 7))

t1p2_gd <- ggplot(data=d2_gd, aes(x=value, y=count, fill=Gender)) +
  geom_bar(stat="identity") +
  facet_grid(~variable)+
  labs(x="", y="Count", fill= "Gender") +
  theme(plot.title = element_text(size=16, margin=margin(t=20, b=20)), axis.text.x = element_text(size = 7))

t1p2 <- ggarrange(t1p2_choco,t1p2_gd,
                  ncol = 1, nrow = 2)
t1p2
# ggsave(t1p2, file="plots/Choco_Goodday.eps")

# pdf("plots/Corr_Matrix.pdf")
t1p3 <- ggpairs(ODI, columns = 10:14,
         columnLabels = c("birthyear","neighbours","stress","reward","random no."),
         diag=list(continuous="barDiag"),
         lower = list(continuous="cor")) +
    theme_minimal() +
       theme(
         axis.line = element_blank(),
         axis.text.y.left = element_blank(),
         axis.text.x.bottom = element_text(size = 7),
         axis.ticks.x.bottom = element_line()
       )
print(t1p3)
# dev.off()

#### Extract clean dataset ####



