library(tidyverse)
library(quanteda)

# Dont use naieve bayes
# Follow quanteda tutorial


sms <- read_delim("data/sms/SmsCollection.csv", 
                  ";", escape_double = FALSE, trim_ws = TRUE) %>% as_tibble()
table(sms$label)
names(sms)
sms %>% head()


msg.corpus<-corpus(sms$text)
#separating Train and test data
sms_train<-sms[1:4458,]
sms_test<-sms[4458:nrow(sms),]

msg.dfm <- dfm(msg.corpus, tolower = TRUE)  #generating document freq matrix
msg.dfm <- dfm_trim(msg.dfm, min_count = 5, min_docfreq = 3)  
msg.dfm <- dfm_weight(msg.dfm) 
#trining and testing data of dfm 
msg.dfm.train<-msg.dfm[1:4458,]

msg.dfm.test<-msg.dfm[4458:nrow(sms),]
nb.classifier<-textmodel_nb(msg.dfm.train,sms.train[,1])
nb.classifier
pred<-predict(nb.classifier,msg.dfm.test)
table(predicted=pred$nb.predicted,actual=sms.test[,1])
mean(pred$nb.predicted==sms.test[,1])*100

