"
Prep:
 X Table of descriptives (mean var range )
 X Types, obvious correlations, 

Feature engineering: 
  X Drop: Name, Ticket, Embarked
  X Family size (SibSp + Parch)
    -> Plot familysize vs survived
  X Extract deck from cabin, make factor
  X Impute deck by fare + Pclass + family size
    -> Plot deck vs survived

Model: 
  X Split back into train, test
    -> 10 fold cross validation
  X Survived ~ Pclass + Sex + Age + FamSize + Cabin
    -> Binary classification problem
    -> Use two classification algorithms
       X -> randomForest
        -> ?
        
        # To improve: TRY WITH CARET PACKAGE
" 

remove(list = ls())
options(warn=-1)
set.seed(1337)

library(here)
library(tidyverse)
library(lubridate)
library(caret)
library(GGally)
library(tm) # Remove numbers
library('ggplot2') # visualization
library('ggthemes') # visualization
library('mice') # imputation
library('randomForest') # classification algorithm
library("e1071")

# Functions ##### 
cabinToDeck <- function(cabin){
  deck <- gsub('[[:digit:]]', '', cabin)
  deck <- substring(deck,1,1) %>% as_factor()
  return(deck)
}

countNA <- function(x){sum(is.na(x))}

# Rest ##### 

train_set <- read_csv('./data/titanic/train.csv')
test_set  <- read_csv('./data/titanic/test.csv')

train_size <- nrow(train_set) %>% as.numeric()

original_data <- bind_rows(train_set,test_set) %>% as_tibble()

cleaned_data <- original_data %>% 
  select(-c(Name, Ticket, Embarked)) %>% 
  mutate(PassengerId = as.factor(PassengerId),
         Sex = as_factor(Sex),
         Survived = as.factor(Survived),
         FamSize = SibSp + Parch,
         Deck = cabinToDeck(Cabin)) %>% 
  select(-c(SibSp, Parch, Cabin))

descriptives <- summary(cleaned_data[1:train_size,])
descriptives

missingvalues <- map(cleaned_data, countNA) %>% as_tibble()
missingvalues

temp_data <- mice(cleaned_data %>% select(-PassengerId,-Survived),
             m = 1, meth = 'pmm', maxit = 50, print=FALSE, seed = 1337)

imputed_data <- complete(temp_data, 1) %>% 
  as_tibble() %>% 
  mutate(PassengerId = cleaned_data$PassengerId, Survived = cleaned_data$Survived) %>% 
  select(8,1:7) %>%
  select(-Fare) 

missingvalues <- map(imputed_data, countNA) %>% as_tibble()
missingvalues

(desc_table <- summary(imputed_data))
# (desc_plot <- ggpairs(imputed_data %>% select(-c(PassengerId))))
  
# Two classifiers with cross validation
train_set <- imputed_data[1:train_size,]
test_set <- imputed_data[(train_size+1):nrow(imputed_data),]

train_control <- trainControl(method="cv", number=10)

model_rf <- train(Survived ~ Pclass + Sex + Age + FamSize + Deck,
                  data = train_set,  method="rf", metric = "Accuracy",
                  trControl=train_control,na.action = na.omit)

model_svm <- train(Survived ~ Pclass + Sex + Age + FamSize + Deck,
                   data = train_set,  method="svmRadial",metric = "Accuracy",
                   trControl=train_control, na.action = na.omit)

results <- resamples(list(rf = model_rf, svm = model_svm))
                     
summary(results)
dotplot(results)

predictions <- predict(model_rf, test_set)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test_set$PassengerId, Survived = predictions)

# Write the solution to file
write.csv(solution, file = './data/titanic/rf_mod_Solution.csv', row.names = F)

