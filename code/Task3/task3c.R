library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(glmnet)
library(tm)
library(SnowballC)
library(Matrix)
library(ROCR)
# Dont use naieve bayes
# Follow quanteda tutorial

remove(list = ls())

sms <- read_delim("data/sms/SmsCollection.csv", 
                  ";", escape_double = FALSE, trim_ws = TRUE) %>% as_tibble()
table(sms$label)
names(sms)
sms %>% head()

####REGULARIZED REGRESSION CLASSIFIER####
str(sms)
# convert spam/ham to factor.
sms$label <- factor(sms$label)

# tokenize texts
# create DTM using TM package

sms_dtm <- Corpus(VectorSource(sms$text)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  DocumentTermMatrix()

#creating train and test data
index = sample(5534, 5534*0.8)

sms_train_matrix <- as.matrix(sms_dtm[index, ])
sms_test_matrix <- as.matrix(sms_dtm[-index, ])

sms_dtm_train <- Matrix(sms_train_matrix, sparse = T)
sms_dtm_test  <- Matrix(sms_test_matrix, sparse = T)

# save the labels
sms_train_labels <- sms[index, ]$label
sms_test_labels  <- sms[-index, ]$label

# check for the proportion of train and test
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
#train model
lasso <- cv.glmnet(x = sms_dtm_train,
                   y = as.integer(sms_train_labels == "ham"),
                   alpha = 1,
                   nfold = 5,
                   family = "binomial")
#As an initial evaluation of the model, we print the most predictive features. 
#We begin by obtaining the best value of lambda:
index_best <- which(lasso$lambda == lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[, index_best]
head(sort(beta, decreasing = TRUE), 20)

pred <- predict(lasso, sms_dtm_test, type = "response", s = lasso$lambda.min)
head(pred)


#Let’s inspect how well the classification worked.

actual_class <- as.integer(sms_test_labels == "ham")
predicted_class <- as.integer(predict(lasso, sms_dtm_test, type = "class"))
tab_class <- table(actual_class, predicted_class)
tab_class

#We can use the function confusionMatrix() from the caret package to quantify the performance of the classification.

confusionMatrix(tab_class, mode = "everything")



