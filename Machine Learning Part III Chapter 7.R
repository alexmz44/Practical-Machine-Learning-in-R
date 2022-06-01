setwd("C:/Users/alexa/OneDrive/Documents/R learning/R Machine Learning Book Files/Student/Data")
library(tidyverse)
email <- read_csv("email.csv")
head(email)

# Lots of ML Algs require that the class feature 
email <- email %>% mutate(message_label = as.factor(message_label))

#use gather to pivot each variable into two columns.
#gather(key, value, ignore value, ignore value)
email %>%
  filter(message_label == "ham") %>%
  gather(word,count, -message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count))%>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

email %>%
  filter(message_label == "spam") %>%
  gather(word,count, -message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count))%>%
  arrange(desc(occurrence)) %>%
  slice(1:10)
# Split the data into test and training sets

set.seed(1234)
sample_set <- sample(nrow(email), round(nrow(email)*.75), replace = F)
email_train <- email[sample_set,]
email_test <- email[-sample_set,]

# Check distributions
round(prop.table(table(select(email, message_label))),2)

round(prop.table(table(select(train, message_label))),2)

round(prop.table(table(select(test, message_label))),2)

# Build the model
#naive Bayes (learning formula,train dataset , laplace = 1)
library(e1071)
model <- naiveBayes(message_label ~ . -message_index, data = train, laplace = 1)

# evaluate model
library(stats)

# use the third argument "raw" to get predicted probabilities over predicted labels
email_pred <- predict(model,test, type = "raw")
head(email_pred)






