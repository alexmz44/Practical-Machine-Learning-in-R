setwd("C:/Users/alexa/OneDrive/Documents/R learning/R Machine Learning Book Files")
library(tidyverse)
income <- read_csv("income.csv", col_types = "nffnfffffnff")
glimpse(income)

# Partition data into training and test sets 
library(caret)
#createDataPartition(y = dependent variable, p = proportion of examples that should be in triaining set, list specifies the format that it should be returned)
set.seed(1234)
sample_set <- createDataPartition( y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set,] 
income_test <- income[-sample_set,]

#We know the data is imbalanced, so we will use the smote method
library(DMwR)
income_train <- SMOTE(income~.,
                      data.frame(income_train),
                      perc.over = 100,perc.under = 200)
 

# Validate data using k -fold approach
#train(training formula, 
#training data,
#metric = type of performance measure , 
#method = training method or algorithm to use,
#trControl = resampling technique)

#We will use the 5 fold by filling this out like so
library(caret)
library(rpart)
set.seed(1234)
income_mod <- train(
  income~.,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)


## use this code to get the accuracy of each of the folds which is 5 folds and
# then after you get the average accuracy of all of these 
income_mod$resample %>%
  arrange(Resample) %>%
  summarise(AvgAccuracy = mean(Accuracy))

#LOOCV leave one out cross validation
set.seed(1234)
income_mod <- train( income ~ ., data = income_train, metric = "Accuracy", method = "rpart", trControl = trainControl( method = "LOOCV") )

#Monte Carlo Validation is another common approach
# the train control portion is trainControl(mehtod = "LGOCV, p = .1, number = 10)
income_mod <- train(income ~.,
                    data = income_train,
                    metric = "Accuracy", 
                    method = "rpart",
                    trControl = trainControl(method  = "LGOCV", p=.1, number = 10))
income_mod$resample %>%
  arrange(Resample)


#BOOTSTRAPPING
#.632 bootstrap
income_mod <- train(income ~.,
                    data = income_train,
                    metric = "Accuracy",
                    method = "rpart", 
                    trControl = trainControl(method = "boot632"))
income_mod$resample %>%
  arrange(Resample)

#using email example
load("spam.RData")
spam_matrix <- confusionMatrix(email_pred, email_test$message_label, positive = "spam")







#PRECISON AND RECALL
#precision
posPredValue( email_pred, email_test $ message_label, positive = "spam")
#recall 
spam_recall <- spam_sensitivity



#Sensitivity and specificity 
sensitivity( email_pred, email_test $ message_label, positive = "spam")
specificity( email_pred, email_test $ message_label, positive = "spam")

#Fscore 
spam_fmeasure <- (2* spam_precision * spam_recall)/ (spam_precision + spam_recall)

#plotting ROC curves

library(e1071)
email_pred_prob <- predict(model, email_test, type = "raw")
head(email_pred_prob)


#generate a predicition object
library(ROCR)

roc_pred <- 
  prediction(
    predictions = email_pred_prob[, "spam"],
    labels = email_test$message_label
  )

#performance(prediction object, measure = "y axis", x.measure ="x axis")

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#plot ROC
plot(roc_perf, main = "ROC Curve", col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)


#Area under Curve (AUC)
auc_perf <- performance(roc_pred, measure = "auc")

spam_auc <- unlist(slot(auc_perf, "y.values"))
spam_auc
