setwd("C:/Users/alexa/OneDrive/Documents/R learning/R Machine Learning Book Files/Student/Data")

library(caret)
library(rpart)
modelLookup("rpart")
library(tidyverse)
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# We know the paramaters now

# Begin partitioning data set 75% train and 25% test
set.seed(1234)
sample_set <- createDataPartition(y = income$income, p = .75,
                                   list = F)

income_train <- income[sample_set,]                                  
income_test <- income[-sample_set,]

# We know of the imbalance so we use SMOTE
library(DMwR)
set.seed(1234)
income_train <- SMOTE(income ~., data.frame(income_train),
                      perc.over = 100,
                      perc.under =200)
#Build and tune a model
#specify the training formula, 
#training data, performance evaluation metric (accuracy), 
#training algorithm (rpart), resampling technique (0.632 bootstrap),
#and number of resampling iterations (3).

set.seed(1234)
income_mod <- train( income ~ ., 
                     data = income_train, metric = "Accuracy",
                     method = "rpart", 
                     trControl = trainControl( method = "boot632", 
                                               number = 3) )
income_mod

income_pred <- predict(income_mod,income_test)
confusionMatrix(income_pred, income_test$income, positive = "<=50K")


#customized parameter tuning
# We can increase the default of 3 using the truelength argument in train

set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneLength = 20
)
income_mod

#Parameter grid creating in R
#This creates 18 different parameter combinations
expand.grid(
  .alpha = c(1,2,3),
  .beta = c(TRUE, FALSE),
  .gamma = seq(from = 4, to = 5, by = .5)
)

# we can use it for the actual parameters as well
tuneGrid <- expand.grid(.cp = seq(from = .0001, to = .002, by = .0001))
set.seed(1234)
income_mod <- train( income ~.,
                     data = income_train,
                     metric = "Accuracy", 
                     method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneGrid = tuneGrid)
income_mod

#TO see if model is not overfitting, lets predict the model against
# the testing data
income_pred <- predict(income_mod, income_test)
confusionMatrix(income_pred, income_test$income, positive = "<=50K")







#ENSEMBLE METHODS
install.packages("randomForest")
modelLookup("rf")

#RandomForest
set.seed(1234)
rf_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(.mtry = 3)
)

rf_pred <- predict(rf_mod, income_test)
confusionMatrix(rf_pred, income_test$income, positive = "<=50K")

#BOOSTING METHODS
install.packages("xgboost")
library(xgboost)
modelLookup("xgbTree")

set.seed(1234)
xgb_mod <- train(
  income~.,
  data = income_train,
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(
    nrounds  = 100,
    max_depth = 6,
    eta = .3,
    gamma = .01,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

xgb_pred <- predict(xgb_mod, income_test)
confusionMatrix(income_pred,income_test$income)

#STACKING ESSEMBLE
# we are changing the name because the stack assemble method does not do well
# with variables that start with special characters or numbers
library(tidyverse)
library(DMwR)
income <- income %>%
  mutate(income = as.factor(recode(income, "<=50K" = "Below", ">50K"=
                                     "Above")))
# re create the training and test partitions bc we changed the classifieers' names

library(caret)
set.seed(1234)
sample_set <- 
  createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set,]
income_test <- income[-sample_set,]
set.seed(1234)

income_train <- SMOTE(income~.,
                      data.frame(income_train),
                      perc.over = 100,
                      perc.under = 200)

#Now begin the stack ensemble
install.packages("caretEnsemble")
library(caretEnsemble)
ensembleLearners <- c("rpart", "glm", "knn")
library(stats)
library(class)

models <- caretList(
                    income ~ .,
                     data = income_train,
                     metric = "Accuracy",
                     methodList = ensembleLearners,
                     trControl = trainControl( method = "repeatedcv", 
                                               number = 10, repeats = 5, 
                                               savePredictions = "final", 
                                               classProbs = TRUE ) 
                    )
resamples(models)
