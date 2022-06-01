setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")

# The goal is to try to use other nonincome factors to classify whether or not
# they have an annual income of >$50,000
library(tidyverse)

income <- read_csv("income.csv", col_types = "nffnfffffnff")

glimpse(income)

# for the purpose of this case study, I am going to focus on just the categorical variables
income %>%
  keep(is.factor) %>%
  summary()



#  We can only see the top 6 distributions. Use a table to see all the values for each feature

table(select(income, workClassification))

table( select( income, educationLevel))

table( select( income, occupation))

table( select( income, nativeCountry))

# we have to replace the "?" value. Since these are factors, we do 
#not use the ifelse() function, we use the recode( ) function

income <- income %>%
  mutate(workClassification = recode(workClassification ,"?" ="UNK")) %>%
  mutate(nativeCountry = recode(nativeCountry, "?" = "UNK")) %>%
  mutate(occupation = recode(occupation, "?" = "UNK"))

# We want to create these into a classification problem that is easier to read
# the two class values that are in the dataset are <= 50K and >50K - we will change
# these to "1" and "0", respectively 

income <- income %>%
  mutate(income = recode(income, "<=50K" = "0")) %>%
  mutate(income = recode(income, ">50K" = "1"))

# checking to see if this worked
summary(income[,"income"])

# It is time to split the data into 75% (Training Set) & 25% (test set)
#income_train and income_test
set.seed(1234)

# we are sampling from the number of rows in the data set, we want to take 75 %
# of that number to create the training set
sample_set <- sample(nrow(income), round(nrow(income)*.75), replace = F)

#we can use the subsetting to get a random sample of 75% of the data set
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

# we need to check the class distribution of all three datasets 
# to make sute that they are similar

round(prop.table(table(select(income,income), exclude = NULL)), 4) *100

round(prop.table(table(select(income_train,income), exclude = NULL)), 4) *100

round(prop.table(table(select(income_test,income), exclude = NULL)), 4) *100

# the data has similar class distributions
# We will use the smote function in order to balance the data
# this allows for a larger representation of the classification
# for people with an income over 50K a year
library(DMwR)
set.seed(1234)
income_train <- SMOTE(income ~., data.frame(income_train), perc.over =
                        100, perc.under=200)

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100


# Training the model 
# We will use only categorical features to build our model

summary(income_train[, "income"])

income_model1 <- income_train %>%
  keep(is.factor) %>%
  glm(formula = income ~., family = binomial)

summary(income_model1)

# We have the model, lets now make predictions against the test data

income_pred1 <- predict(income_model1, income_test, type ='response')
head(income_pred1)

# the predictions provide a prob that income = 1

# we need to determine an ideal cutoff value
library(InformationValue)

ideal_cutoff <- 
  optimalCutoff(
    actuals = income_test$income,
    predictedScores = income_pred1,
    optimiseFor = "Both"
  )
summary(ideal_cutoff)

# we apply this to our predictions
income_pred1 <- ifelse(income_pred1 >= 
                         ideal_cutoff, 1, 0)

head(income_pred1)


# we are ready to see how well our model does against the test data
# To do so we need top make a confusion matrix
income_pred1.table <- table(income_test$income, income_pred1)
income_pred1.table

sum(diag(income_pred1.table)) / nrow(income_test)

# the predictive accuracy is about 73% not bad considering we did
# not include the continuous variables
