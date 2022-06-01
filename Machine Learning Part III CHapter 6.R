## In this chapter, we will leanr about K nearest neighbor, also known ad
# part of the lazy learner family
setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")
library(readr)
heart <- read_csv("heart.csv",col_types = "nffnnffnfnfnff")
library(tidyverse)
glimpse(heart)

summary(heart)

# we can use imputations to replace NA's or use an indicator value
# but for practice, we are going to just remove all the NA's 

heart <- heart %>% 
  filter(!is.na(restingBP) & !is.na(cholesterol) & !is.na(highBloodSugar) & !is.na(restingECG)
         & !is.na(restingHR) & !is.na(exerciseAngina) & !is.na(STdepression)
         & !is.na(STslope) & !is.na(coloredVessels) & !is.na(defectType))

## we are going to use a function that ca be used on many different variables to normalize them
# this is the min max normalization method btw
normalize <- function(x){
  return((x - min(x))/ (max(x)- min(x)))
}

heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))

## all numeric features are within 0 and 1
summary(heart)

# now it is time to deal with the categorical variables 
# we can use the dummy.dataframe() function to make them dummys at scale
# that function only takes data frames so we need to make our tiblle into a df

heart <- data.frame(heart)
head(heart)

#we need to split the classification labels from the dataset
# we do this bc we do not want to create dummys for our class

heart_labels <- heart %>% select(heartDisease)
heart <- heart %>% select(-heartDisease)

# colnames to see our features so we can compare them to the old ones
colnames(heart)

library(dummies)

## this creates the dummy variables and adds an underscore between the 
# dummy variables
heart <- dummy.data.frame(data= heart, sep = "_")
colnames(heart)

## creating the sample and test datasets
set.seed(1234)
sample_index <-sample(nrow(heart), round(nrow(heart)*.75),replace = F)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# do the same split for the class
heart_train_labels <- as.factor(heart_labels[sample_index,])
heart_test_labels <- as.factor(heart_labels[-sample_index,])

# we use as.factor for the class variable because it makes a df into 
# a vector of factor values also knn() requires it

## time to use k nearest neighbors
# knn(train data, test data, class labels, k or number of neighbors to be cosidered)
# we will use k = 15 because  sqrt(224) = 15
library(class)
heart_pred1 <- 
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 15
  )
head(heart_pred1)
heart_pred1_table <- table(heart_test_labels, heart_pred1)
heart_pred1_table

sum(diag(heart_pred1_table))/ nrow(heart_test)

#K-NN can be used numerically. We use the average of all the neighbors
# also, we do not use predictive accuracy like its a classification problem. 
# we use the root mean squared error

#IMPROVING THE MODEL

# lets set k = 1 to see if the model gets worse or better
heart_pred2 <- 
  knn(train = heart_train,
      test = heart_test,
      cl = heart_train_labels,
      k = 1)
heart_pred2_table<- table(heart_test_labels, heart_pred2)
sum(diag(heart_pred2_table))/ nrow(heart_test)

# since the accuracy lowered , let us try to go the opposite way to see what happens

heart_pred3 <- 
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 40
  )
heart_pred3_table <- table(heart_pred3, heart_test_labels)
sum(diag(heart_pred3_table)) / nrow(heart_test)


### using k-NN to solve the ch. 5 classification problem on whether some will donate or
## not 

donors <- read_csv("donors.csv", col_types = "nnnnnnnnnnnnffffffffff")
glimpse(donors)

# we are only going to use the numeric variables to as predictors
 donors_numeric <- donors %>%
   keep(is.numeric)
glimpse(donors_numeric) 

donors <- donors_numeric %>%
  mutate(respondedMailing = donors$respondedMailing)
glimpse(donors)

summary(donors)

# lets take care of the missing values. For age we will do mean imputation

donors <- donors %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = T), age))
summary(select(donors,age))

# number of children has 83,000 missing vaolues. For this we use median imputation

donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = T),numberChildren))

donors %>%
  select(numberChildren) %>%
  summary()

# for incomerating and wealthrating, we exclude instances from our dataset. Wealthrating 
# has values that are 0 when the actual scales only work for 1 through 9
# we must exclude those too
donors <- donors %>%
  filter(!is.na(wealthRating) & !is.na(incomeRating) & wealthRating > 0)
summary(donors)

# Normalize the data

normalize <- function(x){
  return((x - min(x))/ (max(x) - min(x)))
}

donors <- donors %>%
  mutate( age = normalize( age)) %>% 
  mutate( numberChildren = normalize( numberChildren)) %>% 
  mutate( incomeRating = normalize( incomeRating)) %>% 
  mutate( wealthRating = normalize( wealthRating)) %>% 
  mutate( mailOrderPurchases = normalize( mailOrderPurchases)) %>% 
  mutate( totalGivingAmount = normalize( totalGivingAmount)) %>%
  mutate( numberGifts = normalize( numberGifts)) %>% 
  mutate( smallestGiftAmount = normalize( smallestGiftAmount)) %>% 
  mutate( largestGiftAmount = normalize( largestGiftAmount)) %>% 
  mutate( averageGiftAmount = normalize( averageGiftAmount)) %>% 
  mutate( yearsSinceFirstDonation = normalize( yearsSinceFirstDonation)) %>%
  mutate( monthsSinceLastDonation = normalize( monthsSinceLastDonation))

summary(donors)

# make sure that everything is between 1 and 0

donors <- data.frame(donors)

set.seed(1234)
sample_set <- sample(nrow(donors), round(nrow(donors)*.75), replace = F)
donors_train <- donors[sample_index,]
donors_test <- donors[-sample_index,]


## there is an inherent imbalance in the dataset
round(prop.table(table(select(donors, respondedMailing),exclude = NULL)), 4)*100
round(prop.table(table(select(donors_train, respondedMailing),exclude = NULL)), 4)*100
round(prop.table(table(select(donors_test, respondedMailing),exclude = NULL)), 4)*100

# balancing the data using smote
library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ ., donors_train, perc.over = 100, perc.under = 200)
round(prop.table(table(select(donors_train, respondedMailing),exclude = NULL)), 4)*100

# we need to hold the labels into different datasets
donors_train_labels <- as.factor(pull(donors_train, respondedMailing))
donors_train_labels

donors_test_labels <- as.factor(pull(donors_test, respondedMailing))
donors_test_labels

donors_train <- data.frame(select(donors_train, -respondedMailing))
donors_test <- data.frame(select(donors_test, -respondedMailing))

## Building the model

library(class)
donors_pred <- knn(
  train = donors_train,
  test = donors_test,
  cl = donors_train_labels,
  k= 5
)

# evaluating the model
donors_pred_table <- table(donors_test_labels, donors_pred)
donors_pred_table

sum(diag(donors_pred_table)) / nrow((donors_test))




