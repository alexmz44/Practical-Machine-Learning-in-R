#Finding the impact of the model when we add categorical and numeric features alike
library(tidyverse)
#Collect the data 
donors <- read_csv("donors.csv", col_types = "nnnnnnnnnnnnffffffffff")


summary(donors)


#CATEGORICAL VARIABLES
#Before we get to fixing the incomerating and wealthrating NA's, we will
#do the categorical variables. replace NA's with 'UNK'
donors <- donors %>% 
  mutate(urbanicity = as.character(urbanicity))%>% 
  mutate(urbanicity = as.factor( ifelse (urbanicity == 'NA', 'UNK', urbanicity))) %>%
  mutate(socioEconomicStatus= as.character(socioEconomicStatus))%>% 
  mutate(socioEconomicStatus = as.factor( ifelse (socioEconomicStatus == 'NA', 'UNK', socioEconomicStatus))) %>%
  mutate(isHomeowner= as.character(isHomeowner))%>% 
  mutate(isHomeowner = as.factor( ifelse (isHomeowner == 'NA', 'UNK', isHomeowner)))%>%
  mutate(gender= as.character(gender))%>% 
  mutate(gender = as.factor( ifelse (gender == 'NA', 'UNK', gender)))

#NUMERIC VARIABLES
#Now lets take care of the NA's in the numeric variables
#For age, we are going to impute them by gender
#This means that it gets the mean age for males and then a different mean
#age for females mean age for UNK, mean age for joint. The code below replaces 
# of the genders accordingly.the NA's of each gender
donors <- donors %>%
  group_by(gender) %>%
mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>%
  ungroup() 
summary( select(donors,age))

# We will use median for numberChildrn

donors <- donors %>% 
  mutate(numberChildren = ifelse(is.na(numberChildren),
                                 median(numberChildren, na.rm = T), numberChildren))
summary(donors)

# for income and wealth rating, we will simply just leave out the Na's

donors <- donors %>%
  filter(!is.na(incomeRating) & !is.na(wealthRating) & wealthRating>0)

# now it is time to normalize the numeric features using min max
normalize <- function(x) {
  return((x - min(x))/ (max(x) - min(x)))
}

donors <- donors %>%
  mutate(age = normalize(age)) %>% 
  mutate(numberChildren = normalize(numberChildren)) %>%
  mutate(incomeRating = normalize(incomeRating)) %>%
  mutate(wealthRating = normalize(wealthRating)) %>%
  mutate(mailOrderPurchases = normalize(mailOrderPurchases)) %>%
  mutate(totalGivingAmount = normalize(totalGivingAmount)) %>%
  mutate(numberGifts = normalize(numberGifts)) %>%
  mutate(smallestGiftAmount = normalize(smallestGiftAmount)) %>%
  mutate(largestGiftAmount = normalize(largestGiftAmount)) %>%
  mutate(averageGiftAmount = normalize(averageGiftAmount)) %>%
  mutate(yearsSinceFirstDonation = normalize(yearsSinceFirstDonation)) %>%
  mutate(monthsSinceLastDonation = normalize(monthsSinceLastDonation))

# we can create dummies for the categorical variables,
# do not included respondedMailing because that is our class variable
# you have to make it a data frame first as well/

library(dummies)
donors<- data.frame(donors)
donors <- 
  dummy.data.frame( 
    data = donors,
    names = c( 
      "inHouseDonor",
      "plannedGivingDonor",
      "sweepstakesDonor",
      "P3Donor",
      "state",
      "urbanicity",
      "socioEconomicStatus",
      "isHomeowner",
      "gender"
    ),
    sep = "_"
  )
colnames(donors)

# Time to create the sample for the test sets and the training sets
set.seed(1234)
sample_index <- sample(nrow(donors), round(nrow(donors)*.75), replace = F)
donors_train <- donors[sample_index,]
donors_test <- donors[-sample_index, ]

# check the class distributions 
round( prop.table( table(select(donors, respondedMailing), exclude = NULL)), 4) * 
  100
round( prop.table( table(select(donors_train, respondedMailing), exclude = NULL)), 
       4) * 100
round( prop.table( table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# there is a class imbalance , use smote to balance the training data
library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing~. , donors_train, perc.over = 100, perc.under =200)
# It is now 50/50 on the training set
# split the class from the rest of the df

donors_train_labels <- as.factor(pull(donors_train))
donors_test_labels <- as.factor(pull(donors_test))

# Convert the training and test datasets to a data frame (without labels)
donors_train <- data.frame(select(donors_train, -respondedMailing)) 
donors_test <- data.frame(select(donors_test, -respondedMailing))


#BUILD THE MODEL
library(class)

donors_p <- 
  knn(
    train = donors_train,
    test = donors_test,
    cl = donors_train_labels,
    k = 5
  )

head(donors_p)


# EVALUATE THE MODEL
donors_pred_table <- table(donors_test_labels, donors_p)
donors_pred_table

# What is the accuracy of our prediction? 
sum( diag(donors_pred_table)) / nrow(donors_test) 

# The model for knn has an accuracy of about .59% not bad or good