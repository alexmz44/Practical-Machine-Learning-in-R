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




