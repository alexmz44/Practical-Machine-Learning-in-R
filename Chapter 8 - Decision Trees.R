# We are trying to predict whther the permit goes through an expedited process or not
library(tidyverse)
setwd("C:/Users/alexa/OneDrive/Documents/R learning/R Machine Learning Book Files/Student/Data")
permits <- read_csv("permits.csv", col_types = "ffffffnnnnfffff")

glimpse(permits)

# we are trying to predict permitcategory

summary(permits)

# we notice that there a lot of NA's but decision tree algos hanfle them
# really well.

# The below problem comes up a lot where NA is an actual value and R does nto read it
# as NA, so I fixed it for valuation

# It is also important to note that there are values that make no sense, such as negative square footage
# fix those like so:

permits <- permits %>%
  mutate(valuation = ifelse(valuation < 1, NA, valuation)) %>%
  mutate(floorArea = ifelse(floorArea <1 , NA, floorArea)) %>%
  mutate(numberUnits = ifelse(numberUnits <1, NA, numberUnits)) %>%
  mutate(stories = ifelse(stories <1, NA, stories))


# we know that the tallest building in LA is only about 73 stories, so those values are invalid too

permits <- permits %>% 
  mutate(stories = ifelse(stories >73, NA, stories))

summary(select(permits, valuation, floorArea, numberUnits, stories))

# decision trees already have great feature selecting capabilities
# lets keep it simple and reduce our dataset to 3 features and our class feature
permits <- permits %>%
  select(permitType,
         permitSubtype,
         initiatingOffice,
         permitCategory
         )
set.seed(1234)
sample_set <- sample(nrow(permits), round(nrow(permits) *.80), replace = F)
permits_train <- permits[sample_set,]
permits_test <- permits[-sample_set,]

#Check the distribution
round( prop.table( table( select( permits, permitCategory))), 2)

round( prop.table( table( select( permits_train, permitCategory))), 2)

round( prop.table( table( select( permits_test, permitCategory))), 2)

# BUILD THE MODEL CART Algorithm

#rpart function _ three arguments rpart(prediction formula, method, training dataset)

library(rpart)

permits_mod <- 
  rpart(
    permitCategory ~., 
    method = "class",
    data = permits_train
  )
# visualize the tree mdodel
library(rpart.plot)

rpart.plot(permits_mod)


# Predict using the model
nrow(permits_test)
permits_pred <- predict(permits_mod, permits_test, type = "class")
permits_table <- table(permits_test$permitCategory, permits_pred)
permits_table

sum(diag(permits_table))/ nrow(permits_test)

# we can improve the model since it is nonparametric


























#####EXERCISES AT THE END OF THE CHAPTER QUESTION #2
summary(permits)
#checking some of the features
table(permits$month)
# removing some features I feel may be useless
permits <- permits %>%
  select(-status,-year,-month)
#the missing values are not a big deal with decision trees, we will skip over them
#SPLIT THE DATA
set.seed(1234)
sample_set <- sample(nrow(permits), round(nrow(permits)*.75), replace = F)

permits_train <- permits[sample_set,]
permits_test <- permits[-sample_set,]

#Check the distributions to see if there is any imbalance
round(prop.table(table(select(permits, permitCategory))),4)*100
round(prop.table(table(select(permits_test, permitCategory))),4)*100
round(prop.table(table(select(permits_train, permitCategory))),4)*100

#LOOKS GOOD
#BUILD THE MODEL
library(rpart)
permits_model <- rpart(
  permitCategory ~.,
  method = "class",
  data = permits_train
)

