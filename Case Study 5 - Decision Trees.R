setwd("C:/Users/alexa/OneDrive/Documents/R learning/R Machine Learning Book Files")

# Import the data
income <- read_csv("income.csv", col_types = "nffnfffffnff")
glimpse(income)

#income feature is the one we are trying to predict (income <= 50k or >50k)

summary(income)

#There are some values denoted as "?", but decision trees handle missing data extremely well
#so we will leave it as is

#Split into training and test sets
set.seed(1234)
sample_set<- sample(nrow(income), round(nrow(income) *.75), replace = F)
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

# check the distributions of the data. Train and test must be similar to the original set
# also look for class imbalance issues

round(prop.table(table(select(income, income), exclude = NULL)),4) * 100
round(prop.table(table(select(income_train, income), exclude = NULL)),4) * 100
round(prop.table(table(select(income_test, income), exclude = NULL)),4) * 100

#Class imbalance issue visible 
#use SMOTE to normalize

library(DMwR)
set.seed(1234)
income_train <- SMOTE(income~ ., data.frame(income_train),perc.over = 100, perc.under = 200)

round(prop.table(table(select(income_train, income), exclude = NULL)),4) * 100

#BUILDING THE MODEL

library(rpart)

income_model <- rpart(income ~., 
                      method = "class", 
                      data = income_train)

#evaluating the model
library(rpart.plot)
rpart.plot(income_model)

#Try to see how well the model does against our test set
income_predict <- predict(income_model, income_test, type = "class")
income_predict_table <- table(income_test$income, income_predict)

income_predict_table

#Calculating accuracy 
sum(diag(income_predict_table)) / nrow(income_test)

#about 75% accuracy