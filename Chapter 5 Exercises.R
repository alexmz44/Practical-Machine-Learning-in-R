#1.
##a. classification
##b. regression
##c. classification
##d. regression

#2.
#a. Ethnicity
#b. Not known, Single, Widowed, Married

#3.

#4.
#we needed to include the numeric variables
# from the case study 2 
library(corrplot)
income%>%
  keep(is.numeric) %>%
  cor()%>%
  corrplot()

## we are trying to examine outliers - # work hours seems to have a 99 hour 
# work week this could be somewhat odd as that is many hours
income %>%
  keep(is.numeric) %>%
  summary()

## to see this I want to create a histogram
income %>%
  keep(is.numeric) %>%
  gather() %>% ## in geom_hist I made a realization - when you gether, it brings everything
  # into a table whenre there are 2 columns. left column is the variable, right is the value. x is just plotting the values
  ggplot() + geom_histogram( mapping = aes( x = value, fill = key), color = "black") + facet_wrap (~ key, scales = "free")+
  theme_minimal()
  

# we already balanced the datasets  

#c. we need to include the continuous variables 
income_model2 <- income_train %>% 
  glm(formula = income ~ ., family ="binomial")

#d.
summary(income_model2)
#age seems to be highly significant 
# as well as workhours
# and education years

#e. confusion matrix and prediction
income_pred2 <- predict(income_model2, income_test, type= "response")

# remember income on the test set is only 0's and 1's  so it makes sense to make this table 
income_pred2 <- ifelse(income_pred2 >= .50, 1, 0)
matrix <- table(income_test$income, income_pred2)

accuracy <- sum(diag(matrix))/ nrow(income_test)
