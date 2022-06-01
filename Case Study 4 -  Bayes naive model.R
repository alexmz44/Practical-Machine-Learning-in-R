heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")
glimpse(heart)
summary(heart)

# naive bayes does not require us to normalize the data or even 
# fill in the missing values

# Go straight to splitting the data 
set.seed(1234)
sample_set <- sample(nrow(heart), round(nrow(heart)*.75), replace = F)
heart_train <- heart[sample_set,]
heart_test <- heart[-sample_set,]

round(prop.table(table(select(heart,heartDisease))), 2)


round(prop.table(table(select(heart_train,heartDisease))), 2)

round(prop.table(table(select(heart_test,heartDisease))), 2)
# Build the model
library(e1071)
heart_model <- naiveBayes(heartDisease ~., data = heart_train, laplace = 1)

heart_model

# Now lets see how good the model does against the data of the unseen portion (test)
heart_pred <- predict(heart_model, heart_test, type = "class")
heart_pred_table <- table(heart_test$heartDisease, heart_pred)
heart_pred_table

sum(diag(heart_pred_table)) / nrow(heart_test)
