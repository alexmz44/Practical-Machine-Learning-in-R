setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")
library(tidyverse)
bikes <- read_csv("bikes.csv",col_types = "Dffffddddd")
View(bikes)
cov(bikes$humidity, bikes$rentals)
sd(bikes$humidity)
sd(bikes$rentals)

#pearsons correlation

cor(bikes$humidity, bikes$rentals)

cor(bikes$windspeed, bikes$rentals)
cor(bikes$temperature, bikes$rentals)

#corrplot
library(corrplot)
bikenumeric <- bikes %>% 
  select(-date)
bikenumeric
cor(bikenumeric)

bike_correlations <- cor(bikenumeric)
corrplot(bike_correlations)
corrplot(bike_correlations, type = "upper")
corrplot.mixed(bike_correlations)


#finding the estimates
B1 <- cov(bikes$temperature, bikes$rentals)/ var(bikes$temperature)
B1

B0 <- mean(bikes$rentals - B1 * mean(bikes$temperature))
B0

bike_model <- lm(data= bikes,rentals~temperature)
bike_model

summary(bike_model)
#SLR
plot(data= bikes, rentals~ temperature)
abline(bike_model)

#MLR
library(stats)
bikes_model2 <- lm(data = bikes , rentals ~ humidity+ temperature+ windspeed)
summary(bikes_model2)

# Testing the assumptions 
        #1 the residuals have mean = 0
mean(bikes_model2$residuals)

        #2 Normaility of residuals
install.packages("olsrr")
library(olsrr)
ols_plot_resid_hist(bikes_model2)
        #3. Homoskedasticity
ols_plot_resid_fit(bikes_model2)
        #4. Autocorrelation 0-2 positive autocorr , = no autocorr, 2 to 4 indicating negative auto
library(car)
durbinWatsonTest(bikes_model2)

#BP Test
library(lmtest)
bptest(bikes_mod2)

#Influential point analysis - cook's distance
library(olsrr)
ols_plot_cooksd_bar(bikes_model2)
cooks_outliers <- ols_plot_cooksd_chart(bikes_model2)$outliers

cooks_outliers
cooks.distance(bikes_model2)

library(dplyr)
arrange(cooks_outliers, desc(cooks_distance))

#look at what is happening in value 69
bikes[69,c("rentals","humidity","windspeed","temperature")]
summary(bikes[-69,c("rentals","humidity","windspeed","temperature")])
#Without obs 69, humidity's minimum is not as low as 0
#windspeed exceeds third quartile

outlier_index <- as.numeric(unlist(cooks_outliers[,"observation"]))
summary(bikes[outlier_index, c("rentals","humidity","windspeed","temperature")])
summary(bikes[-outlier_index, c (" rentals"," humidity"," windspeed"," temperature")])
bikes

#creating the data without the outliers
bikes2 <- bikes[-outlier_index,]

#variance inflation factor (VIF) if >5 and <.2 , multicollinearity is present
ols_vif_tol(bikes_model2)


#polynomial regression equation
bikes <- bikes %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>%
  mutate (temperature2 = temperature^2)
bikes_model3 <- 
  lm(data = bikes,
     rentals ~ humidity + windspeed + temperature +
       humidity2 + windspeed2 + temperature2)
summary(bikes_model3)

#Transforming categorical variables
summary(bikes2[,c("season", "holiday", "weekday", "weather")])

#transforming the values
library(dplyr)
bikes <- bikes %>%
  mutate( season = revalue( season, c(" 1" =" Winter", "2" =" Spring", "3" =" Summer", "4" =" Fall"))) %>% 
  mutate( holiday = revalue( holiday, c(" 0" =" No", "1" =" Yes"))) %>% 
  mutate( weekday = revalue( weekday, c(" 0" =" Sunday", "1" =" Monday", "2" =" Tuesday", "3" =" Wednesday", "4" =" Thursday", "5" =" Friday", "6" =" Saturday"))) %>% 
  mutate( weather = revalue( weather, c(" 1" =" Clear", "2" =" Light precipitation", "3" =" Heavy precipitation")))


## wroking with dates
install.packages("lubridate")
library(lubridate)
bikes <- bikes %>% mutate (day = as.numeric( date-min( date))) %>% mutate ( month = as.factor( month( date))) %>% mutate ( year = as.factor( year( date))) %>% select (-date)



#working with mixed selection process
ols_step_both_p( model = 
                   lm( data = bikes2,
                       rentals ~ humidity + weekday + holiday + temperature + humidity2 + temperature2 + season + windspeed * weather + realfeel + day + month + year ), 
                 pent = 0.2, prem = 0.01, details = FALSE )


########################################################################
#Chapter 5 Logistic regression

library("tidyverse")
donors <- read_csv("donors.csv", col_types = "nnffnnnnnnnnffffffffff")


glimpse(donors)
donors %>% keep( is.factor) %>% summary()

donors <- donors %>% mutate( incomeRating = as.character( incomeRating))%>% 
  mutate(incomeRating = as.factor( ifelse (incomeRating == 'NA', 'UNK', incomeRating)))

donors%>%
  select(incomeRating) %>%
  table() %>%
  prop.table()

#replace all NA's with UNK
donors <- donors %>% mutate(wealthRating = as.character( wealthRating))%>% 
  mutate(wealthRating = as.factor( ifelse (wealthRating == 'NA', 'UNK', wealthRating)))%>% 
  mutate(urbanicity = as.character(urbanicity))%>% 
  mutate(urbanicity = as.factor( ifelse (urbanicity == 'NA', 'UNK', urbanicity))) %>%
  mutate(socioEconomicStatus= as.character(socioEconomicStatus))%>% 
  mutate(socioEconomicStatus = as.factor( ifelse (socioEconomicStatus == 'NA', 'UNK', socioEconomicStatus))) %>%
  mutate(isHomeowner= as.character(isHomeowner))%>% 
  mutate(isHomeowner = as.factor( ifelse (isHomeowner == 'NA', 'UNK', isHomeowner)))%>%
  mutate(gender= as.character(gender))%>% 
  mutate(gender = as.factor( ifelse (gender == 'NA', 'UNK', gender)))

#check to see if it is corrrect
donors %>%
  keep(is.factor) %>%
  summary()

#numeric values now

donors %>%
  keep(is.numeric) %>%
  summary()

# we use the mean of age to replace the NA's but we group by gender
# comparing summary stats of age before and after imputation
donorstestage <- donors %>%
  group_by(gender) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = T), age )) %>%
  ungroup()

donorstestage %>%
  select(age) %>%
  summary()

donors %>%
  select(age) %>%
  summary()

donors <- donorstestage


## comparing summary stats of number of children before and after imputation
donorstestchildren <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren),
                                 median(numberChildren, na.rm = T),
                                 numberChildren))


donorstestchildren %>% select(numberChildren) %>% summary()

donors %>% select(numberChildren) %>% summary()

donors <- donorstestchildren

# we see big outliers in variables such as mailorderpurchases, total giving mount,
# number of gifts, smallest gift amount, largest gift amount, average gift amount
donors %>% 
  keep(is.numeric) %>%
  summary()

donors %>% 
  select(-respondedMailing,-monthsSinceLastDonation, -yearsSinceFirstDonation,-age)%>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key),bins = 30, color = "black") +
  facet_wrap(~ key, scales = "free") + 
  theme_minimal()


# we can get rid of outliers by using the method that anything 1.5s larger or smaller than the]
#IQR is labeled as an outlier 
# we do the larger side because the data is right skewed

library(stats)
donors <- donors %>%
  mutate(max1 = quantile(mailOrderPurchases, .75) + (1.5 * IQR(mailOrderPurchases))) %>%
  mutate(max2 = quantile(totalGivingAmount, .75) + (1.5 *IQR(totalGivingAmount))) %>%
  mutate(max3 = quantile(numberGifts, .75) + (1.5 *IQR(numberGifts))) %>%
  mutate(max4 = quantile(smallestGiftAmount, .75) + (1.5 *IQR(smallestGiftAmount))) %>%
  mutate(max5 = quantile(largestGiftAmount, .75) + (1.5 *IQR(largestGiftAmount))) %>%
  mutate(max6 = quantile(averageGiftAmount, .75) + (1.5 *IQR(averageGiftAmount))) %>%
    filter(mailOrderPurchases <= max1) %>%
    filter(totalGivingAmount <= max2)%>%
    filter(numberGifts <= max3) %>%
    filter(smallestGiftAmount <= max4) %>%
    filter(largestGiftAmount <= max5) %>%
    filter(averageGiftAmount <= max6) %>%
      select(-max1,-max2,-max3,-max4,-max5,-max6)
  
#check graphs again - more balanced distribution
donors %>% 
  select(-respondedMailing,-monthsSinceLastDonation, -yearsSinceFirstDonation,- age,-numberChildren)%>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key),bins = 30, color = "black") +
  facet_wrap(~ key, scales = "free") + 
  theme_minimal()

# now its time to split the data and sample it as well
set.seed(1234)
sample_set <- sample(nrow(donors), round(nrow(donors) *.75), replace = F)
donors_train <- donors[sample_set,]
donors_test <- donors[-sample_set,]

round(prop.table(table(select(donors, respondedMailing), exclude = NULL)),4) *100  

round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) *100

round(prop.table(table(select(donors_test,  respondedMailing), exclude = NULL)), 4) *100

#Class imbalance (synthetic minority oversampling technique)
# we balance the training data prior to modelling 
install.packages("DMwR")
install.packages(c("zoo","xts","quantmod"))

remotes::install_github("cran/DMwR")
library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~., data.frame(donors_train),perc.over = 100, perc.under =200)

#look at the new class distributions
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)),4) * 100

round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100

round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# now the training set is balanced, lets change F/T to 0/1

donors <- donors %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE,1,0)))
donors_train <- donors_train%>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE,1,0)))
donors_test<- donors_test %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE,1,0)))

#one of the most popular R functions to build a binomial logistic regression Model is glm()
#generalized linear Model
library(stats)


#glm(training data, family =  type of regression model we intend to build, formula)

donors.model <- glm(donors_train, family = binomial,formula = respondedMailing ~.)
summary(donors.model)

# the coefficients can be confusing bc they represent log odds
# to convert to something that can be understandable
exp(coef(donors.model)["averageGiftAmount"])
exp(coef(donors.model)["incomeRating2"])

# predicting the response for out of sample observations
#predict(model created, test data, type of prediction)
donors.pred1 <- predict(donors.model, donors_test, type = "response")
# we get an errors but we can remove these observations from the test data

filter(donors_test, state == "RI"| state == "NH")

donors_test <- donors_test %>%
  filter(state!="RI" & state!="NH")

donors.pred1 <- predict(donors.model, donors_test, type = "response")
head(donors.pred1)

donrs_pred1 <- ifelse(donors.pred1>= .5, 1, 0)
head(donrs_pred1)

#create confusion matrix to compare predicted results with actual results
donors_pred1_table <- table(donors_test$respondedMailing, donrs_pred1)
donors_pred1_table


sum(diag(donors_pred1_table))/ nrow(donors_test)

library(stats)
library(corrplot)

donors %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

##variance inflation facotr check vif larger than 5 indicates 
library(car)
vif(donors.model)

# new model
donors_mod2 <- glm(data = donors_train, 
                   family = binomial,
                   formula = respondedMailing ~ incomeRating + wealthRating +
                     mailOrderPurchases + numberGifts + yearsSinceFirstDonation + 
                     monthsSinceLastDonation + sweepstakesDonor + state + urbanicity + 
                     socioEconomicStatus + isHomeowner + gender)

summary(donors_mod2)         

##no vif over 5 so are we good , the model did give up some AIC
## this is okay because this is a good cost tradeoff for the long run
vif(donors_mod2)

donors_pred2 <- predict(donors_mod2, donors_test, type = 'response')
head(donors_pred2)

#making a cut off value to make sure 
install.packages("InformationValue")
library(InformationValue)


#optimalCutoff(actual values for the for the respsonse, predicted values for the responsespecifies that we want the optimal cutoff to be based on the value that maximizes the proportions of correctly predicted observations for both 1 and 0.

ideal_cutoff <- optimalCutoff(actuals = donors_test$respondedMailing,
                              predictedScores = donors_pred2,
                              optimiseFor = "Both")
ideal_cutoff

donors_pred2 <- ifelse(donors_pred2 >= ideal_cutoff, 1, 0)
donors_pred2_table <- table(donors_test$respondedMailing, donors_pred2)

# lets look at the table
donors_pred2_table

# lets test the classification accuracy
sum(diag(donors_pred2_table)) / nrow(donors_test)

