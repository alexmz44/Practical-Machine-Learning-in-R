setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")
library(tidyverse)
health <- read_csv("health.csv")
glimpse(health)

health <- health %>%
  mutate(diabetes = as.factor(diabetes)) %>%
  mutate(smoker = as.factor(smoker))

summary(health)

#notice mean and median for systolic are really close. This 
#likely indicates a normal distribution

health %>%
  ggplot() +
  geom_histogram(mapping = aes(x=systolic),bins = 40, fill = "red", color = "black")+
  theme_minimal()

#Use a stack of histograms to look at sat dist of the predictor variables


# Choose health data, select everything that is not systolic, keep everything that is only numeric
# i.e. get rid of the factors, gather them all into one table
# graph them all
health %>%
  select(-systolic) %>%
  keep(is.numeric) %>%
  gather()%>%
  ggplot () + geom_histogram( mapping = aes( x = value, fill = key), color = "black") + facet_wrap (~ key, scales = "free")+
  theme_minimal()
# age has a uniform distribution
# everyone else is normal, except fast food which is skewed to the right


#coorelation bewteen the continuous variables
correlation <- cor(health[,c("systolic", "weight", "height", "bmi", "waist", "age", "fastfood")])
correlation
library(corrplot)
corrplot(correlation)

#from correlation matrix, age is the strongest correlated with systolic,
#this will be our first predictor

lm.health <- lm(data= health, systolic ~ age)
summary(lm.health)

# The rse is low and the age is significant but R^2 shows that only a little bit
# of the variation in the response explains our model

lm.health2 <- lm(data=health, systolic  ~.)
summary(lm.health2)

## this model shows that everything is significant except waist
## smoker 1 and fast food are insignificant

#test to see if residual mean = 0 
mean(resid(lm.health2))

library(olsrr)
#slight right skew in the hist
ols_plot_resid_hist(lm.health2)

ols_plot_resid_fit(lm.health2)


#test for autocorr using the durbin watson test
#p value greater than .05 and DW stat of 2.04. Do not reject null
# that no first order correlation exists
library(car)
durbinWatsonTest(lm.health2)

#cooks distance chart
ols_plot_cooksd_chart(lm.health2)


# creating a list of observations that are outliers based on cooks distance
health[1358,]
summary(health)

cooksd <- data.frame(cooks.distance(lm.health2))
colnames(cooksd) <- ("cooks")

health<- 
  health %>%
  add_column(cooksd)


outliers <- subset(health,  cooks >= (4/ (nrow(cooksd)-6-1)),select=ID)
outlier_index <- as.numeric(unlist(outliers))
outlier_index

# comparing the stat summary of ponly outliers
summary(health[outlier_index,])

# now comparing the stat summary of without the out liers
summary(health[-outlier_index,])

# make new datatset without the influential out liers
health2 <- health[-outlier_index,]

# vif test for multicollinearity 
vif <- tibble(ols_vif_tol(lm.health2))

# since weight has the lowest tolerance of waist, bmi, weight and height, we drop everything but that
lm.health3 <- lm(data= health2, systolic ~ weight + age + diabetes)
summary(lm.health3)

#there way be an accelerated relationship between age and blood pressure
#to account for this, it may be useful to consider squarign age

health2 <- health2 %>%
  mutate(age2 =age^2,
         lage = log(age))

# now used the mixed selection formual with the interaction between weight and diabetes
# and age and diabetes 
library(olsrr)
ols_step_both_p(
  model<- lm(
    data = health2,
    systolic ~ weight*diabetes + age * diabetes + age2 * diabetes +
      lage * diabetes
  ),
  pent = .2,
  prem =.01,
  details = T
)
