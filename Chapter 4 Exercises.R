setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")
#length
#budget
#type
#actors
#rating

#2.
library(corrplot)
healthnumeric <- 
  health %>%
  select(-c(diabetes,smoker))
healthcor <- cor(healthnumeric)
corrplot.mixed(healthcor)

#3.
#a. it would have a .033 increase holding ll else constant
#b.
.695 + 97*.033
#c.
.695 + 82*.033
#d. it fits well at an 83.5% explaination of variation

#4.
bikes <- read_csv("bikes.csv")
lm.bikes <- lm(data = bikes, realfeel ~ temperature)
summary(lm.bikes)

#5.
#a. .025
#b. 
-1.900439 + .025702*82 + .182456*17
#c.
-1.900439 + .025702*97 + .182456*19
#d. 90% variation explained

#6. 
bikes.lm2 <- lm(data= bikes, realfeel ~ temperature + windspeed + humidity)
summary(bikes.lm2)
