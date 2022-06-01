#Vehicles (1st Example)
install.packages("RWeka")

vignette(package = 'dplyr')
library(tidyverse)

setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/R learning/R Machine Learning Book Files")

# Read_csv reads it into a tibble, while read.csv reads into a df
# Instance is row , feature is a column
vehicles <- read_csv("vehicles.csv", col_types = "nnnfnfffffnn")
glimpse(vehicles)

head(vehicles)

summary(vehicles)

#dplyr package features
#get one column
select(vehicles, c(class,year))

#OR 

select(vehicles,class,year)

#summarize selected columns
summary(select(vehicles,class,cylinders))

#use table( ) to see more than just the top 6 features
table(select(vehicles, class))

#use pro.table to see the proportional distribution
prop.table(table(select(vehicles, class)))


#we can pipeline to make things logically easier to read
vehicles %>%
  select(class) %>%
  table() %>%
  prop.table
.


vehicles %>% 
  filter(drive == "2-Wheel Drive") %>%
  select(co2emissions) %>%
  summary()
                                      #####Data Visualization
#box plot - comparison of data
library(ggplot2)
library(tidyverse)
vehicles %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = co2emissions, y= class, fill = "red"))+
  labs(title = 'Boxplot of C02 Emissions by Vehicle Class', x = "Class", y = "c02 Emissions")
vehicles %>% ggplot() + geom_boxplot( mapping = aes( x = class, y = co2emissions), fill = "red") + labs( title = "Boxplot of C02 Emissions by Vehicle Class", x = "Class", y = "C02 Emissions")


#scatterplot - relationships of data
vehicles %>%
  ggplot() +
  geom_point( mapping = aes(x = vehicles$citympg , y = vehicles$co2emissions), color = "blue", size = 1)+
  labs(title = "Scatterplot of C02 Emissions vs. City Miles per Gallon",
       x= "City MPG", y = "C02 Emissions")


#histogram - distribution of data
vehicles %>%
  ggplot()+
  geom_histogram(mapping = aes(x = co2emissions),bins = 30, fill = "green", color = "black") +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Frequency")


#stacked bar chart - composition
vehicles %>%
  ggplot()+
  geom_bar(mapping = aes( x = year, fill = drive), color = 'black') +
  labs(title = "Stacked Bar Chart of Drive Type Composition by Year",
       x = "Model Year", y = "Number of cars")+
  coord_flip()



                                                ######data preparation

vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

#median imputation for missing values
#if else syntax(test, yes, no)
vehicles <- vehicles %>%
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm = T), citympg)) %>%
  mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg, na.rm = T),
                             highwaympg))
#mean imputation
vehicles <- vehicles %>%
  mutate(displacement = ifelse(is.na(displacement),mean(displacement, na.rm = T),displacement))

 vehicles %>%
   select(citympg,highwaympg,displacement) %>%
   summary()

                                            #####TRANFORMING DATA
#decimal scaling
vehicles %>%
  select(co2emissions)%>% 
  summary()

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_d = co2emissions / (10^4)) %>%
  summary()

#Z score normalization
vehicles %>% 
  select(co2emissions) %>%
  mutate(co2emissions_z = (co2emissions - mean(co2emissions))/
           sd(co2emissions)) %>%
  summary()

# we can just use scale too
summary(scale(vehicles$co2emissions))

#or
vehicles %>%
  select(co2emissions) %>%
  scale()%>%
  summary()

#Min-Max Normalization
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = 
           ((co2emissions - min(co2emissions))
            /(max(co2emissions)- min(co2emissions)))*(1-0)+0
         ) %>% summary()

#log transformation
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_b = log10(co2emissions)) %>%
  summary()

#Dummy coding

install.packages("dummies")
library(dummies)

vehicles %>%
  select(drive) %>%
  summary()

vehicles2 <- vehicles %>%
  mutate(drive2 = recode(drive, "2-Wheel Drive" = "Front-Wheel Drive"))%>%
  mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive"))%>%
  select(drive,drive2)
vehicles2

summary(vehicles2)
