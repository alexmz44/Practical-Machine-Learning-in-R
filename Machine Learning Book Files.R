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

vehicles2 <- data.frame(vehicles2)

summary(vehicles2)

#dummy.data.frame(data,names,sep)

vehicles2 <- dummy.data.frame(data = vehicles2, names = "drive2",sep = "_")
head(vehicles2)

#sampling sample(100,20,replace = F)
set.seed(1234)
sample(100,20,replace= F)
sample(100,20,replace= T)

#using sample set vectors (creating training and test sets)
set.seed(1234)
sample_set<- sample(nrow(vehicles),nrow(vehicles)*.75, replace = F)
vehicles_train <-vehicles[sample_set,]
vehicles_train

vehicles_test <- vehicles[-sample_set,]
vehicles_test


#stratified random sampling
install.packages("caTools")
library(caTools)
vehicles %>%
  select(drive) %>%
  table()%>%
  prop.table()

#sample.split(data$feature,SplitRatio = how much of the data should be used to create the data)
set.seed(1234)
sample_set <- sample.split(vehicles$drive, SplitRatio = .01)
vehicles_stratified <- subset(vehicles,sample_set ==TRUE)
vehicles_stratified

#look at the proportional distribution of values for the drive feature in the sample
vehicles_stratified %>%
  select(drive) %>%
  table()%>%
  prop.table()





##EXERCISES
#1. select descriptive statistics of those 4 selected
vehicles %>%
  filter(transmissiontype == "Manual")%>%
  select(drive,make,model,class) %>%
  summary()
#2.Min - Max Normalization
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = 
    ((co2emissions - min(co2emissions))
        /(max(co2emissions)- min(co2emissions))) *(10-1) +1)%>%
  summary()
#3.  In the vehicles dataset, discretize 
#the co2emissions variable using the High 
#value for emission levels at or above 500 
#grams per mile and Low for emission levels 
#below this mark. Using the discretized variable 
#for the strata, generate a stratified random
#sample of percent of the dataset. Show the 
#proportional distribution of values for the 
#discretized variable for the original 
#population and for the sample.

vehicles_new <- vehicles %>%
  mutate(co2emissions = ifelse((vehicles$co2emissions >= 500), "HIGH", "LOW"))
class(vehicles_new)
set.seed(1234)
sample_set <- sample.split(vehicles_new$co2emissions, SplitRatio = .02)
stratified_vehicles <- subset(vehicles_new, sample_set == T)

stratified_vehicles %>%
  select(co2emissions) %>%
  table() %>%
  prop.table

vehicles %>%
  select(co2emissions) %>%
  table() %>%
  prop.table










