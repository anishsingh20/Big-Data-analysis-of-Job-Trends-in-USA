#H-1B Visa Petitions -EDA and Predictive Analytics

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(ggmap)
library(tidyr) #for data reshaping and transformation

visa<-read.csv('C:/Users/hp/Downloads/h1b_kaggle.csv')
#Data frame consists of 3002458 observations

str(visa)


#converting YEAR to a categorical variable

visa %>% mutate(YEAR = as.character(YEAR)) -> visa

head(visa)


summary(na.omit(visa$PREVAILING_WAGE))

table(visa$CASE_STATUS)
#largest number of CERTIFIED VISA cases ,followed by CERTIFIED-WITHDRAWN VISA cases

ggplot(aes(x = CASE_STATUS ), data = na.omit(visa), Ntop=3) + 
  geom_bar(aes(fill = YEAR )) + coord_flip() 
  

