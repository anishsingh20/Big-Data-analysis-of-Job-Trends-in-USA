#Random sampling and forming smaller data set 
small_visa<-sample_n(visa,5000)

write.csv(small_visa , file = "smallvisa.csv", row.names = FALSE,na="")


#important functions for simplified plotting 
job_filter <- function(df,input_vec) {
  # Function to filter only the rows from dataframe 
  # with Job titles provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # A new column JOB_INPUT_CLASS is created to identify the Job Type
  # If multiple job type inputs match with a single row in the dataframe df, the 
  # output contains them in different rows each with distinct JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(input_vec) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      #regexpr used for pattern matching and replacement
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
  }
  return(unique(new_df))
}


find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics that I will be using in my data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}

  find_top <- function(df,x_feature,metric, Ntop = 3) {
    # Function to find the top values in x_feature based on metric value
    # Inputs:
    # df            : filtered dataframe from job_type, location, employer and year range inputs
    # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
    # metric        : metric for data comparison 
    # Output        : list of top values in x_feature based on metric
    arrange_criteria <- interp(~ desc(x), x = as.name(metric))
    
    df %>% 
      group_by_(x_feature) %>% 
      mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
      # Metrics that I will be using in my data analysis
      summarise(TotalApps = n(),
                Wage = median(PREVAILING_WAGE), 
                CertiApps = sum(certified),
                Share = CertiApps/850) %>%
      arrange_(arrange_criteria) -> top_df
    
    top_len <- min(dim(top_df)[1],Ntop)
    
    return(top_df[1:top_len,1])
  }

plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
           group_by_(.dots=c(x_feature,fill_feature)) %>% 
           mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
           # metrics I will be using in my data analysis   
           summarise(TotalApps = n(),
                     CertiApps = sum(certified), 
                     Wage = median(PREVAILING_WAGE),
                     Share = CertiApps/850))
}

  plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
    # Function to plot output
    # Inputs:
    # df            : dataframe output of plot_input()
    # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
    # fill_feature  : additional level of classification; for e.g., Year
    # metric        : metric for data comparison 
    # xlabb         : x label
    # ylabb         : y label
    # Output        : ggplot object
    
    # Prevents numbers on plot transforming into scientific notation
    options(scipen = 999)
    
    g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
      geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
      coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
    
    return(g)
  }


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}

  plot_input <- function(df, x_feature, fill_feature = "YEAR", metric = "TotalApps",filter = FALSE, ...) {
    # Function to transform the filtered dataframe to one with computed metrics
    # Inputs:
    # df            : filtered dataframe from job_type, location, employer and year range inputs
    # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
    # fill_feature  : additional level of classification; for e.g., Year
    # metric        : metric for data comparison 
    # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
    # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
    
    #Finding out the top across the entire range independent of the fill_feature e.g. Year
    top_x <- unlist(find_top(df,x_feature,metric, ...))
    
    # lazyeval package interp () generates expression that interprets x_feature and metric arguments
    # this is fed into filter_ and arrange_ accordingly
    # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
    
    filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
    arrange_criteria <- interp(~ desc(x), x = as.name(metric))
    
    if(filter == TRUE) {
      df %>%
        filter_(filter_criteria) -> df
    }
    
    #Grouping by not just x_feature but also fill_feature
    return(df %>% 
             group_by_(.dots=c(x_feature,fill_feature)) %>% 
             mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
             # metrics I will be using in my data analysis   
             summarise(TotalApps = n(),
                       CertiApps = sum(certified), 
                       Wage = median(PREVAILING_WAGE),
                       Share = CertiApps/850))
  }

  
  plot_output <- function(df, x_feature,fill_feature = "YEAR",metric, xlabb,ylabb) {  
    # Function to plot output
    # Inputs:
    # df            : dataframe output of plot_input()
    # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
    # fill_feature  : additional level of classification; for e.g., Year
    # metric        : metric for data comparison 
    # xlabb         : x label
    # ylabb         : y label
    # Output        : ggplot object
    
    # Prevents numbers on plot transforming into scientific notation
    options(scipen = 999)
    
    g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
      geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
      coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
    
    return(g)
  }


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}




#H-1B Visa Petitions -EDA and Predictive Analytics

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(ggmap)
library(tidyr) #for data reshaping and transformation
library(ggrepel)
library(lazyeval)

visa<-read.csv('C:/Users/hp/Downloads/h1b_kaggle.csv')
#Data frame consists of 3002458 observations

str(visa)


#converting YEAR to a categorical variable

visa %>% mutate(YEAR = as.character(YEAR)) -> visa

head(visa)


summary(na.omit(visa$PREVAILING_WAGE))

table(visa$CASE_STATUS)
#largest number of CERTIFIED VISA cases ,followed by CERTIFIED-WITHDRAWN VISA cases

table(visa$FULL_TIME_POSITION)
# N       Y 
#426332 2576111
#More Number of immigrants were working as Full time positions

visa %>% filter(JOB_TITLE == 'SOFTWARE ENGINEER') ->Softvisas

#Grouping by SOC_NAME and summarizing by sallaries in each category for 
#certified CASE STATUS

visabySOC<-na.omit(visa) %>%  group_by(YEAR,SOC_NAME)  %>% filter(CASE_STATUS == 'CERTIFIED' ) %>%
summarise(mean_sallary = mean(PREVAILING_WAGE), median_sallary = median(PREVAILING_WAGE) ,total = n())

#For CHIEF EXECUTIVES 
filter(visabySOC,SOC_NAME=='CHIEF EXECUTIVES')

#2015 CHIEF EXECUTIVES     170847.7         167530   488
#2016 CHIEF EXECUTIVES     167041.8         174408   433


#FOR WEB DEVELOPERS
filter(visabySOC,SOC_NAME=='WEB DEVELOPERS')
#avg yeary sallary increased in 2016 but less candidates


#FOR DATABASE ADMINSTRATORS
filter(visabySOC,SOC_NAME=='DATABASE ADMINSTRATORS')


#PLOTTING
input <- plot_input(visa, "EMPLOYER_NAME", "YEAR", "TotalApps",filter = TRUE, Ntop = 10)

g1 <- plot_output(input, 'EMPLOYER_NAME','YEAR','TotalApps', 'EMPLOYER','TOTAL NO. of APPLICATIONS') + theme(axis.title = element_text(size = rel(1.5)),
                                                                                                            axis.text.y = element_text(size=rel(0.5)))

#For Certified/Approved  H1B visa applications

g2 <- plot_output(input, 'EMPLOYER_NAME','YEAR','Share', 'EMPLOYER','% SHARE of APPROVED H1B VISA PETITIONS') +  theme(axis.title = element_text(size = rel(1.5)),
                                                                                       axis.text.y = element_text(size=rel(0.5)))
g2



#TOP 5 EMPLOYERS WITH MOST PETITIONS



#finding out the top 5 EMPLOYERS  with most Petitions]

top_employers <- unlist(find_top(visa,"EMPLOYER_NAME","TotalApps",Ntop = 5))


#finding most common job titles for the top 5 employers along with mean sallaries
visa %>%
  filter(EMPLOYER_NAME %in% top_employers & FULL_TIME_POSITION == 'Y') %>%
  select(JOB_TITLE,EMPLOYER_NAME,CASE_STATUS,PREVAILING_WAGE) -> common_jobs


#plot for top 15 job titles of the top 5 employers with most petitions

g3<-ggplot(common_jobs[1:15,], aes(x=reorder(JOB_TITLE,COUNT),y=COUNT)) +
  geom_bar(stat = "identity",fill='red') + coord_flip() +
  xlab("JOB TITLE") + ylab("TOTAL NO. OF APPLICATIONS") + get_theme() 
g3


ggplot(aes(x =JOB_TITLE,y = mean_sallary),data = common_jobs[1:15,]) + 
  geom_bar(stat='identity', fill='blue') + coord_flip() + 
  scale_y_continuous(limits=c(0,10000000))

common_jobs %>% filter(JOB_TITLE=='ANALYST')

common_jobs %>% filter(JOB_TITLE=='BUSINESS ANALYST')



#finding the SOC_Name of those top 5 Employers- ROLLUp Operations-Summarizations
visa %>% filter(PREVAILING_WAGE >= 1000000 & EMPLOYER_NAME %in% top_employers & FULL_TIME_POSITION == 'Y') %>%
  group_by(SOC_NAME) %>%
    summarise(count = n()) %>%
      arrange(desc(count)) -> common_SOC_names



#Finding the most common Worksites of the Foreign Worker's intented area of working 
# for top 5 Employers and a wage > = 100000 USD
visa %>% filter( EMPLOYER_NAME %in% top_employers & FULL_TIME_POSITION == 'Y') %>%
  group_by(WORKSITE) %>%
  summarise(count = n(),MEAN_WAGES = mean(PREVAILING_WAGE)) %>%
  arrange(desc(count)) -> common_Worksites



#PLOT OF MOST COMMON WORKSITES AND COUNT OF APPLICATIONS FOR THAT AREA
ggplot(aes(x = reorder(WORKSITE,count),  y = count), data = common_Worksites[1:15,]) + 
  geom_bar(stat='identity', fill = 'yellow',color = 'black') + 
  xlab('Common_worksites') + 
  ylab('No of Applications by Foreign Nationals in the area') + 
  coord_flip()




ggplot(aes(x = CASE_STATUS ), data = na.omit(visa)) + 
  geom_bar(aes(fill = YEAR )) + coord_flip() 
  





#WAGE DISTRIBUTIONS - for those 5 Top Employers with most H1B VISA applicatins petitions 


visa %>%
  filter(EMPLOYER_NAME %in% top_employers, JOB_TITLE %in% unlist(common_jobs$JOB_TITLE[1:20])) %>%
  group_by(JOB_TITLE) -> job_wages_visa
#grouped by JOB TITLE

job_wages_visa %>% mutate(MONTHLY_WAGE = PREVAILING_WAGE/12) -> job_wages_visa 


#boxplot of Wages Vs the Job_title with highest count of applications
ggplot(aes(x  = reorder(JOB_TITLE,MONTHLY_WAGE,median), y = MONTHLY_WAGE),data = na.omit(job_wages_visa)) +
  geom_boxplot(fill='red') + coord_flip(ylim = c(0,10000)) + 
  xlab("JOB TITLE") + ylab("WAGE Monthly(USD)") +
  get_theme()





#PHASE3
#WAGES OFFERED BY COMPANIES LIKE GOOGLE,AMAZON,FACEBOOK and their JOB TITLES:



split_first <- function(word, split) {
  # Function to obtain first value in a  strsplit
  # Inputs:
  # word      : word to be split
  # split     : split parameter to be passed to strsplit
  return(strsplit(word,split= split))
}


visa$EMPLOYER_COMPACT = sapply(visa$EMPLOYER_NAME,split_first,split = " ")

#list of Jobs
job_list <- c("Programmer","Computer","Software","Systems","Developer")

#list of Employer names
employer_list <- toupper(c("IBM","Infosys","Wipro","Tata","Deloitte","Amazon","Google","Microsoft","Facebook"))


job_filter(visa,job_list) %>%
  filter(EMPLOYER_COMPACT %in% employer_list) %>%
  group_by(EMPLOYER_COMPACT) -> employer_df






#DATA SCIENCETIST JOB ANALYSIS

#Creating a list of data science Related Jobs
job_list <- c("Data Scientist","Data Engineer","Machine Learning")


#Employers which send applications for Data Science positions
#filtering ie querying only jobs in job_list 
#DATA SCIECNE DF
job_filter(visa,job_list) %>%
  group_by(EMPLOYER_NAME) %>%

  select(EMPLOYER_NAME,SOC_NAME,WORKSITE,JOB_TITLE,PREVAILING_WAGE) %>%
    
      arrange(desc(PREVAILING_WAGE))->employ_data

visa %>%
  mutate(SOC_NAME = toupper(SOC_NAME)) -> visa





#DATA SCIENCETIST POSITIONS in which DEPARTMENT?
job_filter(visa,job_list) %>%
  #filter(CASE_STATUS = 'CERTIFIED') 
  filter(!is.na(SOC_NAME))  %>%
  group_by(SOC_NAME) %>%
    summarise(total_H1B_Applications= n() , Median_wages = median(PREVAILING_WAGE)) %>%
      filter(total_H1B_Applications > 10 ) %>%
          arrange(desc(total_H1B_Applications))->data_science_df1


data_science_soc_df <- plot_input(job_filter(visa,job_list),
                                  "SOC_NAME",
                                  "YEAR",
                                  filter = TRUE,
                                  Ntop = 10)

plot_output(data_science_soc_df, "SOC_NAME","YEAR", "TotalApps", "INDUSTRY", "NO. OF APPLICATIONS")
#highest applications for STATISTICIANS AND SOFTWARE DEVELOPERS

  





#COMPANIES WHICH ISSUE MOST DATA SCIENCE POSITIONS H1B-VISA APPLICATIONS
job_filter(visa,job_list) %>%
  select(EMPLOYER_NAME,JOB_TITLE,PREVAILING_WAGE,SOC_NAME) %>%
  #filter(CASE_STATUS = 'CERTIFIED') 
  
  filter(!is.na(SOC_NAME))  %>%
  group_by(EMPLOYER_NAME) %>%
  
  summarise(total_H1B_Applications= n() , Median_wages = median(PREVAILING_WAGE)) %>%
  filter(total_H1B_Applications > 10 ) %>%
  arrange(desc(total_H1B_Applications))->employer

filter(employer , EMPLOYER_NAME == 'GOOGLE INC.')
#only 13 applications send over 2011- 2016

ggplot(aes(x = reorder(EMPLOYER_NAME,total_H1B_Applications), y =total_H1B_Applications ),data = employer[1:20,]) + 
  geom_bar(stat='identity',fill='blue',color='black') + 
  get_theme() + 
  xlab(" Top 20 Employers of Data Science Positions") + 
  ylab("No of H1B Applications issued") + 
  coord_flip() 
  
  
ggplot(aes(x = reorder(EMPLOYER_NAME,Median_wages), y = Median_wages ),data = employer[1:20,]) + 
  geom_col(fill='black') + 
  get_theme() + 
  xlab(" Top 20 Employers of Data Science Positions") + 
  ylab("WAGES(USD)") + 
  coord_flip() 






#Generating Data Science data 
data_science_df <- plot_input(job_filter(visa,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

h <- plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")

h

#WAGES PLOT
ggplot( aes( x = reorder(JOB_INPUT_CLASS,PREVAILING_WAGE,median) , y = PREVAILING_WAGE ), data = job_filter(visa,job_list)) + 
  geom_boxplot(aes(fill=YEAR)) + 
  get_theme() + 
  xlab('JOB-TITLE') + 
  ylab('Wages') +
  coord_cartesian(ylim = c(25000,150000))
#Highest Sallaries for Machine Learning Engineers








#creating a list of jobs with Title such as business analysts and data analysts
job_list1<-c("Business Analyst","Data Analyst",'Statistician')

analyst_df <- plot_input(job_filter(visa,job_list1),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

p1 <- plot_output(analyst_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")

#highest applications for Business analyst  that too in 2016
p1

#Wages of all Job Titles
ggplot(aes(x = reorder(JOB_INPUT_CLASS,PREVAILING_WAGE,median) , y = PREVAILING_WAGE ),data = job_filter(visa,job_list1)) + 
  geom_boxplot(aes(fill=YEAR)) + 
  get_theme() + 
  xlab("JOB-TITLE") + 
  ylab("WAGES(USD)") +
  coord_cartesian(ylim=c(25000,100000))

#highest sallary for Statisticians
#Interesting trend is that the sallaries of All Data Analysts , Business Analysts
# and Statisticians have all increased a bit over time from 2011-2016


#Data Science Sallaries as per the field 

data_science_soc_df <- plot_input(job_filter(h1b_df,job_list),
                                  "SOC_NAME",
                                  "YEAR",
                                  filter = TRUE,
                                  Ntop = 10)


plot_output(data_science_soc_df, "SOC_NAME","YEAR", "Wage", "INDUSTRY", "WAGE (USD)")



#Highest Sallaries for Data Sciencetists in field of 
# MATHAMATICIANS , REASEARCH SCIENCETISTS and SOFTWARE DEVELOPERS




#lOCATIONWISE ANALYSIS -GENERATING A MAP WITH MOST LIKABLE JOB LOCATIONS HAVING 
#HIGHEST APPLICATIONS FOR DATASCIENCE ,DATA ENGINEER AND MACHINE LEARNING APPLICATIONS


#MAP GENERATING FUNCTION which  returns  a USA MAP

library(ggrepel)


map_gen <- function(df,metric,USA,...) {
  # Function to generate map plot for given metric in df 
  # This is laid on top of USA map
  # Inputs:
  # df      : dataframe with metrics, lat, lon, WORKSITE columns
  # metric  : metric for data comparison 
  # USA     : dataframe for US maps with lat, long columns. map_data(map = "usa") from ggplot2
  # Output  : ggplot object
  
  
  # Creating Map Dataframe
  df %>%
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    group_by(WORKSITE,lat,lon) %>%
    summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)) -> map_df
  
  # # Lat-Long Limits
  # df %>%
  #   summarise(lat_min = min(lat,na.rm=TRUE),
  #             lat_max = max(lat,na.rm=TRUE),
  #             long_min = min(lon,na.rm=TRUE),
  #             long_max = max(lon,na.rm=TRUE)) -> geo_coord
  
  # Finding top Locations for metric
  top_locations <- unlist(find_top(df,"WORKSITE",metric, ...))
  
  # First layer    : USA Map
  # Second layer   : geom_point() with point alpha and size varying with metric
  # Third layer    : points mapping to top locations using ggrepel package
  g <- ggplot(USA, aes(x=long, y=lat)) + 
    geom_polygon() + xlab("Longitude (deg)") + ylab("Latitude(deg)") + 
    geom_point(data=map_df, aes_string(x="lon", y="lat", label = "WORKSITE", alpha = metric, size = metric), color="yellow") + 
    geom_label_repel(data=map_df %>% filter(WORKSITE %in% top_locations),aes_string(x="lon", y="lat",label = "WORKSITE"),
                     fontface = 'bold', color = 'black',
                     box.padding = unit(0.0, "lines"),
                     point.padding = unit(1.0, "lines"),
                     segment.color = 'grey50',
                     force = 3) +
    # Zoom into the specific location input
    #coord_map(ylim = c(max(geo_coord$lat_min - 5,23), min(geo_coord$lat_max - 5,50)),xlim=c(max(geo_coord$long_min - 5,-130),min(geo_coord$long_max + 5,-65))) +
    # Using the whole USA map
    coord_map(ylim = c(23,50),xlim=c(-130,-65)) +
    get_theme()
  
  #returns the USA map with jobs
  return(g)
}




USA = map_data(map = "usa")

g <- map_gen(job_filter(visa,job_list),"TotalApps",USA,Ntop = 3)

g






prop.table(table(common_jobs$CASE_STATUS))

require(neuralnet)

?neuralnet
train = subset(common_jobs,CASE_STATUS==c('CERTIFIED','WITHDRAWN'))
train$CASE_STATUS<-ifelse(train$CASE_STATUS=='CERTIFIED',1,0)
set.seed(1212)

mod1<-neuralnet(CASE_STATUS ~  PREVAILING_WAGE ,data=train[sample(nrow(train),2000),],
                hidden=2 ,
                    err.fct = 'ce',linear.output = FALSE)
summary(mod1)
plot(mod1)
mod1$weights
mod1$net.result
mod1

test<-train[sample(nrow(train),2000),]
predictions<-data.frame(compute(mod1,test[,4]))



  
