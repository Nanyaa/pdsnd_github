
#Begin by reading all the datasets. The datasets are all in csv formats. The read.csv function is used to do this.
new_york = read.csv('new_york_city.csv')
washington = read.csv('washington.csv')
chicago = read.csv('chicago.csv')

#Exploring the New York dataset by viewing the first few rows and columns
head(new_york)

#Exploring the Washington dataset by viewing the first few rows and columns
head(washington)

#Exploring the first few rows and columns of the Chicago dataset
head(chicago)

#Importing the dplyr library
library(dplyr)

#Importing the readr library
library(readr)

#Importing the ggplot library
library(ggplot2)

#Importing the lubridate library
library(lubridate)

#Summary of the New York dataset
summary(new_york)

#Summary of the Washington dataset
summary(washington)

#Summary of the Chicago dataset
summary(chicago)

#Extracting year, month, day, hour, minute, second from the start and end times
new_york$`Start.Time` <- ymd_hms(new_york$`Start.Time`)
new_york$`End.Time` <- ymd_hms(new_york$`End.Time`)

#Extracting year, month, day, hour, minute, second from the start and end times
washington$`Start.Time` <- ymd_hms(washington$`Start.Time`)
washington$`End.Time` <- ymd_hms(washington$`End.Time`)

#Extracting year, month, day, hour, minute, second from the start and end times
chicago$`Start.Time` <- ymd_hms(chicago$`Start.Time`)
chicago$`End.Time` <- ymd_hms(chicago$`End.Time`)

#Function to extract hour, month and days
extract <- function(data){
  data$hour <- hour(data$`Start.Time`)
  data$month <- month(data$`Start.Time`)
  data$day <- weekdays(data$`Start.Time`)
  return(data)}

#Extracting each dataset
new_york <- extract(new_york)
washington <- extract(washington)
chicago <- extract(chicago)

#This question looks to explore the most popular month of travel in New York City
ggplot(new_york, aes(month)) + geom_histogram(bins = 6, color = I('grey'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,50000, 1000))+
  xlab("Month Number") +  ylab("Count Value")+ ggtitle("Most Popular Month in New York City")

#This question looks to explore the most common month of travel in Washington
ggplot(washington, aes(month)) + geom_histogram(bins = 6, color = I('grey'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,50000, 1000))+
  xlab("Month Number") +  ylab("Count Value")+ ggtitle("Most Popular Months in Washington")

#This question looks to explore the most common month of travel in Chicago
ggplot(chicago, aes(month)) + geom_histogram(bins = 6, color = I('grey'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,50000, 200))+
  xlab("Month Number") +  ylab("Count Value")+ ggtitle("Most Popular Months in Chicago")

#Count of travellers by gender in New York
new_york %>% 
  group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity')+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in new york")

#Counts of travellers by gender in Chicago
chicago %>% 
  group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity')+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in chicago")

#Summary of the New York trip duration column
summary(new_york$`Trip.Duration`)

#The result shows that the minimum time is 61.0s, the median time is 610.0s and the maximum time is 1088634s.
#We will move further to investigate by gender. Box plots will be used for this purpose
ggplot(data = subset(new_york, !is.na(Gender)), aes("Gender",`Trip.Duration`, fill = Gender)) + geom_boxplot()+  
  scale_y_continuous(limits = c(0,2000)) +
  xlab("Gender") +  ylab("Time in Seconds ")+ ggtitle("Trip duration in New York City")

summary(chicago$`Trip.Duration`)

#The result shows that the minimum time is 60.0s, the median time is 670.0s and the maximum time is 85408.0s.
#We will move further to investigate by gender. Box plots will be used for this purpose
ggplot(data = subset(chicago, !is.na(Gender)), aes("Gender",`Trip.Duration`, fill = Gender)) + geom_boxplot()+  
  scale_y_continuous(limits = c(0,2000))+
  xlab("Gender") +  ylab("Time in Seconds ")+ ggtitle("Trip duration in Chicago")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
