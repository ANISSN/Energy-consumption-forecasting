# ###########################################################################-
# GOAL: Appliance optimization
# DESCRIPTION: Appliance optimization
# AUTHOR: Aniss N
# ###########################################################################-

setwd("C:\\Users\\nisso\\Desktop\\Ubiqum\\Projects")

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(readr)
library(tidyr)
library(RMySQL)
library(reshape2)
library(magrittr)
library(scales)
library(plotly)
library(ggfortify)
library(forecast)
library(prophet)
library(shiny)
library(shinydashboard)

# Importing DF ------------------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1 + 2/Data/New Data/all_yr_data_preprocessed_no_na.rds")

# Testing -----------------------------------------------------------------

all_yr_data %>% 
  filter(Year == 2007, Yearday == 7) %>%
  mutate(grp = rep(1:(n()/10),each=10)) %>% 
  group_by(grp) %>% 
  summarise(Sub_metering_1 = max(Sub_metering_1), DateTime = first(DateTime)) %>% 
  ggplot() +
  geom_col(aes(x=DateTime,y = Sub_metering_1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M", tz = "Europe/Paris"))


all_yr_data %>% 
  filter(Year == 2007, Month == 3, Day %in% c(1,2,3,4,5,6,7)) %>%
  group_by(Day,Hour) %>%
  summarise(Sub_metering_1 = sum(Sub_metering_1), DateTime = first(DateTime)) %>% 
  ggplot() +
  geom_col(aes(x=DateTime,y = Sub_metering_1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  scale_x_datetime(breaks = date_breaks("12 hour"),labels = date_format("%H:%M", tz = "Europe/Paris"))


#Submeter 2
  
all_yr_data %>% 
  filter(Year == 2008, Yearday == 12) %>%
  mutate(grp = rep(1:(n()/10),each=10)) %>% 
  group_by(grp) %>% 
  summarise(Sub_metering_2 = max(Sub_metering_2), DateTime = first(DateTime)) %>% 
  ggplot() +
  geom_col(aes(x=DateTime,y = Sub_metering_2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M", tz = "Europe/Paris"))
  
  
all_yr_data %<>% 
  mutate(Fridge = if_else(Sub_metering_2 > 0 & Sub_metering_2 < 5,1,0),
         Wash_Machine = if_else(Sub_metering_2 > 5 & Sub_metering_2 < 25,1,0),
         Dryer = if_else(Sub_metering_2 > 25,1,0))



# Pattern detection -------------------------------------------------------



detect_fridge <- function(df){
  i <- 1
  result <- vector()
  while(i <= length(df$Sub_metering_2)){
    
    if(df$Sub_metering_2[i] > 0 & df$Sub_metering_2[i] < 10){
      #Check how many minutes the energy is positive
      test_positive <- F
      ipos <- i
      while(df$Sub_metering_2[ipos] > 0 & ipos <= length(df$Sub_metering_2)){
        ipos <- ipos+1
      }
      if((ipos-i) > 15 & (ipos-i) < 80){ test_positive <- T }
      
      #Check how many minutes the energy is null or > 10
      test_negative <- F
      ineg <- ipos
      while((df$Sub_metering_2[ineg] == 0 | df$Sub_metering_2[ineg] > 10) &
            ineg <= length(df$Sub_metering_2)){
        ineg <- ineg+1
      }
      if((ineg-ipos) > 40){ test_negative <- T }
      
      #Check the results
      if(test_positive == T & test_negative == T){
        result[i:(ipos-1)] <- 1
        result[ipos:(ineg-1)] <- 0
        i <- ineg
      }
      else if(test_positive == T & test_negative == F & ineg > length(df$Sub_metering_2)){
        result[i:(ipos-1)] <- 1
        result[ipos:(ineg-1)] <- 0
        i <- ineg
      }
      else{
        result[i:(ineg-1)] <- 0
        i <- ineg
      }
    }
    else{
      result[i] <- 0
      i <- i+1
    }
  }
  return(result)
}

all_yr_data %<>% cbind(fridge = detect_fridge(all_yr_data))

all_yr_data %>% 
  filter(Year == 2010, Yearday == 200) %>%
  mutate(grp = rep(1:(n()/10),each=10)) %>% 
  group_by(grp) %>% 
  summarise(Sub_metering_2 = max(Sub_metering_2),fridge = max(fridge),
            DateTime = first(DateTime)) %>% 
  ggplot() +
  geom_col(aes(x=DateTime,y = Sub_metering_2)) +
  geom_line(aes(x=DateTime,y = fridge+5), color="red") +
  ylim(0,10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M", tz = "Europe/Paris"))









