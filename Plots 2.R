# ###########################################################################-
# GOAL: Analyze submeters
# DESCRIPTION: Explore the data
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


# Import DF ---------------------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed.rds")

# Data Viz -----------------------------------------------

all_yr_data %>% 
  filter(Year == 2008 & Month == 1 & Day == 9 & (minute(DateTime) %in% c(0,10,20,30,40,50))) %>% 
  plot_ly(., x = ~DateTime, y = ~Sub_metering_1, type = 'scatter', mode = 'lines',name = 'Kitchen') %>% 
  add_trace(y = ~Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


all_yr_data %>% 
  filter(Year == 2008 & Week == 12 & (minute(DateTime) == 0)) %>% 
  plot_ly(., x = ~DateTime, y = ~Sub_metering_1, type = 'scatter', mode = 'lines',name = 'Kitchen') %>% 
  add_trace(y = ~Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

all_yr_data %>% 
  filter(Year == 2008 & Month == 3 & (hour(DateTime) == 20 & minute(DateTime) == 0)) %>% 
  plot_ly(., x = ~DateTime, y = ~Sub_metering_1, type = 'scatter', mode = 'lines',name = 'Kitchen') %>% 
  add_trace(y = ~Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



all_yr_data %>% 
  filter(Year == 2008 & Month == 3 & Day == 10 & (hour(DateTime) %in% c(7,12,18,23)
                                                  & minute(DateTime) == 0)) %>%
  summarise(Kitchen = sum(Sub_metering_1),Laundry_Room = sum(Sub_metering_2),
            Water_Heater_and_AC = sum(Sub_metering_3)) %>% 
  melt(measure.vars=c("Kitchen","Laundry_Room","Water_Heater_and_AC")) %>% 
  plot_ly(labels = ~variable, values = ~value, type = 'pie') %>%
  layout(title = 'Percentage of total use at various times of day by each sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    
all_yr_data %>% 
  filter(Year == 2008 & Month == 5 & Day == 10) %>%
  summarise(Kitchen = sum(Sub_metering_1),Laundry_Room = sum(Sub_metering_2),
            Water_Heater_and_AC = sum(Sub_metering_3)) %>% 
  melt(measure.vars=c("Kitchen","Laundry_Room","Water_Heater_and_AC")) %>% 
  plot_ly(labels = ~variable, values = ~value, type = 'pie') %>%
  layout(title = 'Percentage of total use at various times of day by each sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


all_yr_data %>% 
  filter(Year == 2009) %>%
  summarise(Kitchen = sum(Sub_metering_1),Laundry_Room = sum(Sub_metering_2),
            Water_Heater_and_AC = sum(Sub_metering_3)) %>% 
  melt(measure.vars=c("Kitchen","Laundry_Room","Water_Heater_and_AC")) %>% 
  plot_ly(labels = ~variable, values = ~value, type = 'pie') %>%
  layout(title = 'Percentage of total use at various times of day by each sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))










