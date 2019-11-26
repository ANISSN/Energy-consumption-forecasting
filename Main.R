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
library(forecast)


# Importing the DF and preprocessing ---------------------------------------

all_yr_data <- read_delim("./Module 3 - Task 1/Data/Raw Data/household_power_consumption.txt",
                          delim = ";", na = c("?"))

all_yr_data$Date <- as_date(all_yr_data$Date, format = "%d/%m/%Y", tz = "GMT")

str(all_yr_data)
all_yr_data <- all_yr_data %>% unite(col = "DateTime", Date, Time, sep = " ")
all_yr_data$DateTime <- as_datetime(all_yr_data$DateTime)
attr(all_yr_data$DateTime, "tzone") <- "Europe/Paris"

saveRDS(all_yr_data,"./Module 3 - Task 1/Data/Raw Data/all_year_data.rds")
all_yr_data <- readRDS("./Module 3 - Task 1/Data/Raw Data/all_year_data.rds")

all_yr_data %<>% 
  select(DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,
         Global_active_power, Global_reactive_power)

all_yr_data$Year <- year(all_yr_data$DateTime)
all_yr_data$Quarter <- quarter(all_yr_data$DateTime, with_year = F)
all_yr_data$Month <- month(all_yr_data$DateTime)
all_yr_data$Week <- week(all_yr_data$DateTime)
all_yr_data$Weekday <- wday(all_yr_data$DateTime)
all_yr_data$Day <- day(all_yr_data$DateTime)
all_yr_data$Yearday <- yday(all_yr_data$DateTime)
all_yr_data$Hour <- hour(all_yr_data$DateTime)
all_yr_data$Minute <- minute(all_yr_data$DateTime)


#Convert KW into Wh
all_yr_data$Global_active_power <- round(all_yr_data$Global_active_power*1000/60,2)
all_yr_data$Global_reactive_power <- round(all_yr_data$Global_reactive_power*1000/60,2)

saveRDS(all_yr_data,"./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed.rds")


# Check duplicated dates
sum(duplicated(all_yr_data$DateTime))

# Replacing NA with knn ---------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed.rds")

all_yr_data %<>% 
  filter(Year != 2006)

colSums(is.na(all_yr_data))
all_yr_data %>%
  filter(is.na(Sub_metering_2), is.na(Sub_metering_3))

knn_1_replace_na <- function(df) {
  means <- vector()
  for(i in 1:length(df)){
    if(is.na(df[i])) {
      i_inf <- i
      i_sup <- i
      while(i_inf > 0 & is.na(df[i_inf])){
        i_inf <- i_inf - 10080
      }
      while(is.na(df[i_sup])){
        i_sup <- i_sup + 10080
      }
      if(i_inf < 0){
        means <- c(means,(df[i_sup])/2)
      }
      else{
        means <- c(means,(df[i_inf]+df[i_sup])/2)
      }
      
    }
  }
  i2 <- 1
  for(i in 1:length(df)){
    if(is.na(df[i])) {
      df[i] <- means[i2]
      i2 <- i2+1
    }
  }
  return(df)
}

all_yr_data$Sub_metering_1 <- knn_1_replace_na(all_yr_data$Sub_metering_1)
all_yr_data$Sub_metering_2 <- knn_1_replace_na(all_yr_data$Sub_metering_2)
all_yr_data$Sub_metering_3 <- knn_1_replace_na(all_yr_data$Sub_metering_3)
all_yr_data$Global_active_power <- knn_1_replace_na(all_yr_data$Global_active_power)
all_yr_data$Global_reactive_power <- knn_1_replace_na(all_yr_data$Global_reactive_power)

saveRDS(all_yr_data,"./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed_no_na.rds")
all_yr_data <- readRDS("./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed_no_na.rds")

# Adding temperature ------------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed.rds")
temperature <- read_csv("./Module 3 - Task 1/Data/New Data/orly_temperature.csv")

all_yr_data$DATE <- date(all_yr_data$DateTime)
all_yr_data %<>% 
  left_join(temperature)
all_yr_data$DATE <- NULL

saveRDS(all_yr_data,"./Module 3 - Task 1/Data/New Data/all_yr_data_preprocessed.rds")





