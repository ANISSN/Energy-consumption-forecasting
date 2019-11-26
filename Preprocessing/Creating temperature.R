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


# Creating temperature file -----------------------------------------------


tmean <- read_delim("./Module 3 - Task 1/Data/Raw Data/orly_tmean.txt",
                    delim = ",", na = c("-9999"))

tmax <- read_delim("./Module 3 - Task 1/Data/Raw Data/orly_tmax.txt",
                   delim = ",", na = c("-9999"))

tmin <- read_delim("./Module 3 - Task 1/Data/Raw Data/orly_tmin.txt",
                   delim = ",", na = c("-9999"))

tmean$DATE <- as_date(as.character(tmean$DATE), format = "%Y%m%d", tz = "GMT")
tmax$DATE <- as_date(as.character(tmax$DATE), format = "%Y%m%d", tz = "GMT")
tmin$DATE <- as_date(as.character(tmin$DATE), format = "%Y%m%d", tz = "GMT")

tmean$TG <- round(tmean$TG*0.1,digits = 2)
tmax$TX <- round(tmax$TX*0.1,digits = 2)
tmin$TN <- round(tmin$TN*0.1,digits = 2)

tmean$Year <- year(tmean$DATE)
tmax$Year <- year(tmax$DATE)
tmin$Year <- year(tmin$DATE)

tmean %<>% filter(Year == 2007 | Year == 2008 | Year == 2009) %>% select(DATE,TG)
tmax %<>% filter(Year == 2007 | Year == 2008 | Year == 2009) %>% select(DATE,TX)
tmin %<>% filter(Year == 2007 | Year == 2008 | Year == 2009) %>% select(DATE,TN)

temperature <- tmean %>% 
  left_join(tmax) %>% 
  left_join(tmin)

colnames(temperature) <- c("DATE", "TMEAN","TMAX","TMIN")

temperature %>% 
  filter(is.na(TMIN))
temperature$TMIN <- na.locf(temperature$TMIN)

write_csv(temperature,"./Module 3 - Task 1/Data/New Data/orly_temperature.csv")




