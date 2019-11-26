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

con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con)
dbListFields(con,'yr_2007')
yr_2006 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")

all_yr_data <- bind_rows(yr_2007,yr_2008,yr_2009)
saveRDS(all_yr_data,"./Module 3 - Task 1/Data/Raw Data/all_yr_data.rds")

