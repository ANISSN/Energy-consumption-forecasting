# ###########################################################################-
# GOAL: Create forecasting
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
library(prophet)

# Importing DF ------------------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1 + 2/Data/New Data/all_yr_data_preprocessed_no_na.rds")

# Per week ----------------------------------------------------------------

seq_by_7 <- c(rep(1:204,each=7))


all_yr_data_per_week <- all_yr_data %>% 
  group_by(Year,Yearday) %>%
  summarize(DateTime=first(DateTime), Global_active_power=sum(Global_active_power),
            Sub1=sum(Sub_metering_1),Sub2=sum(Sub_metering_2),Sub3=sum(Sub_metering_3)) %>% 
  cbind(Week_all = seq_by_7[1:1426])

all_yr_data_per_week %<>% 
  group_by(Week_all) %>%
  summarize(DateTime=first(DateTime), Global_active_power= sum(Global_active_power))


#--------------- using prophet

#Select data until end sept and convert to KWh
all_yr_data_per_week %>%
  filter((Week_all < 197)) %>%
  select(ds = DateTime, y = Global_active_power) %>%
  mutate(y=y/1000) %>% 
  prophet() -> ts_per_week_fit2

period <- 21
#Select the period of prediction (oct and dec)
future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=period,freq=7*24*60*60)

#Predict the consumption for the period and display it
forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
dyplot.prophet(ts_per_week_fit2,forecast_per_week2)


start_for <- length(forecast_per_week2$yhat) - period + 1
end_for <- length(forecast_per_week2$yhat)
forecast_df <- tibble(Week_all = c(start_for:end_for),
                      DateTime = forecast_per_week2$ds[start_for:end_for],
                      Global_active_power = forecast_per_week2$yhat[start_for:end_for])

#Focus on 2010
ggplot() + 
  geom_col(data=all_yr_data_per_week %>% filter(year(DateTime) == 2010),
           aes(x = Week_all, y = Global_active_power/1000),fill = "peachpuff", color = "white",
           alpha = 0.7, width=1) +
  geom_col(data=forecast_df, aes(x = Week_all, y = Global_active_power), fill = "skyblue",
           color = "white", alpha = 0.3, width=1) +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x="Weeks", y="Energy consumption (KWh)")

#All years plot
ggplot() + 
  geom_col(data=all_yr_data_per_week %>% filter((Week_all < 197)),
           aes(x = Week_all, y = Global_active_power/1000, 
               fill = factor(year(DateTime))), alpha = 0.7, width=1) +
  geom_col(data=forecast_df, aes(x = Week_all, y = Global_active_power), fill = "#E7B800",
           alpha = 0.7, width=1) +
  scale_color_manual(values = c("skyblue1", "skyblue2","skyblue3","peachpuff")) +
  scale_fill_manual(values = c("skyblue1", "skyblue2","skyblue3","peachpuff")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x="Weeks", y="Energy consumption (KWh)", fill="Years")+
  geom_line(data=all_yr_data_per_week,
            aes(x = Week_all, y = Global_active_power/1000), color="darkgray")

#-------------------------------- per month
  
  #Create the TS
  all_yr_data_per_week %>%
    select(ds = DateTime, y = Global_active_power) %>%
    mutate(y=y/1000) %>% 
    prophet() -> ts_per_week_fit2

  #Forecasting
  future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=round(3*4.3,0),freq=7*24*60*60)
  forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
  
  #Extract the dataframe
  start_for <- length(forecast_per_week2$yhat) - round(3*4.3,0) + 1
  end_for <- length(forecast_per_week2$yhat)
  forecast_df <- tibble(Week_all = c(48:(48 + round(3*4.3,0) - 1)),
                        DateTime = forecast_per_week2$ds[start_for:end_for],
                        Global_active_power = forecast_per_week2$yhat[start_for:end_for])
  
  all_yr_data_per_week %>% 
    filter(year(DateTime) == 2010) %>% 
    group_by(Year = year(DateTime), Month = month(DateTime)) %>% 
    summarise(DateTime= first(DateTime),
              Global_active_power = sum(Global_active_power)/1000) -> all_yr_data_per_month_2010
  
  forecast_df %>% 
    group_by(Year = year(DateTime), Month = month(DateTime)) %>% 
    summarise(DateTime= first(DateTime),
              Global_active_power = sum(Global_active_power)) -> forecast_df_month
  
  all_yr_data_per_month_2010[length(all_yr_data_per_month_2010$DateTime),4] <- 
    all_yr_data_per_month_2010[length(all_yr_data_per_month_2010$DateTime),4] +
    forecast_df_month[(forecast_df_month$Year == 2010 & forecast_df_month$Month == 11),4]
  
  forecast_df_month <-
  forecast_df_month[-(forecast_df_month$Year == 2010 & forecast_df_month$Month == 11),]
  
  forecast_df_month <- rbind(all_yr_data_per_month_2010[length(all_yr_data_per_month_2010$DateTime),],
                             forecast_df_month)
  
  #Ploting the results
  ggplot() + 
    geom_area(data=all_yr_data_per_month_2010,
             aes(x = DateTime, y = Global_active_power),fill = "peachpuff",
             color = "gray", alpha = 0.7) +
    geom_area(data=forecast_df_month,
             aes(x = DateTime, y = Global_active_power), fill = "#E7B800",
             color = "gray", alpha = 0.7) +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_x_datetime(breaks = date_breaks("1 month"), labels = date_format("%Y-%m",
                                                                          tz = "Europe/Paris")) +
    labs(x="Months", y="Energy consumption (KWh)")
  
#-------------------------------- per year
  
  #Forecasting
  future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=(3*52+6),freq=7*24*60*60)
  forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
  
  #Extract the dataframe
  start_for <- length(forecast_per_week2$yhat) - (3*52+6) + 1
  end_for <- length(forecast_per_week2$yhat)
  forecast_df <- tibble(Week_all = c(48:(48 + 3*52+6 - 1)),
                        DateTime = forecast_per_week2$ds[start_for:end_for],
                        Global_active_power = forecast_per_week2$yhat[start_for:end_for])

  all_yr_data_per_week %>% 
    group_by(Year = year(DateTime)) %>% 
    summarise(DateTime= first(DateTime),
              Global_active_power = sum(Global_active_power)/1000) -> all_yr_data_per_year
  
  forecast_df %>% 
    group_by(Year = year(DateTime)) %>% 
    summarise(DateTime= first(DateTime),
              Global_active_power = sum(Global_active_power)) -> forecast_df_year
  
  forecast_df_year[forecast_df_year$Year == 2010,3] <- 
  forecast_df_year[forecast_df_year$Year == 2010,3] +
  all_yr_data_per_year[all_yr_data_per_year$Year == 2010,3]
  
  all_yr_data_per_year <- all_yr_data_per_year[-length(all_yr_data_per_year$Year),]

  #Ploting the results
  ggplot() + 
    geom_col(data=all_yr_data_per_year,
              aes(x = Year, y = Global_active_power),fill = "peachpuff",
              color = "gray", alpha = 0.7) +
    geom_col(data=forecast_df_year,
              aes(x = Year, y = Global_active_power), fill = "#E7B800",
              color = "gray", alpha = 0.7) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(min(all_yr_data_per_year$Year):max(forecast_df_year$Year))) +
    labs(x="Months", y="Energy consumption (KWh)")
  
  


