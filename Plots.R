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

all_yr_data <- readRDS("./Module 3 - Task 1 + 2/Data/New Data/all_yr_data_preprocessed_no_na.rds")

# Data exploration for prez -----------------------------------------------

#summary
all_yr_data %>%
  group_by(Year,Month,Day) %>% 
  summarize(Sub_metering_1 = round(sum(Sub_metering_1)/1000,digits = 2),
            Sub_metering_2 = round(sum(Sub_metering_2)/1000,digits = 2),
            Sub_metering_3 = round(sum(Sub_metering_3)/1000,digits = 2)) %>%
  ungroup() %>% 
  select(Sub_metering_1,Sub_metering_2,Sub_metering_3) %>% 
  summary()

# distribution
all_yr_data %>%
  group_by(Year,Month,Day) %>% 
  summarize(Sub_metering_1 = round(sum(Sub_metering_1)/1000,digits = 2),
            Sub_metering_2 = round(sum(Sub_metering_2)/1000,digits = 2),
            Sub_metering_3 = round(sum(Sub_metering_3)/1000,digits = 2)) %>% 
  melt(measure.vars=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")) %>% 
  ggplot() +
  geom_boxplot(aes(x=variable, y=value)) +
  labs(x="",
       y="Consumption (KWh)")

# per week
all_yr_data %>%
  filter(Year == 2009) %>% 
  group_by(Week) %>% 
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3),Month=first(Month)) %>% 
  ggplot() +
  geom_col(aes(x = Week, y = Sub_metering_1/1000, fill = (Sub_metering_1/1000 > 20)),
           show.legend = F) +
  scale_fill_manual(values = c("salmon","darkred")) +
  theme_bw() +
  labs(title="Evolution of energy consumption",
       x="Time (weeks)",
       y="Consumption (KWh)")

# per month
all_yr_data %>%
  group_by(Year,Month) %>% 
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3),Global_active_power= sum(Global_active_power)) %>% 
  ggplot() +
  geom_line(aes(x = Month, y = Global_active_power/1000, color = factor(Year)),size=1.1) +
  scale_x_discrete(limits = c(1:12)) +
  theme_bw() +
  labs(title="Evolution of energy consumption per year",
       x="Time (months)",
       y="Consumption (KWh)",
       color="Years")

# all year total
all_yr_data %>%
  group_by(Year) %>%
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3)) %>%
  melt(measure.vars=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")) %>% 
  ggplot() +
  geom_col(aes(x = variable, y = value/1000, fill=factor(Year)), position = "dodge") +
  scale_fill_manual(values = c("skyblue","skyblue3","blue4")) +
  geom_text(aes(x = variable, y = value/1000, group = factor(Year),
                label = round(value/1000,digits = 2)),
            position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  labs(title="Evolution of energy consumption per sub-meter",
       x="",
       y="Consumption (KWh)",
       fill = "Years") +
  theme_bw()

# Evolution of energy consumption per weekday
all_yr_data %>%
  group_by(Year,Week,Weekday) %>%
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3)) %>%
  group_by(Year,Weekday) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1),Sub_metering_2 = mean(Sub_metering_2),
            Sub_metering_3 = mean(Sub_metering_3)) %>% 
  ggplot() +
  geom_line(aes(x = Weekday, y = Sub_metering_1/1000,color = factor(Year)),size=0.5) +
  scale_x_discrete(limits = c(1:7)) +
  theme_bw() +
  labs(title="Sub-meter 1",
       x="Time (months)",
       y="Consumption (KWh)",
       color="Years")

# Evolution of energy consumption per weekday (quarter view)
all_yr_data %>%
  filter(Year==2007) %>% 
  group_by(Quarter,Week,Weekday) %>%
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3)) %>%
  group_by(Quarter,Weekday) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1),Sub_metering_2 = mean(Sub_metering_2),
            Sub_metering_3 = mean(Sub_metering_3)) %>% 
  ggplot() +
  geom_line(aes(x = Weekday, y = Sub_metering_1/1000,color = factor(Quarter)),size=0.5) +
  scale_x_discrete(limits = c(1:7)) +
  theme_bw() +
  labs(title="2007",
       x="Time (months)",
       y="Consumption (KWh)",
       color="Quarters")


# Extra -------------------------------------------------------------------

all_yr_data %>%
  filter(Year == 2007) %>% 
  group_by(Week) %>% 
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3),TMEAN = mean(TMEAN)) %>% 
  ggplot() +
  geom_col(aes(x = Week, y = Sub_metering_1/1000), fill="gray90") +
  geom_line(aes(x = Week, y = TMEAN), color="aquamarine4", size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(title="Evolution of energy consumption (sub-meter 1)",
       x="Time (weeks)",
       y="Consumption (KWh)")


#Temp and submeter for 3 years
all_yr_data %>%
  group_by(Year,Month) %>% 
  summarize(Sub_metering_1 = sum(Sub_metering_1),Sub_metering_2 = sum(Sub_metering_2),
            Sub_metering_3 = sum(Sub_metering_3),TMEAN = mean(TMEAN),
            YM = first(paste0(Month,"-",substr(Year,nchar(Year)-1,nchar(Year))))) %>%
  ungroup() %>% 
  mutate(YM = factor(YM, levels = factor(YM))) %>% 
  ggplot() +
  geom_col(aes(x = YM, y = Sub_metering_1/1000,fill=factor(Year)), show.legend = FALSE) +
  scale_fill_manual(values = c("mistyrose1","mistyrose2","mistyrose3")) +
  geom_line(aes(x = YM, y = TMEAN*2, group = 1), color="firebrick4", size=0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Temp. (°C)")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank()) +
  labs(title="Sub-meter 1",
       y="Consumption (KWh)")


# Rates of energy (???/KWh)
# 2007 > B:0.1140 HP:0.1316 HC:0.0768
# 2008 > B:0.1106 HP:0.1106 HC:0.0673
# 2009 > B:0.1125 HP:0.1154 HC:0.0734
# 12h-16h et 21h30-7h30


# See the outliers --------------------------------------------------------






























