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
library(stats)

# Importing DF ------------------------------------------------------------

all_yr_data <- readRDS("./Module 3 - Task 1 + 2/Data/New Data/all_yr_data_preprocessed_no_na.rds")

# Testing Sub 1 -----------------------------------------------------------------

color <- vector()
color[1:1429] <- "blue"

# K-means

all_yr_data %>%
  group_by(Year, Yearday, Hour) %>%
  summarise(DateTime = first(DateTime), energy = sum(Global_active_power)) %>%
  mutate(Allday = factor(paste0(Year,"-",Yearday))) -> all_yr_data_smpl

set.seed(20)
all_yr_data_smpl %>% 
  ungroup() %>% 
  select(Year, Yearday, energy) %>%
  mutate(energy = scale(energy)) %>% 
  kmeans(., 3, nstart = 30) -> clusters

all_yr_data_smpl$Cluster <- as.factor(clusters$cluster)

all_yr_data_smpl %>% 
  group_by(Cluster, Hour) %>% 
  summarize(energy = mean(energy)) -> all_yr_data_clust

#Without patterns
all_yr_data_smpl %>% 
  ggplot() +
  geom_line(aes(x = Hour, y = energy, color = Allday), alpha = 0.05) +
  scale_color_manual(values=color) +
  scale_x_continuous(breaks = c(0:23)) +
  theme(legend.position = "none") +
  labs(x="Hours", y="Energy consumption (Wh)")

#Plot the results
all_yr_data_smpl %>% 
  ggplot() +
  scale_x_continuous(breaks = c(0:23)) +
  theme(legend.position = "none") +
  geom_line(data = all_yr_data_clust, aes(x = Hour, y = energy, color = Cluster),
            alpha = 1) +
  scale_color_manual(values=c("springgreen3","firebrick2","skyblue4","gold4")) +
  labs(x="Hours", y="Energy consumption (Wh)")

#Plot the result for a typical week
all_yr_data_smpl %>%
  mutate(Weekday = wday(DateTime)) %>% 
  group_by(Cluster, Weekday) %>% 
  summarize(energy = sum(energy)) %>% 
  ggplot() +
  geom_line(aes(x = Weekday, y = energy/1000, color = Cluster), alpha = 1) +
  scale_x_continuous(breaks = c(1:7),labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")) +
  theme(legend.position = "none") +
  scale_color_manual(values=c("springgreen3","firebrick2","skyblue4","gold4")) +
  labs(x="", y="Energy consumption (KWh)")



