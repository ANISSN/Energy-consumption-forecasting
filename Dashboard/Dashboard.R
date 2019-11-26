# ###########################################################################-
# GOAL: Creating a dashboard for energy consumption
# DESCRIPTION: Creating a dashboard for energy consumption
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

# Shiny -------------------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(title = "Energy consumption monitoring"),
  
  dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Global Forecast", tabName = "Forecast", icon = icon("dashboard")),
        menuItem("Appliance optimization", tabName = "Appliance optimization", icon = icon("th"))
      ),
      uiOutput("out1")
  ),
  
  dashboardBody(includeCSS("./Module 3 - Task 1 + 2/Dashboard/www/custom.css"),
                
          conditionalPanel(condition = "input.tabs == 'Forecast'",
              # Choice of granularity
              radioButtons("period", h4("Period"),
                           choices = list("Weekly" = 1, "Monthly" = 2,
                                          "Yearly" = 3),selected = 1),
              # Weekly
              conditionalPanel(condition = "input.period == 1",
                  box(
                    plotlyOutput(outputId = "fc_week"),
                    sliderInput(inputId="slider1", label="# of weeks to forecast",
                                min=1, max=15, value=5)
                  )
              ),
              # Monthly
              conditionalPanel(condition = "input.period == 2",
                               box(
                                 plotlyOutput(outputId = "fc_month"),
                                 sliderInput(inputId="slider2", label="# of months to forecast",
                                             min=2, max=13, value=2),
                                 "* The last month of the forecast includes only a part of the energy
                                  consumption prediction."
                               )
              ),
              # Yearly
              conditionalPanel(condition = "input.period == 3",
                               box(
                                 plotlyOutput(outputId = "fc_year"),
                                 sliderInput(inputId="slider3", label="# of years to forecast",
                                             min=1, max=3, value=1)
                               )
              )
          ),
          conditionalPanel(condition = "input.tabs == 'Appliance optimization'",
                           selectInput("submeter", h4("Choose a sub-meter:"), 
                                       choices = list("Kitchen" = 1, "Laundry room" = 2,
                                                      "Heater / Air-con" = 3), selected = 1),
                           box(
                                plotlyOutput(outputId = "ao_day")
                           ),
                           box(
                             plotlyOutput(outputId = "ao_week")
                           )
          )
  )
)

server <- function(input, output) {
  
  #----------------------------------------- Forecasting
  seq_by_7 <- c(rep(1:204,each=7))
  #Adding the week info
  all_yr_data_per_week <- all_yr_data %>% 
      group_by(Year,Yearday) %>%
      summarize(DateTime=first(DateTime), Global_active_power=sum(Global_active_power),
              Sub1=sum(Sub_metering_1),Sub2=sum(Sub_metering_2),Sub3=sum(Sub_metering_3)) %>% 
      cbind(Week_all = seq_by_7[1:1426])
  #Grouping by week IF <---------------------------
  all_yr_data_per_week %<>% 
      group_by(Week_all) %>%
      summarize(DateTime=first(DateTime), Global_active_power= sum(Global_active_power))
  #Create the TS
  all_yr_data_per_week %>%
      select(ds = DateTime, y = Global_active_power) %>%
      mutate(y=y/1000) %>% 
      prophet() -> ts_per_week_fit2
  
  # --------------------------------------------- Per week
  output$fc_week <- renderPlotly({
    #Forecasting
    future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=input$slider1,freq=7*24*60*60)
    forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
    #Extract the dataframe
    start_for <- length(forecast_per_week2$yhat) - input$slider1 + 1
    end_for <- length(forecast_per_week2$yhat)
    forecast_df <- tibble(Week_all = c(48:(48 + input$slider1 - 1)),
                          DateTime = forecast_per_week2$ds[start_for:end_for],
                          Global_active_power = forecast_per_week2$yhat[start_for:end_for])
    #Ploting the results
    ggplot() + 
      geom_col(data=all_yr_data_per_week %>% filter(year(DateTime) == 2010),
               aes(x = c(1:47), y = Global_active_power/1000),fill = "peachpuff", color = "white",
               alpha = 0.7, width=1) +
      geom_col(data=forecast_df, aes(x = Week_all, y = Global_active_power), fill = "#E7B800",
               color = "white", alpha = 0.7, width=1) +
      theme(legend.position = "none") +
      theme_minimal() +
      scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
      labs(x="Weeks [2010]", y="Energy consumption (KWh)")
  })
  
  # --------------------------------------------- Per month
  output$fc_month <- renderPlotly({
    #Forecasting
    future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=round(input$slider2*4.3,0),
                                              freq=7*24*60*60)
    forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
    
    #Extract the dataframe
    start_for <- length(forecast_per_week2$yhat) - round(input$slider2*4.3,0) + 1
    end_for <- length(forecast_per_week2$yhat)
    forecast_df <- tibble(Week_all = c(48:(48 + round(input$slider2*4.3,0) - 1)),
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
      labs(x="Months [2010]", y="Energy consumption (KWh)")
  })
  
  # --------------------------------------------- Per year
  output$fc_year <- renderPlotly({
    #Forecasting
    future_per_week2 <- make_future_dataframe(ts_per_week_fit2,periods=(input$slider3*52+5),freq=7*24*60*60)
    forecast_per_week2 <- predict(ts_per_week_fit2,future_per_week2)
    
    #Extract the dataframe
    start_for <- length(forecast_per_week2$yhat) - (input$slider3*52+5) + 1
    end_for <- length(forecast_per_week2$yhat)
    forecast_df <- tibble(Week_all = c(48:(48 + input$slider3*52+5 - 1)),
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
  })
  

# Appliance opti ----------------------------------------------------------

  output$ao_day <- renderPlotly({
    
    if(input$submeter == 1){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_1)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 3
    }
    else if(input$submeter == 2){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_2)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 4
    }
    else if(input$submeter == 3){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_3)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 2
    }
    
    set.seed(20)
    all_yr_data_smpl %>% 
      ungroup() %>% 
      select(Yearday, Hour, energy) %>%
      mutate(energy = scale(energy)) %>% 
      kmeans(., clust, nstart = 30) -> clusters
    
    all_yr_data_smpl$Cluster <- as.factor(clusters$cluster)
    
    all_yr_data_smpl %>% 
      group_by(Cluster, Hour) %>% 
      summarize(energy = mean(energy)) -> all_yr_data_clust
    
    all_yr_data_smpl %>% 
      ggplot() +
      scale_x_continuous(breaks = c(0:23)) +
      theme(legend.position = "none") +
      geom_line(data = all_yr_data_clust, aes(x = Hour, y = energy, color = Cluster),
                alpha = 1) +
      scale_color_manual(values=c("springgreen3","firebrick2","skyblue4","gold4")) +
      labs(x="Hours", y="Energy consumption (Wh)")
  })
  
  output$ao_week <- renderPlotly({
    
    if(input$submeter == 1){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_1)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 3
    }
    else if(input$submeter == 2){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_2)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 4
    }
    else if(input$submeter == 3){
      all_yr_data %>%
        group_by(Year, Yearday, Hour) %>%
        summarise(DateTime = first(DateTime), energy = sum(Sub_metering_3)) %>%
        mutate(Allday = paste0(Year,"-",Yearday)) -> all_yr_data_smpl
      clust <- 2
    }
    
    set.seed(20)
    all_yr_data_smpl %>% 
      ungroup() %>% 
      select(Yearday, Hour, energy) %>%
      mutate(energy = scale(energy)) %>% 
      kmeans(., clust, nstart = 30) -> clusters
    
    all_yr_data_smpl$Cluster <- as.factor(clusters$cluster)
    
    all_yr_data_smpl %>% 
      group_by(Cluster, Hour) %>% 
      summarize(energy = mean(energy)) -> all_yr_data_clust
    
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
  })
}

shinyApp(ui, server)


