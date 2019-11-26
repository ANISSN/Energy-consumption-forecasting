# Creating time series ----------------------------------------------------

all_yr_data %>% 
  filter(Weekday == 2 & hour(DateTime) == 20 & minute(DateTime) == 1) %>%
  select(Sub_metering_3) %>% 
  ts(frequency=52, start=c(2007,1)) -> ts_SM3_weekly

all_yr_data %>% 
  filter(Weekday == 2 & hour(DateTime) == 20 & minute(DateTime) == 1) %>%
  select(Sub_metering_1) %>% 
  ts(frequency=52, start=c(2007,1)) -> ts_SM2_weekly

all_yr_data %>% 
  filter(Weekday == 2 & hour(DateTime) == 20 & minute(DateTime) == 1) %>%
  select(Sub_metering_2) %>% 
  ts(frequency=52, start=c(2007,1))

ts_SM2_weekly %>% 
  autoplot(ts.colour = 'darkblue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")


# Forecasting -------------------------------------------------------------

fitSM3 <- tslm(ts_SM3_weekly ~ trend + season)
summary(fitSM3)
forecastfitSM3 <- forecast(fitSM3, h=20)
plot(forecastfitSM3, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


# Decomposing -------------------------------------------------------------

decomp <- decompose(ts_SM3_weekly)
plot(decomp2)
summary(decomp)

decomp2 <- decompose(ts_SM2_weekly)

#removing the seasonal
ts_SM3_weekly_adj <- ts_SM3_weekly - decomp$seasonal
autoplot(ts_SM3_weekly_adj)
plot(decompose(ts_SM3_weekly_adj))

ts_SM2_weekly_adj <- ts_SM2_weekly - decomp2$seasonal
plot(decompose(ts_SM2_weekly_adj))

#holt winters
ts_SM3_weekly_hw <- HoltWinters(ts_SM3_weekly_adj, beta=FALSE, gamma=FALSE)
plot(ts_SM3_weekly_hw, ylim = c(0, 25))

ts_SM2_weekly_hw <- HoltWinters(ts_SM2_weekly_adj, beta=FALSE, gamma=FALSE)

#forecast
forecast_hw <- forecast(ts_SM3_weekly_hw, h=25, levels=c(10,25))
plot(forecast_hw, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3",start(2010))

forecast_hw2 <- forecast(ts_SM2_weekly_hw, h=25, levels=c(10,25))
plot(forecast_hw2, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")



