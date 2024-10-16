##### Max Maksutova in class exercise Code Prep 10/16 #####
# clear environment
rm(list=ls())

# load libraries - I load a bunch just to be safe
library(dplyr)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(scales)
library(triangle)
library(md)
library(gt)
library(fpp3)
library(TTR)
library(tibble)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)

##### Q1 #####
# load data in
avo <- read.csv("https://jagelves.github.io/Data/Avocado2020_2024P.csv")

# dimensions of avo
dim(avo)
  # Answer: [1] 28674    13

##### Q2 #####
# columns of new tibble
Week <- c("1/3/21","1/3/21")
Type <- c("Organic","Conventional")
Geography <- c("Richmond/Norfolk","Richmond/Norfolk")
Average_Price <- c(1.055744,0.9048261)

# create new tibble
new <- tibble(Geography, Week, Type, Average_Price)

# combo with avo 
avo <- as_tibble(avo)
new_avo <- bind_rows(avo, new) # fills in NAs for missing columns

# conventional & Richmond/Norfolk average price
conv_RN_avg <- new_avo %>%
  filter(Geography == "Richmond/Norfolk", Type == "Conventional") %>% # filter by R/N & conv
  select(Average_Price) %>% # select avg price colum
  summarise(Average_Price = mean(Average_Price)) #take mean of average price column for avg of avgs

print(conv_RN_avg)
  # Answer: 1.066607

##### Q3 #####
# group data by week & read as dates
wk_grp <- new_avo %>%
  select(Week, Average_Price) %>% 
  filter(!is.na(Week)) %>%
  mutate(Week = mdy(Week)) %>%
  group_by(Week) %>%
  summarise(count = n())

# Use diff function on dates column
date_diffs <- diff(wk_grp$Week)

date_diffs # count any pairs where not a multiple of 7

#fix values for each to be a week period
fix_new_avo <- new_avo %>%
  mutate(Week = case_when(
    Week == "1/13/20" ~ "1/12/20",
    Week == "1/11/21" ~ "1/10/21",
    Week == "1/10/22" ~ "1/9/22",
    Week == "1/9/23" ~ "1/8/23",
    Week == "1/8/24" ~ "1/7/24",
    TRUE ~ Week))

#updated diffs
wk_grp <- fix_new_avo %>%
  select(Week, Average_Price) %>%
  filter(!is.na(Week)) %>%
  mutate(Week = mdy(Week)) %>%
  group_by(Week) %>%
  summarise(count = n())
diff(wk_grp$Week)


##### Q4 #####
# filter for R/N & conv
fix_new_avo <- fix_new_avo %>% 
  filter(Geography == "Richmond/Norfolk", Type == "Conventional") %>%
select(Week, Average_Price)
#format week as date & remove nas in week
fix_new_avo <- fix_new_avo %>%
  mutate(Week = mdy(Week)) %>%
  drop_na(Week)%>%
  group_by(Week)

#create tsibble
new_avo_ts <- fix_new_avo %>% as_tsibble(index= Week)

decomp_new_avo <- new_avo_ts %>%
  model(STL = STL(Average_Price~trend()+season())) %>% 
  components()

first_obs <- decomp_new_avo %>%
  filter(Week == ymd("2020-01-12"))

decomp_new_avo %>% autoplot()

print(first_obs)
 # Answer: trend: 1.003472
 # season: -0.1097057
 # remainder: 0.05149151



##### Q5 - mean model #####
# fit mean model
mean_price <- mean(fix_new_avo$Average_Price)
fix_new_avo_mean <- fix_new_avo %>% 
  mutate(predicted_mean = mean_price)

# mean rmse
rmse_mean <- sqrt(mean((fix_new_avo_mean$Average_Price - fix_new_avo_mean$predicted_mean)^2, na.rm = TRUE))
print(rmse_mean)
   # Answer: [1] 0.1367483

##### Q6 - mean forcast #####
# mean model & forcast
mean_model <- new_avo_ts %>%
  model(mean = MEAN(Average_Price))
mean_forcast <- mean_model %>%
  forecast(h = 5)
print(mean_forcast$.mean)
  # Answer: [1] 1.066607

##### Q5 - lin model #####
# fit lin model
lin_model <- lm(Average_Price ~ Week, data = new_avo_ts)

# predicted prices using predict()
predicted_prices <- predict(lin_model, newdata = new_avo_ts)

# create blank column in new_avo_ts
lin_data <- new_avo_ts %>% select(Week, Average_Price) %>%
  mutate(predicted_price = NA)

#for loop to add data to lin_data predicted prices column
for (i in 1:length(predicted_prices)) {
  lin_data$predicted_price[i] <- predicted_prices[i]
}

# calc RMSE
rmse_lin <- sqrt(mean((lin_data$Average_Price - lin_data$predicted_price)^2, na.rm = TRUE))
print(rmse_lin)
  # Answer: [1] 0.1336333

##### Q6 - lin forcast #####
# Create vector for future weeks
most_recent_week <- max(lin_data$Week)
forcast_period <- 5
future_weeks <- seq(most_recent_week + weeks(1), by = "week", length.out = forcast_period)
future_weeks_tb <- tibble(Week = as.Date(future_weeks)) #format as date

# predictions for future weeks based on lin model
forcast_lin <- predict(lin_model, newdata = future_weeks_tb)

# combo forcast in future weeks tibble
future_weeks_tb <- future_weeks_tb %>%
  mutate(predicted_price = forcast_lin)

print(future_weeks_tb)
  # Answer: 1.12

