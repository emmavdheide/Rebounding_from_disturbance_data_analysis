#Code for a figure showing 2022 precipitation against the average precipitation from 2014-2021
#Precipitation data from https://newa.cornell.edu/all-weather-data-query/ site "Rock Spring (PSU Vineyard)

#set working directory
setwd("~/Grad School/Penn State/Project Information/2022 Summer Pilot Study/Rebounding_from_disturbance_data_analysis")

#import data
dat<-read.csv("Cumulative Precipitation.csv")
head(dat)

#Column "Date" is in MM/DD/YYYY format so that R will recognize dates, even though this information is important for day of year but not year itself

#Make sure R knows that the dates are dates
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")

#rearrange data so that it's in long format rather than wide
library(tidyr)
dat_long <- pivot_longer(dat, cols = c("X2022", "X2014_2021_Average"), 
                              names_to = "Series", 
                              values_to = "CumulativeRainfall")

#make the figure
library(ggplot2)

p<-ggplot(dat_long, aes(x = as.Date(Date), y = CumulativeRainfall, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c('X2022' = 'blue', 'X2014_2021_Average' = 'black'), 
                     labels = c('2014-2021 Average', '2022')) +
  labs(
    title = "Cumulative Rainfall Over Time",
    x = "Date",
    y = "Cumulative Rainfall (inches)",
    color = "Series"
  ) +
  scale_x_date(
    date_breaks = "1 month",        # Set breaks at the first day of each month
    date_labels = "%b %d"           # Format labels as abbreviated month and year
  ) +
  theme_minimal()
  
p
