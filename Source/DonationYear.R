# Import Required Libraries
library(tidyr)
library(ggplot2)
library(dplyr)

# Load Data set
ds <- read.csv("D:/Masters/Special Topics/R/Datasets/contribution.csv")


ds_mean_donation_year <- ds_long %>% 
  group_by(Year) %>% 
  summarise(avg_total_donation = mean(Donation))

ggplot(data = ds_mean_donation_year, aes(x = Year, y = avg_total_donation,fill = Year)) +
  geom_col() + 
  labs(
    title = "Average Donation Amounts by Donation Year",
    x = "Donation Year",
    y = "Average Donation",
  ) + 
  scale_x_discrete(labels = c(
    "FY00Giving" = "2000",
    "FY01Giving" = "2001",
    "FY02Giving" = "2002",
    "FY03Giving" = "2003",
    "FY04Giving" = "2004"
  )) +
  theme(legend.position = "none") 