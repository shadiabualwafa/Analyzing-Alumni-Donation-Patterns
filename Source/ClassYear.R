# Import Required Libraries
library(tidyr)
library(ggplot2)
library(dplyr)

# Load Data set
ds <- read.csv("D:/Masters/Special Topics/R/Datasets/contribution.csv")

# Convert the dataset to long format
ds_long <- ds %>%
  pivot_longer(
    cols = starts_with("FY"),  
    names_to = "Year",         
    values_to = "Donation"     
  )


# 1
# Create a scatter plot
ggplot(data = ds_long, aes(x = factor(Class.Year), y = Donation)) +
  geom_jitter(alpha = 0.5, color = "steelblue") +  
  labs(
    title = "Donation Amounts by Class Year",
    x = "Class Year",
    y = "Donation Amount"
  ) +
  theme_minimal() 


# 2
# Calculate average donations by Year
ds_mean_class_year <- ds_long %>%
  group_by(Class.Year,Year) %>%
  summarise(Avg_Donation = mean(Donation, na.rm = TRUE))
# Create a separated line chart
ggplot(data = ds_mean_class_year, aes(x = factor(Class.Year), y = Avg_Donation, color = Year, group = Year)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  facet_wrap(~ Year, ncol = 1, labeller = labeller(Year = c(
    "FY00Giving" = "Year 2000",
    "FY01Giving" = "Year 2001",
    "FY02Giving" = "Year 2002",
    "FY03Giving" = "Year 2003",
    "FY04Giving" = "Year 2004"
  ))) +  
  labs(
    title = "Average Total Donation by Class Year and Donation Year",
    x = "Class Year",
    y = "Average Total Donation"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")  
