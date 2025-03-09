# Import Required Libraries
library(tidyr)
library(ggplot2)
library(dplyr)


# Load Data set
ds <- read.csv("D:/Masters/Special Topics/R/Datasets/contribution.csv")



# Calculate total donations for each individual
ds_totalDonation <- ds %>%
  mutate(Total_Donation = FY00Giving + FY01Giving + FY02Giving + FY03Giving + FY04Giving)




# 1
# Average total donation by number of events attended
ds_mean_attEvent <- ds_totalDonation %>%
  group_by(AttendenceEvent) %>%
  summarize(Mean_Total_Donation = mean(Total_Donation))
# Create the pie chart
ggplot(data = ds_mean_attEvent, aes(x = "", y = Mean_Total_Donation, fill = factor(AttendenceEvent))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Average Total Donation Amounts by Number of Events Attended",
    fill = "Number of Events Attended"
  ) +
  theme_void() +  
  geom_text(aes(label = paste0(round(Mean_Total_Donation, 2))), 
            position = position_stack(vjust = 0.5))  




# 2
# Average total donation by number of events attended and class year
ds_mean_attEvent_class_year <- ds_totalDonation %>%
  group_by(Class.Year,AttendenceEvent) %>%
  summarize(Mean_Total_Donation = mean(Total_Donation))
# Create a bar plot
ggplot(data = ds_mean_attEvent_class_year, aes(x = factor(AttendenceEvent), y = Mean_Total_Donation, fill = factor(AttendenceEvent))) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Total Donation Amounts by Number of Events Attended and  Class Year",
    x = "Number of Events Attended",
    y = "Average Total Donation",
    fill = "Number of Events Attended"
  ) +
  facet_wrap(~ factor(Class.Year), ncol = 1) +  
  theme_minimal()





