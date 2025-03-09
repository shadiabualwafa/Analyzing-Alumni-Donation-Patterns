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
# Average total donation by gender
ds_mean_gender <- ds_totalDonation %>%
  group_by(Gender) %>%
  summarize(Mean_Total_Donation = mean(Total_Donation))
# Create the pie chart
ggplot(data = ds_mean_gender, aes(x = "", y = Mean_Total_Donation, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(theta = "y") +  
  labs(
    title = "Average Total Donation Amounts by Gender",
    fill = "Gender"
  ) +
  scale_fill_discrete(labels = c(
    "M" = "Male",
    "F" = "Female"
  )) + 
  theme_void() +  
  geom_text(aes(label = paste0(round(Mean_Total_Donation, 2))), 
            position = position_stack(vjust = 0.5))


# 2 
# Average total donation by gender and marital status
ds_mean_gender_status <- ds_totalDonation %>%
  group_by(Marital.Status,Gender) %>%
  summarize(Total_Donation = sum(Total_Donation))
# Create a bar plot
ggplot(data = ds_mean_gender_status, aes(x = Gender, y = Total_Donation,fill = Marital.Status)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Donation Amounts by Gender and Marital Status",
    x = "Gender",
    y = "Total Donation",
    fill = "Marital Status"
  ) +
  facet_wrap(~ Marital.Status, ncol = 1,labeller = labeller(Marital.Status = c(
    "S" = "Single",
    "M" = "Married",
    "D" = "Divorced",
    "W" = "Widowed"))) +
  theme_minimal() +   
  scale_x_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"
  )) +   
  scale_fill_discrete(labels = c(
    "S" = "Single",
    "M" = "Married",
    "D" = "Divorced",
    "W" = "Widowed"
  ))  



# 3
# Average total donation by gender and number of events attended
ds_mean_attEvent_gender <- ds_totalDonation %>%
  group_by(Gender,AttendenceEvent) %>%
  summarize(Mean_Total_Donation = mean(Total_Donation))
# Create a bar plot
ggplot(data = ds_mean_attEvent_gender, aes(x = Gender, y = Mean_Total_Donation, fill = factor(AttendenceEvent))) +
  geom_col(position = "dodge") +  
  labs(
    title = "Average Total Donation Amounts by Number of Events Attended and Gender",
    x = "Gender",
    y = "Average Total Donation",
    fill = "Number of Event Attended"
  ) +
  theme_minimal() + 
  scale_x_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"
  ))


# 4
# Average total donation by gender and clsss year
ds_mean_gender_class <- ds_totalDonation %>%
  group_by(Class.Year,Gender) %>%
  summarize(Average_Total_Donation = mean(Total_Donation))
# Create a separated line chart
ggplot(data = ds_mean_gender_class, aes(x = factor(Class.Year), y = Average_Total_Donation, color = Gender, group = Gender)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  facet_wrap(~ Gender, ncol = 1, labeller = labeller(Gender = c(
    "F" = "Female",
    "M" = "Male"))) +  
  labs(
    title = "Average Total Donation by Gender and Class Year",
    x = "Class Year",
    y = "Average Total Donation",
    color = "Gender"  
  ) +
  scale_color_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"
  )) +  
  theme_minimal()  