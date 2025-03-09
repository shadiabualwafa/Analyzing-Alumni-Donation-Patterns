
# Import Required Libraries
library(tidyr)
library(ggplot2)
library(dplyr)

# Load Data set
ds <- read.csv("D:/Masters/Special Topics/R/Datasets/contribution.csv")






# Calculate average total donations by major
donations_by_nextDegree <- ds %>%
  group_by(Next.Degree) %>%
  summarise(Total_Donation = sum(FY00Giving + FY01Giving + FY02Giving + FY03Giving + FY04Giving, na.rm = TRUE))
View(donations_by_nextDegree %>% arrange(desc(Total_Donation)))