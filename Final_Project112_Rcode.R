library(dplyr)
library(tidyverse)

data <- read_csv("data.csv")

cleaned_fifa_data <- data %>%
  select(Name, Age, Nationality, Overall, Club, Value)
