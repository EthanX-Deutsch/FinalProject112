library(dplyr)
library(tidyverse)

cleaned_fifa_data <- data %>%
  select(Name, Age, Nationality, Overall, Club, Value)
