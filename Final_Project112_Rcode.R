library(dplyr)
library(tidyverse)

data <- read_csv("data.csv")

cleaned_fifa_data <- data %>%
  select(-X1, -ID, -Flag, -Potential, -`Club Logo`, -Wage, -Special, -`International Reputation`, -`Weak Foot`, -`Skill Moves`,
         -`Work Rate`, -`Body Type`, -`Real Face`, -`Jersey Number`, -Joined, -`Loaned From`, -`Contract Valid Until`,
         -LS, -ST, -RS, -LW, -LF, -CF, -RF, -RW, -LAM, -CAM, -RAM, -LCM, -CM, -RCM, -RM, -LB, -LM, -LCM, -CM, -RCM, 
         -RM, -LWB, -LDM, -CDM, -RDM, -RWB, -LCB, -CB, -RCB, -RB, -`Release Clause`)

#Mission Statement: Finding Player Optimization for different players and teams to see which playing style fits to a
# certain Fifa player, whether they likes Strong players, fast players, good passing, good dribbling, etc.