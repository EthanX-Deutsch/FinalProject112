library(dplyr)
library(tidyverse)
library(shiny)

data <- read_csv("data.csv")

cleaned_fifa_data <- data %>%
  select(-X1, -ID, -Flag, -Potential, -`Club Logo`, -Wage, -Special, -`International Reputation`, -`Weak Foot`, -`Skill Moves`,
         -`Work Rate`, -`Body Type`, -`Real Face`, -`Jersey Number`, -Joined, -`Loaned From`, -`Contract Valid Until`,
         -LS, -ST, -RS, -LW, -LF, -CF, -RF, -RW, -LAM, -CAM, -RAM, -LCM, -CM, -RCM, -RM, -LB, -LM, -LCM, -CM, -RCM, 
         -RM, -LWB, -LDM, -CDM, -RDM, -RWB, -LCB, -CB, -RCB, -RB, -`Release Clause`)

Arsenal <- cleaned_fifa_data %>%
  filter(Club %in% c("Arsenal"))

Brighton <- cleaned_fifa_data %>%
  filter(Club %in% c("Brighton & Hove Albion"))

Bournemouth <- cleaned_fifa_data %>%
  filter(Club %in% c("Bournemouth"))

Burnley <- cleaned_fifa_data %>%
  filter(Club %in% c("Burnley"))

Cardiff_City <- cleaned_fifa_data %>%
  filter(Club %in% c("Cardiff City"))

Chelsea <- cleaned_fifa_data %>%
  filter(Club %in% c("Chelsea"))

Crystal_Palace <- cleaned_fifa_data %>%
  filter(Club %in% c("Crystal Palace"))
         
Everton <- cleaned_fifa_data %>%
  filter(Club %in% c("Everton"))

Fulham <- cleaned_fifa_data %>%
  filter(Club %in% c("Fulham"))

Huddersfield_Town <-  cleaned_fifa_data %>%
  filter(Club %in% c("Huddersfield Town"))

Leicester <- cleaned_fifa_data %>%
  filter(Club %in% c("Leicester City"))

Liverpool <- cleaned_fifa_data %>%
  filter(Club %in% c("Liverpool"))

Manchester_City <- cleaned_fifa_data %>%
  filter(Club %in% c("Manchester City"))

Manchester_United <- cleaned_fifa_data %>%
  filter(Club %in% c("Manchester United"))

Newcastle <- cleaned_fifa_data %>%
  filter(Club %in% c("Newcastle United"))

Southampton <- cleaned_fifa_data %>%
  filter(Club %in% c("Southampton"))

Tottenham <- cleaned_fifa_data %>%
  filter(Club %in% c("Tottenham Hotspur"))

Watford <- cleaned_fifa_data %>%
  filter(Club %in% c("Watford"))

West_Ham <- cleaned_fifa_data %>%
  filter(Club %in% c("West Ham United"))

Wolves <- cleaned_fifa_data %>%
  filter(Club %in% c("Wolverhampton Wanderers"))
         
ui <- fluidPage(
  selectInput(
    inputId = "team",
    label = "Teams",
    choices = unique(cleaned_fifa_data$Club),
    multiple = TRUE),
  submitButton(text = "Choose Team"),
  reactiveValues()
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

#Mission Statement: Finding Player Optimization for different players and teams to see which playing style fits to a
# certain Fifa player, whether they like Strong players, fast players, good passing, good dribbling, etc.