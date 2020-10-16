library(dplyr)
library(tidyverse)
library(shiny)

data <- read_csv("data.csv")

cleaned_fifa_data <- data %>%
  select(-X1, -ID, -Flag, -Potential, -`Club Logo`, -Wage, -Special, -`International Reputation`, -`Weak Foot`, -`Skill Moves`,
         -`Work Rate`, -`Body Type`, -`Real Face`, -`Jersey Number`, -Joined, -`Loaned From`, -`Contract Valid Until`,
         -LS, -ST, -RS, -LW, -LF, -CF, -RF, -RW, -LAM, -CAM, -RAM, -LCM, -CM, -RCM, -RM, -LB, -LM, -LCM, -CM, -RCM, 
         -RM, -LWB, -LDM, -CDM, -RDM, -RWB, -LCB, -CB, -RCB, -RB, -`Release Clause`) %>%
  filter(Club %in% c("Arsenal", "Brighton & Hove Albion", 
                     "Bournemouth", "Burnley", "Cardiff City", 
                     "Chelsea", "Crystal Palace", "Everton",
                     "Fulham", "Huddersfield Town", "Leicester City", 
                     "Liverpool", "Manchester City", "Manchester United",
                     "Newcastle United", "Southampton", "Tottenham Hotspur",
                     "Watford", "West Ham United", "Wolverhampton Wanderers")) %>%
  mutate(hex_pace = ((Acceleration*.45) + (SprintSpeed*.55)),
         hex_shooting = ((Finishing*.45) + (LongShots*.2) + (Penalties*.05) + (Positioning*.05) + (ShotPower*.2) + (Volleys*.05)),
         hex_passing = ((Crossing*.2) + (Curve*.05) + (FKAccuracy*.05) + (LongPassing*.15) + (ShortPassing*.35) + (Vision*.25)),
         hex_dribbling = ((Agility*.1) + (Balance*.05) + (BallControl*.35) + (Dribbling*.5)),
         hex_defending = ((HeadingAccuracy*.1) + (Interceptions*.2) + (Marking*.3) + (SlidingTackle*.1) + (StandingTackle*.3)),
         hex_physical = ((Aggression*.2) + (Jumping*.05) + (Stamina*.25) + (Strength*.5)))

ui <- fluidPage(
  selectInput(
    inputId = "first_team",
    label = "Choose a Team",
    choices = unique(cleaned_fifa_data$Club),
    multiple = FALSE),
  uiOutput("first_players"),
  uiOutput("first_goalkeep"),
  plotOutput("team1"),
  selectInput(
    inputId = "second_team",
    label = "Choose a Team",
    choices = unique(cleaned_fifa_data$Club),
    multiple = FALSE),
  uiOutput("second_players"),
  uiOutput("second_goalkeep"),
  plotOutput("team2")
  
)

server <- function(input, output) {
  output$first_players <- renderUI({
    player_options1 <- cleaned_fifa_data %>% 
      filter(Club %in% input$first_team) %>%
      filter(Position != "GK") %>%
      pull(Name)
    selectInput(inputId = "player_options1", 
                label = "Choose 10 Players",
                choices = player_options1)
  })
  output$first_goalkeep <- renderUI({
    goalkeep_options1 <- cleaned_fifa_data %>% 
      filter(Club %in% input$first_team) %>%
      filter(Position %in% c("GK")) %>%
      pull(Name)
    selectInput(inputId = "goalkeep_options1", 
                label = "Choose 1 Goalkeeper",
                choices = goalkeep_options1)
  })
  output$second_players <- renderUI({
    player_options2 <- cleaned_fifa_data %>% 
      filter(Club %in% input$second_team) %>%
      filter(Position != "GK") %>%
      pull(Name)
    selectInput(inputId = "player_options2", 
                label = "Choose 10 Players",
                choices = player_options2)
  })
  output$second_goalkeep <- renderUI({
    goalkeep_options2 <- cleaned_fifa_data %>% 
      filter(Club %in% input$fsecond_team) %>%
      filter(Position %in% c("GK")) %>%
      pull(Name)
    selectInput(inputId = "goalkeep_options2", 
                label = "Choose 1 Goalkeeper",
                choices = goalkeep_options2)
  })
}

shinyApp(ui = ui, server = server)

#Mission Statement: Finding Player Optimization for different players and teams to see which playing style fits to a
# certain Fifa player, whether they like Strong players, fast players, good passing, good dribbling, etc.
# 
# Arsenal <- cleaned_fifa_data %>%
#   filter(Club %in% c("Arsenal"))
# 
# Brighton <- cleaned_fifa_data %>%
#   filter(Club %in% c("Brighton & Hove Albion"))
# 
# Bournemouth <- cleaned_fifa_data %>%
#   filter(Club %in% c("Bournemouth"))
# 
# Burnley <- cleaned_fifa_data %>%
#   filter(Club %in% c("Burnley"))
# 
# Cardiff_City <- cleaned_fifa_data %>%
#   filter(Club %in% c("Cardiff City"))
# 
# Chelsea <- cleaned_fifa_data %>%
#   filter(Club %in% c("Chelsea"))
# 
# Crystal_Palace <- cleaned_fifa_data %>%
#   filter(Club %in% c("Crystal Palace"))
# 
# Everton <- cleaned_fifa_data %>%
#   filter(Club %in% c("Everton"))
# 
# Fulham <- cleaned_fifa_data %>%
#   filter(Club %in% c("Fulham"))
# 
# Huddersfield_Town <-  cleaned_fifa_data %>%
#   filter(Club %in% c("Huddersfield Town"))
# 
# Leicester <- cleaned_fifa_data %>%
#   filter(Club %in% c("Leicester City"))
# 
# Liverpool <- cleaned_fifa_data %>%
#   filter(Club %in% c("Liverpool"))
# 
# Manchester_City <- cleaned_fifa_data %>%
#   filter(Club %in% c("Manchester City"))
# 
# Manchester_United <- cleaned_fifa_data %>%
#   filter(Club %in% c("Manchester United"))
# 
# Newcastle <- cleaned_fifa_data %>%
#   filter(Club %in% c("Newcastle United"))
# 
# Southampton <- cleaned_fifa_data %>%
#   filter(Club %in% c("Southampton"))
# 
# Tottenham <- cleaned_fifa_data %>%
#   filter(Club %in% c("Tottenham Hotspur"))
# 
# Watford <- cleaned_fifa_data %>%
#   filter(Club %in% c("Watford"))
# 
# West_Ham <- cleaned_fifa_data %>%
#   filter(Club %in% c("West Ham United"))
# 
# Wolves <- cleaned_fifa_data %>%
#   filter(Club %in% c("Wolverhampton Wanderers"))

