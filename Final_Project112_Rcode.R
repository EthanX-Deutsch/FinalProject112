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
  mutate(`Pace Ability` = ((Acceleration*.45) + (SprintSpeed*.55)),
         `Shooting Ability` = ((Finishing*.45) + (LongShots*.2) + (Penalties*.05) + (Positioning*.05) + (ShotPower*.2) + (Volleys*.05)),
         `Passing Ability` = ((Crossing*.2) + (Curve*.05) + (FKAccuracy*.05) + (LongPassing*.15) + (ShortPassing*.35) + (Vision*.25)),
         `Dribbling Ability` = ((Agility*.1) + (Balance*.05) + (BallControl*.35) + (Dribbling*.5)),
         `Defending Ability` = ((HeadingAccuracy*.1) + (Interceptions*.2) + (Marking*.3) + (SlidingTackle*.1) + (StandingTackle*.3)),
         `Physical Ability` = ((Aggression*.2) + (Jumping*.05) + (Stamina*.25) + (Strength*.5))) %>%
  mutate(gen_position = ifelse(Position %in% c("ST", "CF", "LF", "RF", "LS", "RS"), "ATT", 
                               ifelse(Position %in% c("RW", "LW", "LM", "RM", "CM", "AM", "CAM", "CDM", "RAM", "RDM", "LAM", "LDM", "RCM", "LCM"), "MID",
                               ifelse(Position %in% c("LB", "RB", "LWB", "RWB", "CB", "LCB", "RCB"), "DEF", "GK"))))

ui <- fluidPage(
  splitLayout(selectInput(
    inputId = "first_team",
    label = "Choose First Team",
    choices = unique(cleaned_fifa_data$Club),
    multiple = FALSE),
    selectInput(
      inputId = "second_team",
      label = "Choose Another Team",
      choices = unique(cleaned_fifa_data$Club),
      multiple = FALSE),
    tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
  splitLayout(submitButton("Select Team"),
              submitButton("Select Team")),
  splitLayout(uiOutput("first_players"),
              uiOutput("second_players"),
              tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
  splitLayout(submitButton("Select Players"),
              submitButton("Select Players")),
  splitLayout(uiOutput("first_goalkeep"),
              uiOutput("second_goalkeep"),
              tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
  splitLayout(submitButton("Select Goalkeeper"),
              submitButton("Select Goalkeeper")),
  varSelectInput(
    inputId = "hex_category",
    label = "Choose an Attribute",
    data = cleaned_fifa_data %>%
      select(`Pace Ability`, 
             `Shooting Ability`, 
             `Passing Ability`, 
             `Defending Ability`, 
             `Physical Ability`, 
             `Dribbling Ability`)),
  submitButton("Compare Teams"),
  splitLayout(
    plotOutput("team1"),
    plotOutput("team2")),
  splitLayout(tableOutput("goalkeep1"),
              tableOutput("goalkeep2")),
)



server <- function(input, output) {
  output$first_players <- renderUI({
    player_options1 <- cleaned_fifa_data %>% 
      filter(Club %in% input$first_team) %>%
      filter(Position != "GK") %>%
      pull(Name)
    selectInput(inputId = "first_players", 
                label = "Choose 10 Players",
                choices = player_options1,
                multiple = TRUE)
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
    selectInput(inputId = "second_players", 
                label = "Choose 10 Players",
                choices = player_options2,
                multiple = TRUE)
  })
  output$second_goalkeep <- renderUI({
    goalkeep_options2 <- cleaned_fifa_data %>% 
      filter(Club %in% input$second_team) %>%
      filter(Position %in% c("GK")) %>%
      pull(Name)
    selectInput(inputId = "goalkeep_options2", 
                label = "Choose 1 Goalkeeper",
                choices = goalkeep_options2)
  })
  output$team1 <- renderPlot({
    cleaned_fifa_data %>% 
      filter(Club %in% input$first_team) %>%
      filter(Name %in% input$first_players) %>%
      ggplot(aes(x = reorder(input$first_players, gen_position), y = !!input$hex_category, fill = gen_position)) +
      geom_bar(stat = "identity") +
      geom_hline(aes(yintercept = mean(!!input$hex_category))) +
      scale_fill_manual(values = c("ATT" = "royalblue3", "DEF" = "darkorange2", "MID" = "green3")) +
      labs(x = "Player", y = "Score", fill = "Position") +
      ylim(0, 100) +
      coord_flip() +
      ggthemes::theme_tufte()
    
  })
  output$team2 <- renderPlot({
    cleaned_fifa_data %>% 
      filter(Club %in% input$second_team) %>%
      filter(Name %in% input$second_players) %>%
      ggplot(aes(x = reorder(input$second_players, gen_position), y = !!input$hex_category, fill = gen_position)) +
      geom_bar(stat = "identity") +
      geom_hline(aes(yintercept = mean(!!input$hex_category))) +
      scale_fill_manual(values = c("ATT" = "royalblue3", "DEF" = "darkorange2", "MID" = "green3")) +
      labs(x = "Player", y = "Score", fill = "Position") +
      ylim(0, 100) +
      coord_flip() +
      ggthemes::theme_tufte()
  })
  output$goalkeep1 <- renderTable({
    cleaned_fifa_data %>%
      select(Name, Position, Overall) %>%
      filter(Name %in% input$goalkeep_options1) 
  })
  output$goalkeep2 <- renderTable({
    cleaned_fifa_data %>%
      select(Name, Position, Overall) %>%
      filter(Name %in% input$goalkeep_options2)
  })
}

shinyApp(ui = ui, server = server)
