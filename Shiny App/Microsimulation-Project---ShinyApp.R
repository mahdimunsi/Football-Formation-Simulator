library(tidyverse)
data <- read.csv("Microsimulation_Games_2.csv")

data <- data %>%
  drop_na(home_club_formation, away_club_formation, competition_name) %>%
  mutate(winner = ifelse(winner_club_name == home_club_name, "home",
                         ifelse(winner_club_name == away_club_name, "away", "draw")))

################################################################################

# Training the model
library(caret)
model <- train(winner ~ home_club_formation + away_club_formation + competition_name, data = data,
               method = "multinom")

################################################################################

# Simulating the model

simulate_match <- function(home_formation, away_formation, competition) {
  predict(model, newdata = data.frame(home_club_formation = home_formation,
                                      away_club_formation = away_formation,
                                      competition_name = competition), type = "prob")
}

# Example simulation
simulate_match("3-1-4-2", "4-2-3-1", "LaLiga") *100


################################################################################

# Building the Shiny App

ui <- fluidPage(
  titlePanel("Microsimulation Football Match Predictor"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("competition", "Choose the Competition:", 
                  choices = c("LaLiga", "Ligue 1", "Serie A", "Premier League", "Bundesliga"),
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("home_formation", "Choose Home Team Formation:", 
                  choices = c("4-2-3-1", "4-3-2-1", "4-3-3", "4-4-2", "4-1-4-1", "4-3-1-2", "3-5-2", "5-4-1", "3-6-1", "4-5-1", "3-4-3", "5-3-2", "4-4-1-1", "3-4-2-1", "3-1-4-2", "3-4-1-2", "4-1-3-2", "3-3-3-1", "4-2-4", "5-2-3"),
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("away_formation", "Choose Away Team Formation:", 
                  choices = c("4-2-3-1", "4-3-2-1", "4-3-3", "4-4-2", "4-1-4-1", "4-3-1-2", "3-5-2", "5-4-1", "3-6-1", "4-5-1", "3-4-3", "5-3-2", "4-4-1-1", "3-4-2-1", "3-1-4-2", "3-4-1-2", "4-1-3-2", "3-3-3-1", "4-2-4", "5-2-3"),
                  options = pickerOptions(actionsBox = TRUE)),
      actionButton("simulate", "Simulate Match")
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  results <- eventReactive(input$simulate, {
    if(input$simulate > 0) {
      predicted_results <- simulate_match(input$home_formation, input$away_formation, input$competition) * 100
      names(predicted_results) <- c("Away Win", "Draw", "Home Win")
      return(predicted_results)
    }
  })
  
  output$prediction <- renderText({
    req(results())
    paste("Predicted Match Outcomes:", 
          sprintf("Away Win: %.2f%%, Draw: %.2f%%, Home Win: %.2f%%", results()["Away Win"], results()["Draw"], results()["Home Win"]))
  })
}

shinyApp(ui = ui, server = server)