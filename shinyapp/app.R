source("in_game_win_prob_tracker_functions.R")


ui <- fluidPage(
  
  # App title ----
  headerPanel("Live Win Probablility Tracker"),
  
  # Sidebar panel for inputs ---- 
  sidebarPanel(
    helpText("This is a work in Progress. If you would like to use 
             it, input the game id you are looking for into the
             textbox and the in game probabilty graph will appear to 
             the right"),
    numericInput("game_id", "Write game id", 2019020809)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("live_prob_graph")
  )
)

server <- function(input, output){
  output$live_prob_graph <- renderPlot({
    rt_probs_graph_for_id(input$game_id)
  })
}

shinyApp(ui, server)