source("in_game_win_prob_tracker_functions.R")
source("xg_functions.R")

ui <- fluidPage(
  
  # App title ----
  headerPanel("Game "),
  
  # Sidebar panel for inputs ---- 
  sidebarPanel(
    helpText("This is a work in Progress. If you would like to use 
             it, input the game id you are looking for into the
             textbox and the in game probabilty graph will appear to 
             the right"),
    dateInput("date", "Select Date"),
    uiOutput("game_selection"),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Cumulative xG plot", 
                         plotOutput("xg_plot") %>% withSpinner()),
                tabPanel("Win Probability Plot", 
                         plotOutput("prob_graph") %>% withSpinner())
                )
  )
)

server <- function(input, output){
  obsList <- list()
  
  # Initial empty plot so that the loading icon doesn't continuously show
  output$xg_plot <- renderPlot({plot.new()})
  output$prob_graph <- renderPlot({plot.new()})
  
  output[["game_selection"]] <- renderUI({
    obsList <<- list()
    d <- input[["date"]]
    sc.scrape_schedule(start_date = d, 
                       end_date = d, print_sched = FALSE) %>% 
      dplyr::select(game_id, home_team, away_team) %>% 
      as.list(.) %>% pmap(function(game_id, home_team, away_team){
        btn_id <- paste0("game_selection_button_", game_id)
        if(is.null(obsList[[btn_id]])){
          obsList[[btn_id]] <<- observeEvent(input[[btn_id]],{
            output$xg_plot <- renderPlot({xG_graph(game_id)})
            output$prob_graph <- renderPlot({rt_probs_graph_for_id(game_id)})
          })
          actionButton(btn_id, paste(away_team, "@", home_team))
        }
      }) 
  })
}

shinyApp(ui, server)