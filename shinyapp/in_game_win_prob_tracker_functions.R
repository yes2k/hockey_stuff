library(shiny)
library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)
source("EH_scrape_functions.R")

model <- readRDS("ingame_win_prob_model.RDS")

data_pipeline <- function(pbp){
  cols_to_select <- c("game_id", "game_seconds","home_team", "away_team",
                      "home_score", "away_score", "game_strength_state", "event_type")
  
  events_to_remove <- c("GSTART", "GEND", "PSTART", "PEND")
  
  new_pbp <- pbp %>% 
    dplyr::filter(game_seconds > 0 & !event_type %in% events_to_remove) %>% 
    dplyr::select(all_of(cols_to_select)) %>%
    mutate(GF = home_score) %>%
    mutate(GA = away_score) %>%
    mutate(num_home_players = str_extract(game_strength_state, "^.")) %>%
    mutate(num_away_players = str_extract(game_strength_state, ".$")) %>%
    mutate(num_home_players = ifelse(num_home_players == "E", 
                                     ifelse(game_seconds < 3600, "5", "3"), 
                                     num_home_players)) %>%
    mutate(num_away_players = ifelse(num_away_players == "E", 
                                     ifelse(game_seconds < 3600, "5", "3"),
                                     num_away_players)) %>%
    mutate(num_home_players = as.numeric(num_home_players)) %>%
    mutate(num_away_players = as.numeric(num_away_players))

  new_pbp <- new_pbp %>% dplyr::select(-c("home_team", "away_team", 
                                          "event_type", "game_strength_state",
                                          "home_score", "away_score"))
  new_pbp <- type_convert(new_pbp) 
}

rt_probs_graph <- function(model, pbp, game_info){
  new_pbp <- data_pipeline(pbp)
  prob_home_team_winging <- predict(model, newdata = new_pbp %>% dplyr::select(-c("game_id")), type="response")
  new_pbp <- cbind(new_pbp, prob_home_team_winging)
  p <- ggplot(new_pbp, aes(x=game_seconds, y=prob_home_team_winging)) + 
          geom_line() + ggtitle(paste("In Game Win Probability Tracker ", game_info$away_team, " @ ", 
                                      game_info$home_team, ", ", game_info$game_date, sep="")) +
          theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

rt_probs_graph_for_id <- function(gid){
  game_data <- sc.scrape_pbp(as.character(gid))
  probs_graph <- rt_probs_graph(model, game_data$pbp_base, game_data$game_info)
  return(probs_graph)
}