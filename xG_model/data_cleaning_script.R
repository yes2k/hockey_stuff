# This script is used to clean the "raw" hockey data
# to be used in our xG Model
library(tidyverse)
source("xG_model/EH_scrape_functions.R")
source("data/loading_data_script.R")
download_hockey_data()
path_to_data <- "data"

# Takes pbp data from the scraper and transforms
# to be used by our xG Model
data_pipeline_xG <- function(pbp){
  new_pbp <- pbp %>% filter(event_type %in% c("SHOT", "GOAL", "MISS")) %>%
    mutate(GF = home_score,
           GA = away_score,
           num_home_players = str_extract(game_strength_state, "^."),
           num_away_players = str_extract(game_strength_state, ".$"),
           num_home_players = ifelse(num_home_players == "E", 
                                     ifelse(game_seconds < 3600, "5", "3"), 
                                     num_home_players),
           num_away_players = ifelse(num_away_players == "E", 
                                     ifelse(game_seconds < 3600, "5", "3"),
                                     num_away_players),
           num_home_players = as.numeric(num_home_players),
           num_away_players = as.numeric(num_away_players),
           goalie = ifelse(event_team == home_team, home_goalie, away_goalie),
           shooter = event_player_1,
           y = coords_y,
           x = abs(coords_x),
           shot_distance = sqrt((89-x)^2 + y^2),
           shot_angle = atan2(abs(y), abs(89-x)),
           is_home_team = if_else(event_team == home_team, 1, 0),
           seconds_since_last_shot = game_seconds - lag(game_seconds),
           is_backhand = 1*(event_detail == "Backhand"),
           is_deflected = 1*(event_detail == "Deflected"),
           is_slap = 1*(event_detail == "Slap"),
           is_snap = 1*(event_detail == "Snap"),
           is_tipin = 1*(event_detail == "Tip-In"),
           is_wraparound = 1*(event_detail == "Wrap-around"),
           is_wrist = 1*(event_detail == "Wrist"),
           isgoal=ifelse(event_type == "GOAL", 1, 0)) %>%
    dplyr::select("GF", "GA", "num_home_players", "num_away_players", 
                  "x", "y", "isgoal", "game_id", "game_seconds",
                  "shooter", "goalie", "shot_distance",
                  "shot_angle", "seconds_since_last_shot",
                  "event_team", "is_backhand", "is_slap",
                  "is_snap", "is_tipin", "is_wraparound", 
                  "is_wrist", "is_home_team") %>% drop_na()
  return(new_pbp)   
}


train_test_split <- function(){
  if(!exists("pbp_data")){
    pbp_data <- read_csv(paste(path_to_data, "/pbp_base.csv", sep=""))
  }
  cleaned_data <- data_pipeline_xG(pbp_data)
  
  nsplit <- round(nrow(cleaned_data)*0.8)
  write_csv(cleaned_data %>% slice(1:nsplit), "xG_model/train.csv")
  write_csv(cleaned_data %>% slice((nsplit+1):n()), "xG_model/test.csv")
}







