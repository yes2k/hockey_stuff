library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)
source("EH_scrape_functions.R")

files_to_create <- c("game_info_df", "pbp_base", "pbp_extras",
                     "player_shifts", "player_periods", "roster_df",
                     "scratches_df", "events_summary_df", "report")

for(file in paste("data/", files_to_create, ".csv", sep="")){
  if(!file.exists(file)){
    file.create(file)
  }
}


# TODO: scrape from game 483 and onwards
# for (i in 2:1271){
#   print(i)
#   game_id <- paste("201802", sprintf("%04d", i), sep="")
#   game_data <- sc.scrape_pbp(game_id)
#   for(file in files_to_create){
#     write.table(game_data[[file]], paste("data/", file, ".csv", sep=""), sep=",", 
#               col.names=!file.exists(paste("data/", file, ".csv", sep="")), append=TRUE, row.names=FALSE)
#   }
# }

# Loading data
events_summary <- read_csv("data/events_summary_df.csv")
game_info <- read_csv("data/game_info_df.csv")
pbp_base <- read_csv("data/pbp_base.csv")
pbp_extras <- read_csv("data/pbp_extras.csv")
player_periods <- read_csv("data/player_periods.csv")
player_shifts <- read_csv("data/player_shifts.csv")
report <- read_csv("data/report.csv")
roster <- read_csv("data/roster_df.csv")
scratches <- read_csv("data/scratches_df.csv")

# ===== Looking at where penalties occur on the ice =====
# penalty_positions <- pbp_base %>% filter(event_type == "PENL") %>% 
#                 select(coords_x, coords_y, event_detail, event_description) %>%
#                 extract(col=event_description, regex="([A-Z]+[a-z]+\\(|TEAM.*\\()", into="penalty_type")
#                 # extract(col = event_description, regex = "([A-Z]*.*\\()", into="penalty_type")
#                               
# ggplot(penalty_positions %>% filter(penalty_type == "Interference("), aes(x=coords_y, y=coords_x, col=penalty_type)) + geom_point()

g <- 2018020001
test <- pbp_base %>% filter(game_id == g)
shots <- test %>% filter(event_type == "SHOT")

ggplot(shots, aes(x=coords_y, y=coords_x, col=event_team)) + geom_point()
# load(file = "test_2018_2019_season.rds")
# all_game_ids <- test_2018_2019_season$game_info_df %>% select(game_id) %>% "[["(1) %>% as.numeric()
# 
# 
# ## Geting all the game ids between start_date and end_date
# ## returns a list of teams along with the game ids they were in
# getting_team_game_ids <- function(start_date = Sys.Date(), end_date = Sys.Date()){
#   game_ids <- list(
#     "ANA" = character(), "ARI" = character(), "BOS" = character(), "BUF" = character(), "CAR" = character(),
#     "CBJ" = character(), "CGY" = character(), "CHI" = character(), "COL" = character(), "DAL" = character(), 
#     "DET" = character(), "EDM" = character(), "FLA" = character(), "L.A" = character(), "MIN" = character(), 
#     "MTL" = character(), "N.J" = character(), "NSH" = character(), "NYI" = character(), "NYR" = character(), 
#     "OTT" = character(), "PHI" = character(), "PIT" = character(), "S.J" = character(), "STL" = character(), 
#     "T.B" = character(), "TOR" = character(), "VAN" = character(), "WPG" = character(), "WSH" = character(), 
#     "VGK" = character()
#   )
#   
#   sched <- sc.scrape_schedule(start_date = start_date, end_date = end_date, print_sched=FALSE)
#   for(team in names(game_ids)){
#     game_ids[team] <- sched %>% filter(away_team == team | home_team == team) %>%
#                               select(game_id)
#   }
#   return(game_ids)
# }
# 
# ## Getting the last n game id's for the team playing in curr_game_id
# last_n_games <- function(curr_game_id, nhl_data, n){
#   start_date <- min(nhl_data$game_info_df$game_date)
#   end_date <- max(nhl_data$game_info_df$game_date)
#   
#   team_game_ids <- getting_team_game_ids(start_date = start_date, end_date = end_date)
#   
#   curr_game_home_team <- nhl_data$game_info_df %>% filter(game_id == curr_game_id) %>% select(home_team) %>% '[['(1)
#   curr_game_away_team <- nhl_data$game_info_df %>% filter(game_id == curr_game_id) %>% select(away_team) %>% '[['(1)
#   
#   home_last_n_game_ids <-  as.integer(team_game_ids[[curr_game_home_team]])[as.integer(
#                                 team_game_ids[[curr_game_home_team]]) < curr_game_id]
#   
#   away_last_n_game_ids <-  as.integer(team_game_ids[[curr_game_away_team]])[as.integer(
#                                 team_game_ids[[curr_game_away_team]]) < curr_game_id]
#   
#   if(length(home_last_n_game_ids) >= 4 && length(away_last_n_game_ids) >= 4){
#     home_last_n_game_ids <- home_last_n_game_ids[(length(home_last_n_game_ids) - n + 1):length(home_last_n_game_ids)]
#     away_last_n_game_ids <- away_last_n_game_ids[(length(away_last_n_game_ids) - n + 1):length(away_last_n_game_ids)]
#     
#     return_list <- list(as.numeric(home_last_n_game_ids), as.numeric(away_last_n_game_ids))
#     names(return_list) <- c(curr_game_home_team, curr_game_away_team)
#     return(return_list) 
#   }else{
#     return(0)
#   }
# }
# 
# getting_vars <- function(curr_game_id, nhl_data, n){
#   home_t <- nhl_data$game_info_df %>% filter(game_id == curr_game_id) %>% select(home_team) %>% '[['(1)
#   away_t <- nhl_data$game_info_df %>% filter(game_id == curr_game_id) %>% select(away_team) %>% '[['(1) 
#   
#   home_win_team <- (nhl_data$game_info_df %>% filter(game_id == curr_game_id) %>%
#       mutate(winning_team = ifelse(home_score > away_score, home_team, away_team)) %>%
#       select(winning_team) %>% "[["(1)) == home_t
#   
#   print(paste("home team:", home_t))
#   print(paste("away team:", away_t))
#   
#   last_n_game_ids <- last_n_games(curr_game_id, nhl_data, n)
#   
#   home_last_n_ids <- last_n_game_ids[[home_t]]
#   away_last_n_ids <- last_n_game_ids[[away_t]]
#   
#   # Win record for home team for last n games
#   home_team_win_record_last_n <- as.integer((nhl_data$game_info_df %>% filter(game_id %in% home_last_n_ids) %>%
#                                         mutate(winning_team = ifelse(home_score > away_score, home_team, away_team)) %>%
#                                         select(winning_team) %>% "[["(1)) == home_t)
#   
#   # Wind record for away team for last n games
#   away_team_win_record_last_n <- as.integer((nhl_data$game_info_df %>% filter(game_id %in% away_last_n_ids) %>%
#                                         mutate(winning_team = ifelse(home_score > away_score, home_team, away_team)) %>%
#                                         select(winning_team) %>% "[["(1)) == away_t)
#   
#   # GA for home team for last n games
#   home_team_ga_last_n <- nhl_data$game_info %>% filter(game_id %in% home_last_n_ids) %>%
#           mutate(GA = ifelse(home_team == home_t, away_score, home_score)) %>%
#           select(GA) %>% "[["(1)
#   
#   # GF for home team for last n games 
#   home_team_gf_last_n <- nhl_data$game_info %>% filter(game_id %in% home_last_n_ids) %>%
#           mutate(GF = ifelse(home_team == home_t, home_score, away_score)) %>%
#           select(GF) %>% "[["(1)
#   
#   # GA for away team for last n games
#   away_team_ga_last_n <- nhl_data$game_info %>% filter(game_id %in% away_last_n_ids) %>%
#           mutate(GA = ifelse(home_team == away_t, away_score, home_score)) %>%
#           select(GA) %>% "[["(1)
#   
#   # GF for away team for last n games
#   away_team_gf_last_n <- nhl_data$game_info %>% filter(game_id %in% away_last_n_ids) %>%
#           mutate(GF = ifelse(home_team == away_t, home_score, away_score)) %>%
#           select(GF) %>% "[["(1)
#   
#   return(c(home_team_win_record_last_n, home_team_ga_last_n, home_team_gf_last_n, away_team_win_record_last_n, 
#         away_team_ga_last_n, away_team_gf_last_n, home_win_team))
# }
# 
# 
# num_games_lag <- 4
# data <- matrix(,ncol= 4*6 + 1, nrow = 0)
# 
# for(x in all_game_ids){
#   g <- last_n_games(x, test_2018_2019_season, num_games_lag)
#   if(is.list(g)){
#     print(x)
#     vars <- getting_vars(x, test_2018_2019_season, num_games_lag)
#     data <- rbind(data, vars)
#   }
# }
# 
# name<- c("home_team_win-4", "home_team_win-3", "home_team_win-2", "home_team_win-1", 
#                  "home_team-ga-4", "home_team-ga-3", "home_team-ga-2", "home_team-ga-1",
#                  "home_team-gf-4", "home_team-gf-3","home_team-gf-2", "home_team-gf-1",
#                  "away_team_win-4", "away_team_win-3", "away_team_win-2", "away_team_win-1", 
#                  "away_team-ga-4", "away_team-ga-3", "away_team-ga-2", "away_team-ga-1",
#                  "away_team-gf-4", "away_team-gf-3","away_team-gf-2", "away_team-gf-1",
#                  "home_team_win")
# 
# df <- data.frame(data)
# names(df) <-name
# 

