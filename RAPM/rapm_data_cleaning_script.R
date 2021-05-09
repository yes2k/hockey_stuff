library(tidyverse)
library(caret)
library(glmnet)
library(data.table)
library(Matrix)
library(furrr)
library(progressr)
source("xG_model/data_cleaning_script.R")

# library(reticulate)
memory.limit(size = 30000) # need to increase the memory limit otherwise 
                           # there won't be enought memory for creating the 
                           # sparse matrix
# Loading in the data
pbp_data <- read_csv("data/pbp_base.csv")
sub_pbp <- pbp_data %>% filter(season == 20182019)

# Getting xG model
xg_model <- readRDS("xG_model/xg_log_reg_model.RDS")

# ====================== Getting the shift and shot data ===========================
old_names1 <- c(paste0("away_on_", 1:7), paste0("home_on_", 1:7),
                "home_team", "away_team", "shift_start", "shift_end",
                "home_team_shots_per_60", "game_id", "home_score", "away_score",
                "home_team_xg_per_60")
new_names1 <- c(paste0("def_player_", 1:7), paste0("off_player_", 1:7),
                "home_team", "away_team", "shift_start", "shift_end",
                "shots_per_60", "game_id", "home_score", "away_score",
                "xg_per_60")

old_names2 <- c(paste0("away_on_", 1:7), paste0("home_on_", 1:7),
                "home_team", "away_team", "shift_start", "shift_end",
                "away_team_shots_per_60", "game_id", "home_score", "away_score",
                "away_team_xg_per_60")
new_names2 <- c(paste0("off_player_", 1:7), paste0("def_player_", 1:7),
                "home_team", "away_team", "shift_start", "shift_end",
                "shots_per_60", "game_id", "home_score", "away_score",
                "xg_per_60")

get_rapm_data_per_game <- function(pbp){
  
  # Getting xG data
  xg_data <- data_pipeline_xG(pbp)
  xg_preds <- predict(xg_model, newdata = xg_data %>% 
                        dplyr::select(-c("game_id", "game_seconds")),
                      type = "response" )
  
  final_xg_data <- bind_cols(xg_data %>% dplyr::select("game_id", "game_seconds", 
                                                       "is_home_team"), xG=xg_preds)
  # final_xg_data <- final_xg_data %>% rename(xG = `...4`)
  
  # splitting the data so that each row is a single shift where no on leaves the ice
  shift_data <- pbp %>% filter(event_type == "CHANGE") %>% 
    mutate(is_home_team_event = if_else(event_team == home_team, 
                                        1, 0)) %>% 
    select(game_id, game_seconds, event_team, 
           paste0("away_on_", 1:7), paste0("home_on_", 1:7), 
           is_home_team_event, home_goalie, away_goalie, 
           home_team, away_team, home_score, away_score) %>% 
    group_by(game_seconds) %>% 
    slice(n()) %>% 
    ungroup(game_seconds) %>% 
    mutate(shift_start = game_seconds,
           shift_end = lead(game_seconds)) %>%   
    head(-1) %>% 
    mutate(across(c(home_on_1:home_on_7, away_on_1:away_on_7), 
                  function(x){
                    ifelse(x==home_goalie, NA, 
                           ifelse(x==away_goalie, NA, x))})) 
  
  shift_times <- shift_data[c("shift_start", "shift_end")]
  
  # Getting shot data
  shot_type <- c("BLOCK", "GOAL", "MISS", "SHOT")
  shot_data  <- pbp %>% filter((event_type %in% shot_type) & 
                                 (game_strength_state == "5v5" | 
                                    game_strength_state == "4v4" |
                                    game_strength_state == "3v3")) %>% 
    dplyr::select(game_seconds, event_team, event_type, home_team,
                  away_team) %>% 
    mutate(is_home_team = if_else(event_team == home_team, 1, 0)) %>% 
    inner_join(x=., y=final_xg_data, by = c("game_seconds", "is_home_team"))
  
  # Combinding shot data with shift data
  home_team_shot_times <- shot_data %>% 
    filter(is_home_team == 1) %>% 
    pull(game_seconds)
  
  away_team_shot_times <- shot_data %>% 
    filter(is_home_team == 0) %>% 
    pull(game_seconds)
  
  home_team_xg_times <- shot_data %>% 
    filter(is_home_team == 1) %>% 
    dplyr::select(game_seconds, xG)
  
  away_team_xg_times <- shot_data %>% 
    filter(is_home_team == 0) %>% 
    dplyr::select(game_seconds, xG)
  
  final_shift_data <- apply(shift_times, 1, function(x){
    home_team_shot <- sum(x[["shift_start"]] <= home_team_shot_times &
                            home_team_shot_times < x[["shift_end"]])
    away_team_shot <- sum(x[["shift_start"]] <= away_team_shot_times &
                            away_team_shot_times < x[["shift_end"]])
    # home_team_xg <- 0
    # away_team_xg <- 0
    home_team_xg <- home_team_xg_times %>% filter(x[["shift_start"]] <= game_seconds &
                                                    game_seconds < x[["shift_end"]]) %>%
                    pull(xG) %>% sum()
    away_team_xg <- away_team_xg_times %>% filter(x[["shift_start"]] <= game_seconds &
                                                    game_seconds < x[["shift_end"]]) %>%
                    pull(xG) %>% sum()

    return(tibble(home_team_shots = home_team_shot, 
                  away_team_shots = away_team_shot,
                  home_team_xg = home_team_xg,
                  away_team_xg = away_team_xg))
  }) %>% bind_rows(.) %>% bind_cols(., shift_data) %>% 
    mutate(home_team_shots_per_60 = (home_team_shots)*(3600/(shift_end - shift_start)),
           away_team_shots_per_60 = (away_team_shots)*(3600/(shift_end - shift_start)),
           home_team_xg_per_60 = (home_team_xg)*(3600/(shift_end - shift_start)),
           away_team_xg_per_60 = (away_team_xg)*(3600/(shift_end - shift_start)))
  
  # Double each row and make columns for offensive players and defensive players
  # and also create a dummy variable that is 1 if home team == offensive team
  home_team_data <- final_shift_data %>%
    dplyr::select(all_of(old_names1)) %>%
    rename_with(~ new_names1[which(old_names1 == .x)], .cols = old_names1) %>% 
    mutate(off_is_home = 1, score_state = home_score - away_score)
  
  away_team_data <- final_shift_data %>% 
    dplyr::select(all_of(old_names2)) %>%
    rename_with(~ new_names2[which(old_names2 == .x)], .cols = old_names2) %>% 
    mutate(off_is_home = 0, score_state = away_score - home_score)
  
  bind_rows(home_team_data, away_team_data) %>% mutate(id=1:n()) %>%
    arrange(shift_start)
}

# d <- sub_pbp %>% group_by(game_id) %>% 
#   group_map(function(x, y){get_rapm_data_per_game(x)}, .keep=TRUE) %>% 
#   bind_rows(.)

plan(multisession, workers = 5)
d <- sub_pbp %>% 
  group_by(game_id) %>% group_split() %>% 
  future_map_dfr(get_rapm_data_per_game, .progress = TRUE)


# ====================== Multi hot encoding the players ===========================
# Multi hot encoding the player variables
off_dummy <- d %>% dplyr::select(c(paste0("off_player_", 1:7), "id", "game_id")) %>% setDT()
off_dummy <- dcast(setDT(melt(off_dummy,id.vars = c("id", "game_id")))[,ind:=1], id+game_id~value, 
                   value.var = "ind",fill=0) %>%
  as_tibble() %>% dplyr::select(-"NA")

colnames(off_dummy) <- c("id", "game_id", paste0("off_",colnames(off_dummy)[3:length(off_dummy)])) 


def_dummy <- d %>% dplyr::select(c(paste0("def_player_", 1:7), "id", "game_id")) %>% setDT()
def_dummy <- dcast(setDT(melt(def_dummy,id.vars = c("id", "game_id")))[,ind:=1], id+game_id~value, 
                   value.var = "ind",fill=0) %>%
  as_tibble() %>% dplyr::select(-"NA")

colnames(def_dummy) <- c("id", "game_id", paste0("def_",colnames(def_dummy)[3:length(def_dummy)])) 


final_data <- bind_cols(off_dummy %>% dplyr::select(-c("id", "game_id")), def_dummy, 
                        d %>% dplyr::select(home_team, away_team, shift_start, 
                                            shift_end, shots_per_60, off_is_home, xg_per_60))

# ====================== Writing the data ===========================
saveRDS(final_data, "RAPM/final_data.RDS")

# removing unnesesary variables
rm("final_data", "off_dummy", "def_dummy", "d", "final_data")
