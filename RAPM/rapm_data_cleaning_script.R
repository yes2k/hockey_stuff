library(tidyverse)
library(lubridate)
library(reshape2)
source("xG_model/EH_scrape_functions.R")
source("data/loading_data_script.R")
# download_hockey_data()
path_to_data <- "data"

pbp_data <- read_csv(paste0(path_to_data, "/pbp_base.csv"))
player_shifts <- read_csv(paste0(path_to_data, "/player_shifts.csv"))

gid <- 2017020265
sub_pbp <- pbp_data %>% filter(game_id == gid)
sub_player_shifts <- player_shifts %>% filter(game_id == gid)
sub_pbp %>% select(game_seconds, home_on_1, event_type, event_detail) 
sub_player_shifts %>% 
  filter(position != "G") %>% 
  select(seconds_start, seconds_end, player, is_home) 
