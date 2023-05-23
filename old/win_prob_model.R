library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)
library(googledrive)
library(arm)
library(caret)
library(glmnet)
# source("EH_scrape_functions.R")
# files_to_create <- c("game_info_df", "pbp_base", "pbp_extras",
#                      "player_shifts", "player_periods", "roster_df",
#                      "scratches_df", "events_summary_df", "report")
# 
# # TODO: scrape from game 483 and onwards
# for (i in 483:1271){
#   print(i)
#   game_id <- paste("201802", sprintf("%04d", i), sep="")
#   game_data <- sc.scrape_pbp(game_id)
#   for(file in files_to_create){
#     write.table(game_data[[file]], paste("data/", file, ".csv", sep=""), sep=",",
#               col.names=!file.exists(paste("data/", file, ".csv", sep="")), append=TRUE, row.names=FALSE)
#   }
# }

# Loading data
# events_summary_id <- "1Unfegmn3_LfxFLsCWRFR1wphiDz-saP2"
# game_info_id <- "1FHg7g0v7TUwFLyQXeKNBBduF4PvpXEgQ"
# pbp_base_id <- "1i_Eflfc4Mc18mgoxAi_yyRN4lI1VBa3W"
# pbp_extras_id <- "1dZZKYv-ZD_vC0OF-fy4UTJ_gAn4Waf5N"
# player_periods_id <- "1hf4XO5Z5hcyx-USryUaUvagmwDpoYuJU"
# player_shifts_id <- "1PfGd6homdvVD63nf8nd9VsRQhTykawjH"
# report_id <- "1-gyamQ0ot5HCXgTNdTydy0Fu_35kp7-i"
# roster_id <- "1Uqsmeq9rKZWIVut97Yyw0YB-36rCFdWM"
# scratches_id <- "1hMqObiOABAWGwthXTGdJIY0OtniQRQ9A"
# 
# drive_download(as_id(events_summary_id), overwrite = TRUE)
# drive_download(as_id(game_info_id), overwrite = TRUE)
# drive_download(as_id(pbp_base_id), overwrite = TRUE)
# drive_download(as_id(pbp_extras_id), overwrite = TRUE)
# drive_download(as_id(player_periods_id), overwrite = TRUE)
# drive_download(as_id(player_shifts_id), overwrite = TRUE)
# drive_download(as_id(report_id), overwrite = TRUE)
# drive_download(as_id(roster_id), overwrite = TRUE)
# drive_download(as_id(scratches_id), overwrite = TRUE)

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

all_game_ids <- game_info$game_id


# Modeling Pr(home team winning), all stats are from the point of view of the home team
# ev_shots_for_per_60 <-((pbp_base %>% group_by(game_id) %>% filter((event_type == "SHOT" | event_type == "GOAL") & game_strength_state == "5v5" & event_team == home_team) %>% 
#                               summarise(home_ev_num_shots_for=n()) %>% dplyr::select(home_ev_num_shots_for)) / 
#                               ((pbp_base %>% filter(event_type == "GEND") %>% dplyr::select(game_seconds)) / 3600)) %>%
#                               cbind(game_info$game_id, .)
# colnames(ev_shots_for_per_60) <- c("game_id", "ev_shots_for_per_60")
# ev_shots_for_per_60 <- ev_shots_for_per_60 %>% as_tibble()
# 
# ev_shots_against_per_60 <- ((pbp_base %>% group_by(game_id) %>% filter((event_type == "SHOT" | event_type == "GOAL") & game_strength_state == "5v5" & event_team == away_team) %>% 
#                                     summarise(home_ev_num_shots_for=n()) %>% dplyr::select(home_ev_num_shots_for)) / 
#                                     ((pbp_base %>% filter(event_type == "GEND") %>% dplyr::select(game_seconds)) / 3600)) %>%
#                                     cbind(game_info$game_id, .)
# colnames(ev_shots_against_per_60) <- c("game_id", "ev_shots_against_per_60")
# ev_shots_against_per_60 <- ev_shots_against_per_60 %>% as_tibble()
# 
# 
# # Powerplay shots for per 60
# powerplay_time <- pbp_base %>% group_by(game_id) %>% filter(event_type == "PENL" & event_team == away_team) %>% 
#                       extract(col=event_detail, regex="(^[0-9][0-9]?)", into="penalty_length") %>% 
#                       mutate(penalty_length = as.numeric(penalty_length)) %>% 
#                       summarise(penalty_time = sum(penalty_length))
# 
# powerplay_shots_for <- (pbp_base %>% group_by(game_id) %>% filter((event_type == "SHOT" | event_type == "GOAL") & game_strength_state == "5v4" & event_team == home_team) %>%
#                                  summarise(powerplay_shots_for=n()))
# 
# # Adding in missing games that have 0 shots or 0 minutes of powerplay time
# for(g in all_game_ids){
#   if(!(g %in% powerplay_time$game_id)){
#     print(g)
#     powerplay_time <- powerplay_time %>% add_row(game_id=g, penalty_time=0)
#   }
#   if(!(g %in% powerplay_shots_for$game_id)){
#     print(g)
#     powerplay_shots_for <- powerplay_shots_for %>% add_row(game_id=g, powerplay_shots_for=0)
#   }
# }
# powerplay_shots_for <- powerplay_shots_for %>% arrange(game_id)
# powerplay_time <- powerplay_time %>% arrange(game_id)
# 
# pp_shots_for_per_60 <- cbind(powerplay_time$game_id, powerplay_shots_for$powerplay_shots_for / (powerplay_time$penalty_time / 60)) %>%
#                         data.frame(.)
# colnames(pp_shots_for_per_60) <- c("game_id", "pp_shots_per_60")
# pp_shots_for_per_60 <- pp_shots_for_per_60 %>% as_tibble()
# 
# pp_shots_for_per_60 <- pp_shots_for_per_60 %>% replace_na(list(game_id=0, pp_shots_per_60=0))
# 
# 
# # Penalty kill shots against per 60
# pk_time <- pbp_base %>% group_by(game_id) %>% filter(event_type == "PENL" & event_team == home_team) %>% 
#                     extract(col=event_detail, regex="(^[0-9][0-9]?)", into="pk_length") %>% 
#                     mutate(pk_length = as.numeric(pk_length)) %>% 
#                     summarise(pk_time = sum(pk_length))
# 
# pk_shots_against <- (pbp_base %>% group_by(game_id) %>% filter((event_type == "SHOT" | event_type == "GOAL") & game_strength_state == "5v4" & event_team == away_team) %>%
#                           summarise(pk_shots_against=n()))
# 
# 
# # Adding in missing games that have 0 shots or 0 minutes of pk time
# for(g in all_game_ids){
#   if(!(g %in% pk_time$game_id)){
#     print(g)
#     pk_time <- pk_time %>% add_row(game_id=g, pk_time=0)
#   }
#   if(!(g %in% pk_shots_against$game_id)){
#     print(g)
#     pk_shots_against <- pk_shots_against %>% add_row(game_id=g, pk_shots_against=0)
#   }
# }
# pk_time <- pk_time %>% arrange(game_id)
# pk_shots_against <- pk_shots_against %>% arrange(game_id)
# 
# pk_shots_against_per_60 <- cbind(pk_time$game_id, pk_shots_against$pk_shots_against/(pk_time$pk_time / 60)) %>%
#                         data.frame(.)
# colnames(pk_shots_against_per_60) <- c("game_id", "pk_shots_against_per_60")
# pk_shots_against_per_60 <- pk_shots_against_per_60 %>% as_tibble()
# pk_shots_against_per_60 <- pk_shots_against_per_60 %>% replace_na(list(game_id=0, pk_shots_against_per_60=0))
# 
# 
# # Getting save percentage for both home and away teams
# sv_for <- pbp_base %>% group_by(game_id) %>% filter((event_type=="SHOT" | event_type=="GOAL") & event_team==away_team) %>%
#                   count(event_type) %>% pivot_wider(names_from=event_type, values_from=n) %>% replace(., is.na(.), 0) %>%
#                   mutate(sv_for_percentage=SHOT/(SHOT+GOAL)) %>% dplyr::select(game_id, sv_for_percentage)
# 
# sv_against <- pbp_base %>% group_by(game_id) %>% filter((event_type=="SHOT" | event_type=="GOAL") & event_team==home_team) %>%
#                   count(event_type) %>% pivot_wider(names_from=event_type, values_from=n) %>% replace(., is.na(.), 0) %>%
#                   mutate(sv_against_percentage=SHOT/(SHOT+GOAL)) %>% dplyr::select(game_id, sv_against_percentage)


# ========= Getting dummy variables for each player, home and away ===========
players_in_game <- player_periods %>% group_by(game_id) %>% dplyr::select(player, is_home, game_id) %>% unique() 

# 1 is home, 0 is away, NA is didn't play in game
dummy_players_in_game <- players_in_game %>% pivot_wider(names_from = player, values_from = is_home)

player_dummy_home <- dummy_players_in_game %>% mutate_at(vars(-group_cols()), function(x){ifelse(x == 0 || is.na(x), 0, 1)})
player_dummy_away <- dummy_players_in_game %>% mutate_at(vars(-group_cols()), function(x){ifelse(x == 1 || is.na(x), 0, 1)})

player_names <- colnames(dummy_players_in_game)[2:length(colnames(dummy_players_in_game))]

colnames(player_dummy_home) <- c("game_id", paste("home.", player_names, sep=""))
colnames(player_dummy_away) <- c("game_id", paste("away.", player_names, sep=""))

dummy_players <- inner_join(player_dummy_away, player_dummy_home, by="game_id")

y <- game_info %>% mutate(home_team_win=ifelse(home_score>away_score, 1, 0)) %>% dplyr::select(game_id, home_team_win)

# data_list <- list(ev_shots_for_per_60, ev_shots_against_per_60, pp_shots_for_per_60, pk_shots_against_per_60, 
#                   sv_for, sv_against, y, dummy_players)

df <- inner_join(y, dummy_players, by="game_id")
# for (i in 3:length(data_list)){
#   print(i)
#   df <- dplyr::inner_join(df, data_list[[i]], by="game_id")
# }

model <- cv.glmnet(df %>% dplyr::select(-c("game_id", "home_team_win")) %>% data.matrix(.), df %>% dplyr::pull("home_team_win"), alpha=0, family = "binomial")
model_coef <- coef(model, s="lambda.min")
model_coef <- as_tibble(data.frame(name = model_coef@Dimnames[[1]][model_coef@i + 1], coefficient = model_coef@x))

