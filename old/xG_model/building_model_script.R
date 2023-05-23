library(tidyverse)
library(caret)
library(xgboost)

train <- read_csv("xG_model/train.csv")
path_to_model <- "xG_model"

if(!file.exists("xG_model/xg_log_reg_model.RDS")){
  xg_log_reg_model <- glm(isgoal~GF+GA+num_home_players+num_away_players+x+y+shot_distance+
                            shot_angle+seconds_since_last_shot+is_backhand+is_slap+
                            is_snap+is_tipin+is_wraparound+is_wrist, data=train, family="binomial")
  saveRDS(xg_log_reg_model, paste0(path_to_model,"/","xg_log_reg_model.RDS"))
} else {
  xg_log_reg_model <- readRDS(paste0(path_to_model,"/","xg_log_reg_model.RDS"))
}
