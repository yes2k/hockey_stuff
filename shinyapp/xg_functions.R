source("EH_scrape_functions.R")

xg_model <- readRDS("xg_log_reg_model.RDS")

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
                  "is_wrist", "is_home_team")
  return(new_pbp)   
}


xG_graph <- function(gid){
  game_data <- sc.scrape_pbp(as.character(gid), live_scrape = TRUE)
  
  shot_data <- data_pipeline_xG(game_data$pbp_base)
  game_info <- game_data$game_info

  preds <- predict(xg_model, newdata = shot_data, type = "response")
  xG_data <- cbind(xG = preds, shot_data) %>% as_tibble(.)
  plot_data <- xG_data %>% 
    dplyr::select("xG", "game_seconds", "event_team") %>% 
    mutate(row = row_number()) %>%
    pivot_wider(names_from = event_team, values_from = xG) %>%
    replace(is.na(.), 0) %>% dplyr::select(-row)
  
  home_team <- game_info$home_team
  away_team <- game_info$away_team
  plot_data[home_team] <- cumsum(plot_data[home_team])
  plot_data[away_team] <- cumsum(plot_data[away_team])
  plot_data <- plot_data %>% 
    pivot_longer(cols=-c(game_seconds), values_to="Cumulative xG")
  
  return(ggplot(data = plot_data, aes(x=game_seconds/60, y=`Cumulative xG`, col=name)) +
           geom_line() + 
           ggtitle(paste("Cumulative xG ", game_info$away_team, " @ ", 
                         game_info$home_team, ", ", game_info$game_date, sep="")) + 
           theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_continuous(name="Game Minutes", breaks = seq(0, 65, 10)) +
    theme_classic() +  guides(col=guide_legend(title="Teams"))
    )
}
