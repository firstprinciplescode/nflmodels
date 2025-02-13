library(nflreadr)
library(nflfastR)
library(sqldf)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(flexclust)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)

setwd("C:/Users/vflre/Downloads/NFL Models")

### BEFORE - NO pbpICIPATION / PERSONNEL INFO

pbp_nfl <- load_pbp(c(2016:2024))

'%ni%' <- Negate('%in%')

# LET'S KEEP THINGS SEPARATE UNTIL THE END TO AVOID CROSS-CONTAMINATION OF INFO

pbp_nfl <- pbp_nfl %>% mutate(mod_ydstogo = ifelse(goal_to_go == 1, yardline_100, ydstogo))
pbp_nfl <- pbp_nfl %>% mutate(lead_timeout = lag(timeout_team, 1), last_timeout_ind = case_when(lead_timeout == posteam ~ 1, is.na(lead_timeout) ~ 0, (lead_timeout != posteam & !is.na(lead_timeout)) ~ -1))
pbp_nfl$td_side = ifelse((!is.na(pbp_nfl$td_team) & pbp_nfl$posteam == pbp_nfl$td_team), 1, 0)
pbp_nfl <- pbp_nfl %>% mutate(posteam_ind = ifelse(home_team == posteam, 1, 0))

pbp_nfl$week <- ifelse(pbp_nfl$week == 18 & pbp_nfl$season <= 2020, 28, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 19 & pbp_nfl$season <= 2020, 29, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 19 & pbp_nfl$season > 2020, 28, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 20 & pbp_nfl$season <= 2020, 30, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 20 & pbp_nfl$season > 2020, 29, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 21 & pbp_nfl$season <= 2020, 32, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 21 & pbp_nfl$season > 2020, 30, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 22, 32, pbp_nfl$week)

pbp_nfl$posteam[which(pbp_nfl$posteam == "ARI")] = "ARZ"
pbp_nfl$posteam[which(pbp_nfl$posteam == "BAL")] = "BLT"
pbp_nfl$posteam[which(pbp_nfl$posteam == "CLE")] = "CLV"
pbp_nfl$posteam[which(pbp_nfl$posteam == "HOU")] = "HST"
pbp_nfl$posteam[which(pbp_nfl$posteam == "SD")] = "LAC"
pbp_nfl$posteam[which(pbp_nfl$posteam == "OAK")] = "LV"

pbp_nfl$defteam[which(pbp_nfl$defteam == "ARI")] = "ARZ"
pbp_nfl$defteam[which(pbp_nfl$defteam == "BAL")] = "BLT"
pbp_nfl$defteam[which(pbp_nfl$defteam == "CLE")] = "CLV"
pbp_nfl$defteam[which(pbp_nfl$defteam == "HOU")] = "HST"
pbp_nfl$defteam[which(pbp_nfl$defteam == "SD")] = "LAC"
pbp_nfl$defteam[which(pbp_nfl$defteam == "OAK")] = "LV"

pbp_nfl$td_team[which(pbp_nfl$td_team == "ARI")] = "ARZ"
pbp_nfl$td_team[which(pbp_nfl$td_team == "BAL")] = "BLT"
pbp_nfl$td_team[which(pbp_nfl$td_team == "CLE")] = "CLV"
pbp_nfl$td_team[which(pbp_nfl$td_team == "HOU")] = "HST"
pbp_nfl$td_team[which(pbp_nfl$td_team == "SD")] = "LAC"
pbp_nfl$td_team[which(pbp_nfl$td_team == "OAK")] = "LV"

pbp_nfl$posteam[which(pbp_nfl$posteam == "ARI")] = "ARZ"
pbp_nfl$posteam[which(pbp_nfl$posteam == "BAL")] = "BLT"
pbp_nfl$posteam[which(pbp_nfl$posteam == "CLE")] = "CLV"
pbp_nfl$posteam[which(pbp_nfl$posteam == "HOU")] = "HST"
pbp_nfl$posteam[which(pbp_nfl$posteam == "SD")] = "LAC"
pbp_nfl$posteam[which(pbp_nfl$posteam == "OAK")] = "LV"

pbp_nfl$air_yards[which(pbp_nfl$air_yards <= -54)] = 0

pbp_nfl$down_one_ind = ifelse(pbp_nfl$down == 1, 1, 0)
pbp_nfl$down_two_ind = ifelse(pbp_nfl$down == 2, 1, 0)
pbp_nfl$down_three_ind = ifelse(pbp_nfl$down == 3, 1, 0)
pbp_nfl$down_four_ind = ifelse(pbp_nfl$down == 4, 1, 0)

pbp_nfl$distance_to_sticks = pbp_nfl$mod_ydstogo - pbp_nfl$air_yards

pbp_nfl$season_type <- ifelse(pbp_nfl$season_type == "REG", 0, 1)
pbp_nfl$roof <- ifelse(pbp_nfl$roof %in% c('dome', 'closed'), 1, 0)
pbp_nfl$surface <- ifelse(pbp_nfl$surface == "grass", 0, 1)

pbp_nfl <- pbp_nfl %>% filter(play_type %in% c("pass", "run")) %>% filter(special_teams_play == 0 & play_type %ni% c('no_play', 'qb_kneel', 'qb_spike', 'punt', 'field_goal')) %>% filter(!is.na(play_type)) %>% filter(!is.na(posteam)) %>% filter(is.na(two_point_conv_result))

pbp_nfl <- pbp_nfl %>% filter(!grepl("TWO-POINT CONVERSION", desc))

pbp_nfl$season_type <- ifelse(pbp_nfl$season_type == "REG", 0, 1)
pbp_nfl$roof <- ifelse(pbp_nfl$roof %in% c('dome', 'closed'), 1, 0)
pbp_nfl$surface <- ifelse(pbp_nfl$surface == "grass", 0, 1)
pbp_nfl$run_location <- case_when(is.na(pbp_nfl$run_location) ~ 2, pbp_nfl$run_location %in% c("left", "right") ~ 1, pbp_nfl$run_location == "middle" ~ 0)
pbp_nfl$run_gap <- case_when(is.na(pbp_nfl$run_gap) ~ 0, pbp_nfl$run_gap == "guard" ~ 1, pbp_nfl$run_gap == "tackle" ~ 2, pbp_nfl$run_gap == "end" ~ 3)

pbp_nfl$guard_ind = ifelse(pbp_nfl$run_gap == 1, 1, 0)
pbp_nfl$tackle_ind = ifelse(pbp_nfl$run_gap == 2, 1, 0)
pbp_nfl$end_ind = ifelse(pbp_nfl$run_gap == 3, 1, 0)

pbp_nfl$pass_length <- case_when(is.na(pbp_nfl$pass_length) ~ 0, pbp_nfl$pass_length == "short" ~ 1, pbp_nfl$pass_length == "deep" ~ 2)
pbp_nfl$pass_location <- case_when(is.na(pbp_nfl$pass_location) ~ 0, pbp_nfl$pass_location == "middle" ~ 1, pbp_nfl$pass_location %in% c('left', 'right') ~ 2)

pbp_nfl$middle_ind = ifelse(pbp_nfl$pass_location == 1, 1, 0)
pbp_nfl$outside_ind = ifelse(pbp_nfl$pass_location == 2, 1, 0)

pbp_nfl <- pbp_nfl %>% filter(qb_spike == 0 & qb_kneel == 0)
pbp_nfl <- pbp_nfl %>% filter(play_type %in% c("run", "pass"))


pbp_nfl <- pbp_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0)

pbp_nfl$half_ind <- case_when(pbp_nfl$game_half == "Half1" ~ 1, pbp_nfl$game_half == "Half2" ~ 2, pbp_nfl$game_half == "Overtime" ~ 3)


#####
#####
#####


xgb_pbp_before_xtd <- xgb.load('xgb_pbp_before_td.model')

# two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0 & play_type %in% c("pass", "run")
# yardline_100, half_seconds_remaining, mod_ydstogo, posteam_ind, down, shotgun, no_huddle, qb_dropback, score_differential, half_ind, posteam_timeouts_remaining, defteam_timeouts_remaining


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_before_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, mod_ydstogo, posteam_ind, down, shotgun, no_huddle, qb_dropback, score_differential, half_ind, posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_before_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_before_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_before_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_before_xtd), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_before_xtd = ifelse(two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0 & play_type %in% c("pass", "run"), predicted_before_xtd, NA))


pbp_nfl %>%
  filter(!is.na(predicted_before_xtd)) %>%
  mutate(bin = cut(predicted_before_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_after_run_xtd <- xgb.load('xgb_pbp_after_td.model')

# two_point_attempt == 0 & play_type %in% c("run") & rush_attempt == 1 & qb_kneel == 0 & qb_spike == 0
# season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_after_run_xtd <- function(data) {
  X <- data %>% select(
    season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_after_run_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_after_run_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_after_run_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_after_run_xtd), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_after_run_xtd = ifelse(two_point_attempt == 0 & play_type %in% c("run") & rush_attempt == 1 & qb_kneel == 0 & qb_spike == 0 & pass_attempt == 0, predicted_after_run_xtd, NA))


pbp_nfl %>%
  filter(!is.na(predicted_after_run_xtd)) %>%
  mutate(bin = cut(predicted_after_run_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_after_pass_xtd <- xgb.load('xgb_no_part_after_xtd_pass.model')

# play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards)
# yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_after_pass_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_after_pass_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_after_pass_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_after_pass_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_after_pass_xtd), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_after_pass_xtd = ifelse(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards), predicted_after_pass_xtd, NA))


pbp_nfl %>%
  filter(!is.na(predicted_after_pass_xtd)) %>%
  mutate(bin = cut(predicted_after_pass_xtd, breaks = seq(0, 1, by = 0.025), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_xpass_no_weather <- xgb.load('xgb_pbp_xpass_no_weather.model')

# (pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0
# yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_xpass <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_xpass(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_xpass_no_weather, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_xpass <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_xpass), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_xpass = ifelse((pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0, predicted_xpass, NA))


pbp_nfl %>%
  filter(!is.na(predicted_xpass)) %>%
  mutate(bin = cut(predicted_xpass, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(pass_rate = mean(pass_attempt, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_pressure <- xgb.load('xgb_pbp_pressure_no_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & (sack == 0 | pass_attempt == 0))

# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind

# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_pressure <- function(data) {
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_pressure(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_pressure, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_pressure <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_pressure), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_pressure = ifelse((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & (sack == 0 | pass_attempt == 0), predicted_pressure, NA)
  )


#####
#####


xgb_pbp_sack_no_weather <- xgb.load('xgb_pbp_sack_no_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0
# yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential

xgb_pbp_sack_weather <- xgb.load('xgb_pbp_sack_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0
# yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential, temp, wind


# Function to create DMatrix for snap time to throw predictions
create_dmatrix_sack <- function(data, weather_cols = NULL) {
  if (!is.null(weather_cols)) {
    # Filter data to include only rows with weather information
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    # Filter data to include only rows without weather information
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select the appropriate columns
  X <- data %>% select(
    yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential, temp, wind
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make snap time to throw predictions
make_predictions_sack <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_sack(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_pbp_sack_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_sack(part_data)
  predictions_without_weather <- predict(xgb_pbp_sack_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_sack <- NA
  
  # Assign predictions to the new column based on the availability of weather data
  part_data$predicted_sack[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_sack[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_sack <- make_predictions_sack(pbp_nfl)

# Merge predictions back to the original part_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results_sack %>% 
  select(game_id, play_id, predicted_sack), by = c("game_id", "play_id")) %>%
  mutate(predicted_sack = ifelse((play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0), predicted_sack, NA))

pbp_nfl %>%
  filter(!is.na(predicted_sack)) %>%
  mutate(bin = cut(predicted_sack, breaks = seq(0, 1, by = 0.02), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(sack_rate = mean(sack, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_cp <- xgb.load('xgb_pbp_cp_no_weather.model')

# play_type == "pass" & qb_spike == 0 & qb_scramble == 0 & fumble == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & two_point_attempt == 0 & !is.na(air_yards)
# yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, down_one_ind, down_two_ind, down_three_ind, down_four_ind


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_cp <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, down_one_ind, down_two_ind, down_three_ind, down_four_ind
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_cp(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_cp, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_cp <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_cp), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_cp = ifelse(play_type == "pass" & qb_spike == 0 & qb_scramble == 0 & fumble == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & two_point_attempt == 0 & !is.na(air_yards), predicted_cp, NA))


pbp_nfl %>%
  filter(!is.na(predicted_cp)) %>%
  mutate(bin = cut(predicted_cp, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(cp = mean(complete_pass, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_pbp_ypc <- xgb.load('xgb_pbp_ypc_no_weather.model')

# (play_type == "run") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind

# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_ypc <- function(data) {
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_ypc(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_ypc, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_ypc <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_ypc), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_ypc = ifelse(play_type == "run" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0, predicted_ypc, NA))


pbp_nfl %>%
  filter(!is.na(predicted_ypc) & play_type == "run") %>%
  mutate(bin = cut(predicted_ypc, breaks = seq(-2, 10, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ypc = mean(yards_gained, na.rm = TRUE), .groups = 'drop', n = n())

# NOTE - THIS MODEL IS THE TRAINING MODEL, NOT THE FULL MODEL. WE'LL HAVE TO CHECK IN ON THE AFTER_RUN ON THE PART SIDE. 


#####
#####
#####


xgb_pbp_ypa <- xgb.load('xgb_pbp_ypa_no_weather.model')

# play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards)
# yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_ypa <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_ypa(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_ypa, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_ypa <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_ypa), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_ypa = ifelse(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards), predicted_ypa, NA))


pbp_nfl %>%
  filter(!is.na(predicted_ypa) & play_type == "pass") %>%
  mutate(bin = cut(predicted_ypa, breaks = seq(-2, 15, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ypa = mean(yards_gained, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_pbp_yac <- xgb.load('xgb_pbp_yac_no_weather.model')

# play_type == "pass" & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards) & !is.na(yards_after_catch)
# yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, air_yards, down_one_ind, down_two_ind, down_three_ind


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_yac <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, air_yards, down_one_ind, down_two_ind, down_three_ind
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_yac(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_yac, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_yac <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl)

# Merge predictions back to original pbp_nfl dataframe
pbp_nfl <- pbp_nfl %>%
  left_join(prediction_results %>% select(play_id, game_id, predicted_yac), by = c("game_id", "play_id")) %>%
  mutate(
    predicted_yac = ifelse(play_type == "pass" & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards) & !is.na(yards_after_catch), predicted_yac, NA))


pbp_nfl %>%
  filter(!is.na(predicted_yac) & play_type == "pass") %>%
  mutate(bin = cut(predicted_yac, breaks = seq(-2, 15, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(yac = mean(yards_after_catch, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####

pbp_nfl$drive_id <- paste0(pbp_nfl$game_id, "-", pbp_nfl$posteam, "-", pbp_nfl$fixed_drive)


pbp_nfl$predicted_before_xtd[which(is.na(pbp_nfl$predicted_before_xtd))] <- 0


pbp_nfl$after_td <- coalesce(pbp_nfl$predicted_after_pass_xtd, pbp_nfl$predicted_after_run_xtd)
pbp_nfl$after_td[which(is.na(pbp_nfl$after_td))] <- 0


library(dplyr)

pbp_nfl <- pbp_nfl %>%
  group_by(drive_id) %>%
  mutate(
    before_remaining_xtd = 1,  # Start with 100% chance of potential TD at the beginning of each drive
    after_remaining_xtd = 1,
    before_xtd_new = predicted_before_xtd * before_remaining_xtd,  # Initial calculation for the first row in each drive
    after_xtd_new = after_td * after_remaining_xtd
  ) %>%
  mutate(
    # Calculate the remaining expected touchdowns for the next plays in the drive
    before_remaining_xtd = lag(before_remaining_xtd, default = 1) - lag(before_xtd_new, default = 0),
    after_remaining_xtd = lag(after_remaining_xtd, default = 1) - lag(after_xtd_new, default = 0),
    
    # Calculate the new expected touchdowns based on the remaining touchdowns
    before_xtd_new = predicted_before_xtd * before_remaining_xtd,
    after_xtd_new = after_td * after_remaining_xtd
  ) %>%
  ungroup()


pbp_xtd <- pbp_nfl %>% group_by(posteam, week, season) %>% 
              dplyr::summarise(before_old_xtd = sum(predicted_before_xtd),
                              before_new_xtd = sum(before_xtd_new),
                              after_old_xtd = sum(after_td),
                              after_new_xtd = sum(after_xtd_new),
                              actual_td = sum(touchdown),
                              actual_fg = sum(field_goal_attempt))

write.csv(pbp_xtd, 'pbp_xtd.csv')
