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

options(scipen = 999)

part_nfl <- load_participation(c(2016:2023), include_part = T)
part_nfl$part_ind = 1

'%ni%' <- Negate('%in%')

options(scipen = 999)

part_nfl <- part_nfl %>% mutate(mod_ydstogo = ifelse(goal_to_go == 1, yardline_100, ydstogo))
part_nfl <- part_nfl %>% mutate(lead_timeout = lag(timeout_team, 1), last_timeout_ind = case_when(lead_timeout == posteam ~ 1, is.na(lead_timeout) ~ 0, (lead_timeout != posteam & !is.na(lead_timeout)) ~ -1))
part_nfl$td_side = ifelse((!is.na(part_nfl$td_team) & part_nfl$posteam == part_nfl$td_team), 1, 0)
part_nfl <- part_nfl %>% mutate(posteam_ind = ifelse(home_team == posteam, 1, 0))

part_nfl$week <- ifelse(part_nfl$week == 18 & part_nfl$season <= 2020, 28, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 19 & part_nfl$season <= 2020, 29, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 19 & part_nfl$season > 2020, 28, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 20 & part_nfl$season <= 2020, 30, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 20 & part_nfl$season > 2020, 29, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 21 & part_nfl$season <= 2020, 32, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 21 & part_nfl$season > 2020, 30, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 22, 32, part_nfl$week)

part_nfl$posteam[which(part_nfl$posteam == "ARI")] = "ARZ"
part_nfl$posteam[which(part_nfl$posteam == "BAL")] = "BLT"
part_nfl$posteam[which(part_nfl$posteam == "CLE")] = "CLV"
part_nfl$posteam[which(part_nfl$posteam == "HOU")] = "HST"
part_nfl$posteam[which(part_nfl$posteam == "SD")] = "LAC"
part_nfl$posteam[which(part_nfl$posteam == "OAK")] = "LV"

part_nfl$defteam[which(part_nfl$defteam == "ARI")] = "ARZ"
part_nfl$defteam[which(part_nfl$defteam == "BAL")] = "BLT"
part_nfl$defteam[which(part_nfl$defteam == "CLE")] = "CLV"
part_nfl$defteam[which(part_nfl$defteam == "HOU")] = "HST"
part_nfl$defteam[which(part_nfl$defteam == "SD")] = "LAC"
part_nfl$defteam[which(part_nfl$defteam == "OAK")] = "LV"

part_nfl$td_team[which(part_nfl$td_team == "ARI")] = "ARZ"
part_nfl$td_team[which(part_nfl$td_team == "BAL")] = "BLT"
part_nfl$td_team[which(part_nfl$td_team == "CLE")] = "CLV"
part_nfl$td_team[which(part_nfl$td_team == "HOU")] = "HST"
part_nfl$td_team[which(part_nfl$td_team == "SD")] = "LAC"
part_nfl$td_team[which(part_nfl$td_team == "OAK")] = "LV"

part_nfl$possession_team[which(part_nfl$possession_team == "ARI")] = "ARZ"
part_nfl$possession_team[which(part_nfl$possession_team == "BAL")] = "BLT"
part_nfl$possession_team[which(part_nfl$possession_team == "CLE")] = "CLV"
part_nfl$possession_team[which(part_nfl$possession_team == "HOU")] = "HST"
part_nfl$possession_team[which(part_nfl$possession_team == "SD")] = "LAC"
part_nfl$possession_team[which(part_nfl$possession_team == "OAK")] = "LV"

part_nfl$air_yards[which(part_nfl$air_yards <= -54)] = 0

part_nfl$down_one_ind = ifelse(part_nfl$down == 1, 1, 0)
part_nfl$down_two_ind = ifelse(part_nfl$down == 2, 1, 0)
part_nfl$down_three_ind = ifelse(part_nfl$down == 3, 1, 0)
part_nfl$down_four_ind = ifelse(part_nfl$down == 4, 1, 0)

part_nfl$distance_to_sticks = part_nfl$mod_ydstogo - part_nfl$air_yards

part_nfl$season_type <- ifelse(part_nfl$season_type == "REG", 0, 1)
part_nfl$roof <- ifelse(part_nfl$roof %in% c('dome', 'closed'), 1, 0)
part_nfl$surface <- ifelse(part_nfl$surface == "grass", 0, 1)

part_nfl$pass_length <- case_when(is.na(part_nfl$pass_length) ~ 0, part_nfl$pass_length == "short" ~ 1, part_nfl$pass_length == "deep" ~ 2)
part_nfl$pass_location <- case_when(is.na(part_nfl$pass_location) ~ 0, part_nfl$pass_location == "middle" ~ 1, part_nfl$pass_location %in% c('left', 'right') ~ 2)

part_nfl$run_location <- case_when(is.na(part_nfl$run_location) ~ 2, part_nfl$run_location %in% c("left", "right") ~ 1, part_nfl$run_location == "middle" ~ 0)
part_nfl$run_gap <- case_when(is.na(part_nfl$run_gap) ~ 0, part_nfl$run_gap == "guard" ~ 1, part_nfl$run_gap == "tackle" ~ 2, part_nfl$run_gap == "end" ~ 3)

part_nfl$guard_ind = ifelse(part_nfl$run_gap == 1, 1, 0)
part_nfl$tackle_ind = ifelse(part_nfl$run_gap == 2, 1, 0)
part_nfl$end_ind = ifelse(part_nfl$run_gap == 3, 1, 0)

part_nfl$middle_ind = ifelse(part_nfl$pass_location == 1, 1, 0)
part_nfl$outside_ind = ifelse(part_nfl$pass_location == 2, 1, 0)

part_nfl$pressure_ind = case_when(part_nfl$was_pressure == T ~ 1, part_nfl$was_pressure == F ~ 0, is.na(part_nfl$was_pressure) ~ NA)

games_with_missing <- part_nfl %>% filter(offense_personnel == "" | defense_personnel == "") %>% filter(play_type %in% c("pass", "run")) %>% select(nflverse_game_id, possession_team, play_type) %>% distinct()

gwm_off_personnel <- part_nfl %>% filter(offense_personnel != "" | defense_personnel != "") %>% filter(nflverse_game_id %in% games_with_missing$nflverse_game_id & possession_team %in% games_with_missing$possession_team & play_type %in% games_with_missing$play_type) %>% group_by(nflverse_game_id, possession_team, offense_personnel, play_type) %>% dplyr::summarize(n = n()) %>% arrange(desc(n)) %>% ungroup() %>% group_by(nflverse_game_id, possession_team, play_type) %>% mutate(rank = rank(-n, ties.method = "first")) %>% filter(rank == 1) %>% select(-c(n, rank))
colnames(gwm_off_personnel)[3] <- "mode_off_personnel"

part_nfl <- part_nfl %>%
  left_join(gwm_off_personnel, by = c("nflverse_game_id", "possession_team", "play_type")) 

part_nfl$offense_personnel <- ifelse(part_nfl$offense_personnel == "" | is.na(part_nfl$offense_personnel), part_nfl$mode_off_personnel, part_nfl$offense_personnel)

gwm_def_personnel <- part_nfl %>% filter(offense_personnel != "" | defense_personnel != "") %>% filter(nflverse_game_id %in% games_with_missing$nflverse_game_id & possession_team %in% games_with_missing$possession_team & play_type %in% games_with_missing$play_type) %>% group_by(nflverse_game_id, possession_team, defense_personnel, play_type) %>% dplyr::summarize(n = n()) %>% arrange(desc(n)) %>% ungroup() %>% group_by(nflverse_game_id, possession_team, play_type) %>% mutate(rank = rank(-n, ties.method = "first")) %>% filter(rank == 1) %>% select(-c(n, rank))
colnames(gwm_def_personnel)[3] <- "mode_def_personnel"

part_nfl <- part_nfl %>%
  left_join(gwm_def_personnel, by = c("nflverse_game_id", "possession_team", "play_type"))

part_nfl$defense_personnel <- ifelse(part_nfl$defense_personnel == "" | is.na(part_nfl$defense_personnel), part_nfl$mode_def_personnel, part_nfl$defense_personnel)



part_nfl <- part_nfl %>%
  mutate(offense_formation = case_when(
    !is.na(offense_formation) ~ offense_formation,  # Keeps existing values
    str_detect(offense_personnel, "1 P") ~ "PUNT",
    offense_personnel == "1 RB, 3 TE, 1 WR" ~ "JUMBO",
    offense_personnel == "1 RB, 0 TE, 4 WR" & shotgun == 1 ~ "SHOTGUN",
    offense_personnel == "1 RB, 0 TE, 4 WR" & shotgun == 0 ~ "EMPTY",
    (offense_personnel %in% c("1 RB, 1 TE, 3 WR", "1 RB, 2 TE, 2 WR", 
                              "6 OL, 1 RB, 1 TE, 2 WR", "6 OL, 1 RB, 2 TE, 1 WR") & shotgun == 0) | offense_formation == "ACE" ~ "SINGLEBACK",
    offense_personnel %in% c("2 RB, 2 TE, 1 WR", "2 RB, 1 TE, 2 WR", 
                             "6 OL, 2 RB, 1 TE, 1 WR") & shotgun == 0 ~ "I_FORM",
    offense_personnel != "1 RB, 0 TE, 4 WR" & shotgun == 1 ~ "SHOTGUN",
    TRUE ~ offense_formation  # Default to keep the original value if no condition is met
  )) 

part_nfl$offense_formation[which(part_nfl$offense_personnel == "7 OL, 0 RB, 1 TE, 0 WR,1 P,1 LS,1 K")] = "PUNT"

extract_players <- function(data, pattern) {
  numbers <- str_extract(data, paste0("(\\d+)\\s*", pattern))
  numbers <- ifelse(is.na(numbers), 0, as.integer(str_extract(numbers, "\\d+")))
  return(numbers)
}

extract_position_count <- function(data, primary_pattern, secondary_pattern = NULL, default_primary = 0) {
  # Extract the number for the primary pattern
  primary_count <- ifelse(str_detect(data, primary_pattern), 
                          extract_players(data, primary_pattern), 
                          default_primary)
  
  # Initialize the secondary count as zero
  secondary_count <- if (!is.null(secondary_pattern)) {
    # If the primary pattern is not found, then add the secondary pattern count
    secondary_counts <- extract_players(data, secondary_pattern)
    ifelse(str_detect(data, primary_pattern), 0, secondary_counts)
  } else {
    0
  }
  
  # Sum the counts
  return(primary_count + secondary_count)
}

part_nfl <- part_nfl %>%
  mutate(
    n_ol = extract_position_count(offense_personnel, "OL", "DL", default_primary = 5),
    n_te = extract_position_count(offense_personnel, "TE", "QB", default_primary = 0),
    n_wr = extract_position_count(offense_personnel, "WR", "DB", default_primary = 0),
    n_rb = extract_position_count(offense_personnel, "RB", "LB", default_primary = 0),
    n_st = extract_players(offense_personnel, "K") + extract_players(offense_personnel, "P") + extract_players(offense_personnel, "LS")
  )

part_nfl <- part_nfl %>%
  mutate(
    n_dl = ifelse(str_detect(desc, "Punt formation"),
                  extract_players(defense_personnel, "DL"),
                  extract_players(defense_personnel, "DL") +
                    extract_players(defense_personnel, "RB") +
                    extract_players(defense_personnel, "OL") +
                    extract_players(defense_personnel, "TE")),
    n_lb = ifelse(str_detect(desc, "Punt formation"),
                  extract_players(defense_personnel, "LB"),
                  extract_players(defense_personnel, "LB")),
    n_db = ifelse(str_detect(desc, "Punt formation"),
                  extract_players(defense_personnel, "DB"),
                  extract_players(defense_personnel, "DB") +
                    extract_players(defense_personnel, "WR")),
    n_st_def = 11 - n_dl - n_lb - n_db)

part_nfl$n_st_def[which(part_nfl$n_st_def < 0)] = 0


######
######

part_nfl <- part_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0)

# Load pre-trained XGBoost models
xgb_defenders_box_no_weather <- xgb.load("xgb_defenders_box_no_weather.model")


#####
#####


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix <- function(data) {
  X <- data %>% select(
    season_type, yardline_100, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind,
    mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, posteam_timeouts_remaining,
    defteam_timeouts_remaining, roof, surface, posteam_ind, n_ol, n_te, n_wr, n_rb, n_dl, n_lb, n_db
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_defenders_box_no_weather, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_defenders_in_box <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_defenders_in_box), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    defenders_in_box = ifelse(is.na(defenders_in_box), predicted_defenders_in_box, defenders_in_box)
  )

part_nfl$defenders_in_box[which(part_nfl$defenders_in_box < 0)] = 0


#####
#####


part_nfl <- part_nfl %>%
  mutate(
    i_form_ind = as.integer(offense_formation == "I_FORM"),
    shotgun_ind = as.integer(offense_formation == "SHOTGUN"),
    singleback_ind = as.integer(offense_formation == "SINGLEBACK"),
    pistol_ind = as.integer(offense_formation == "PISTOL"),
    jumbo_ind = as.integer(offense_formation == "JUMBO"),
    empty_ind = as.integer(offense_formation == "EMPTY"),
    wildcat_ind = as.integer(offense_formation == "WILDCAT"),
    st_ind = as.integer(offense_formation %in% c("PUNT", "FIELD_GOAL"))
  )

part_nfl <- part_nfl %>% 
  mutate(
    flat_route_ind = as.integer(route == "FLAT"),
    angle_route_ind = as.integer(route == "ANGLE"),
    screen_route_ind = as.integer(route == "SCREEN"),
    hitch_route_ind = as.integer(route == "HITCH"),
    go_route_ind = as.integer(route == "GO"),
    out_route_ind = as.integer(route == "OUT"),
    slant_route_ind = as.integer(route == "SLANT"),
    cross_route_ind = as.integer(route == "CROSS"),
    post_route_ind = as.integer(route == "POST"),
    corner_route_ind = as.integer(route == "CORNER"),
    in_route_ind = as.integer(route == "IN"),
    wheel_route_ind = as.integer(route == "WHEEL"),
    na_route_ind = as.integer(is.na(route)),
    cover3_ind = as.integer(defense_coverage_type == "COVER_3"),
    cover2_ind = as.integer(defense_coverage_type == "COVER_2"),
    cover0_ind = as.integer(defense_coverage_type == "COVER_0"),   
    cover1_ind = as.integer(defense_coverage_type == "COVER_1"),
    cover4_ind = as.integer(defense_coverage_type == "COVER_4"),
    twoman_ind = as.integer(defense_coverage_type == "2_MAN"),
    cover6_ind = as.integer(defense_coverage_type == "COVER_6"),  
    prevent_cov_ind = as.integer(defense_coverage_type == "PREVENT"),
    man_ind = as.integer(defense_man_zone_type == "MAN_COVERAGE"),
    zone_ind = as.integer(defense_man_zone_type == "ZONE_COVERAGE")
  )


part_nfl <- part_nfl %>% mutate(defense_formation = case_when(
  n_db >= 7 ~ "Prevent",
  (n_db == 6 | defense_personnel == "1 DL, 4 LB, 5 DB") ~ "Quarter",
  (n_dl >= 6 | defense_personnel %in% c("5 DL, 4 LB, 2 DB", "4 DL, 5 LB, 2 DB", "5 DL, 5 LB, 1 DB", "3 DL, 5 LB, 2 DB", "4 DL, 5 LB, 1 DB", "4 DL, 5 LB, 1 DB, 1 OL", "3 DL, 5 LB, 2 DB, 1 OL")) ~ "Goal_Line",
  (defense_personnel %in% c("1 DL, 5 LB, 5 DB", "2 DL, 4 LB, 5 DB", "0 DL, 6 LB, 5 DB", "2 DL, 3 LB, 5 DB", "1 DL, 6 LB, 4 DB", "1 DL, 5 LB, 4 DB", "0 DL, 5 LB, 5 DB, 1 RB") | (n_dl == 2 & n_db == 5)) ~ "Nickel_Lite",
  (defense_personnel %in% c("4 DL, 2 LB, 5 DB", "4 DL, 1 LB, 5 DB", "4 DL, 2 LB, 3 DB") | (n_dl == 4 & n_lb == 2)) ~ "Nickel",
  defense_personnel %in% c("3 DL, 3 LB, 5 DB", "3 DL, 3 LB, 4 DB", "5 DL, 1 LB, 5 DB", "3 DL, 2 LB, 5 DB", "4 DL, 3 LB, 5 DB", "5 DL, 1 LB, 4 DB", "2 DL, 3 LB, 4 DB", "1 DL, 3 LB, 5 DB, 2 RB", "3 DL, 3 LB, 4 DB, 1 WR", "2 DL, 3 LB, 5 DB, 1 RB", "4 DL, 1 LB, 5 DB, 1 TE", "2 DL, 3 LB, 5 DB, 1 TE") ~ "335",
  (defense_personnel %in% c("4 DL, 3 LB, 4 DB", "2 DL, 4 LB, 4 DB", "4 DL, 4 LB, 4 DB", "5 DL, 3 LB, 4 DB") | (n_dl == 4 & n_lb == 3)) ~ "43",
  (defense_personnel %in% c("3 DL, 5 LB, 3 DB", "5 DL, 2 LB, 4 DB", "3 DL, 3 LB, 4 DB", "3 DL, 4 LB, 3 DB", "5 DL, 2 LB, 3 DB", "3 DL, 6 LB, 2 DB", "3 DL, 4 LB, 5 DB", "3 DL, 5 LB, 2 DB", "4 DL, 2 LB, 3 DB, 1 OL", "4 DL, 2 LB, 4 DB, 1 OL") | (n_dl == 3 & n_lb == 4)) ~ "34",
  defense_personnel %in% c("4 DL, 4 LB, 3 DB", "5 DL, 3 LB, 3 DB", "4 DL, 6 LB, 1 DB", "4 DL, 4 LB, 2 DB", "5 DL, 3 LB, 2 DB", "3 DL, 4 LB, 3 DB, 1 OL", "3 DL, 4 LB, 3 DB, 1 RB", "4 DL, 3 LB, 3 DB, 1 OL") ~ "46",
  defense_personnel %in% c("2 DL, 5 LB, 4 DB", "2 DL, 6 LB, 3 DB") ~ "Heavy_2Front",
  offense_formation == "PUNT" ~ "Punt"))

part_nfl <- part_nfl %>%
  mutate(
    d43_ind = as.integer(defense_formation == "43"),
    d34_ind = as.integer(defense_formation == "34"),
    nickel_lite_ind = as.integer(defense_formation == "Nickel_Lite"),
    d335_ind = as.integer(defense_formation == "335"),
    quarter_ind = as.integer(defense_formation == "Quarter"),
    d46_ind = as.integer(defense_formation == "46"),
    nickel_ind = as.integer(defense_formation == "Nickel"),
    goal_line_ind = as.integer(defense_formation == "Goal_Line"),
    heavy_2front_ind = as.integer(defense_formation == "Heavy_2Front"),
    prevent_ind = as.integer(defense_formation == "Prevent"),
    punt_ind_def = as.integer(defense_formation == "Punt")
  )

part_nfl$ngs_air_yards <- ifelse(!is.na(part_nfl$air_yards) & is.na(part_nfl$ngs_air_yards), part_nfl$air_yards, part_nfl$ngs_air_yards)


####
####


xgboost_part_nfl <- part_nfl %>% filter(play_type == "run" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0) %>% select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, end_ind, guard_ind, tackle_ind)

xgboost_part_nfl_weather <- part_nfl %>% filter(play_type == "run" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)) %>% select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, temp, wind, end_ind, guard_ind, tackle_ind)


####
####



sample_split_all_dfs_pass <- sample.split(Y = xgboost_part_nfl$yards_gained, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = xgboost_part_nfl, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_part_nfl, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$yards_gained
y_test_pass_all_dfs <- test_set_pass_all_dfs$yards_gained


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_part_nfl %>% select(-yards_gained)
d_ypass_all = xgboost_part_nfl$yards_gained
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_part_pass = .03 # .03
gamma_part_pass = .25 # .25
max_depth_part_pass = 4 # 4
min_child_weight_part_pass = 3 # 3
alpha_part_pass = 1 # 1
lambda_part_pass = 1 # 1
colsample_bynode_part_pass = .35 # .35
colsample_bylevel_part_pass = .95 # .95
colsample_bytree_part_pass = .95 # .95

xgb_part_pass <- xgboost(data = dtrain_pass_all_dfs, 
                         label = y_train_pass_all_dfs, 
                         eta = eta_part_pass,
                         max_depth = max_depth_part_pass, 
                         alpha = alpha_part_pass,
                         lambda = lambda_part_pass,
                         min_child_weight = min_child_weight_part_pass,
                         colsample_bynode = colsample_bynode_part_pass,
                         colsample_bytree = colsample_bytree_part_pass,
                         colsample_bylevel = colsample_bylevel_part_pass,
                         nround = 340, # 340
                         objective = "reg:squarederror",
                         nthread = 2,
                         gamma = gamma_part_pass,
                         early_stopping_rounds = 50
)


breaks <- seq(-3, 13, by = 0.5)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_part_pass, newdata = dtest_pass_all_dfs)

resultant_df_part_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_part_pass) = c("Preds", "Vals")

resultant_df_part_pass$buckets <- cut(resultant_df_part_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

View(resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


summary(xgb_part_pass)

names_part_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_part_pass, model = xgb_part_pass)
importance_matrix_before_all

predictions <- predict(xgb_part_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)


# Convert predictions and actual values to a dataframe
resultant_df_part_pass <- data.frame(
  Preds = test_preds,
  Actuals = y_test_pass_all_dfs
)

# Calculate RMSE
rmse_value <- sqrt(mean((resultant_df_part_pass$Preds - resultant_df_part_pass$Actuals)^2))
print(paste("RMSE:", rmse_value))
# 6.0584




xgb_part_pass <- xgboost(data = as.matrix(d_xpass_all), 
                         label = d_ypass_all, 
                         eta = eta_part_pass,
                         max_depth = max_depth_part_pass, 
                         alpha = alpha_part_pass,
                         lambda = lambda_part_pass,
                         min_child_weight = min_child_weight_part_pass,
                         colsample_bynode = colsample_bynode_part_pass,
                         colsample_bytree = colsample_bytree_part_pass,
                         colsample_bylevel = colsample_bylevel_part_pass,
                         nround = 340, # 340
                         objective = "reg:squarederror",
                         nthread = 2,
                         gamma = gamma_part_pass,
                         early_stopping_rounds = 50
)

xgb.save(xgb_part_pass, 'xgb_part_ypc_no_weather.model')


######
###### DUE TO WEATHER BEING IN BOTTOM HALF GOING TO SAY DOESN'T MATTER
######


sample_split_all_dfs_pass <- sample.split(Y = xgboost_part_nfl_weather$yards_gained, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = xgboost_part_nfl_weather, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_part_nfl_weather, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$yards_gained
y_test_pass_all_dfs <- test_set_pass_all_dfs$yards_gained


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_part_nfl_weather %>% select(-yards_gained)
d_ypass_all = xgboost_part_nfl_weather$yards_gained
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_part_pass = .032 # .032
gamma_part_pass = .25 # .25
max_depth_part_pass = 4 # 4
min_child_weight_part_pass = 3 # 3
alpha_part_pass = 1 # 1
lambda_part_pass = 1 # 1
colsample_bynode_part_pass = .35 # .35
colsample_bylevel_part_pass = .95 # .95
colsample_bytree_part_pass = .95 # .95

xgb_part_pass <- xgboost(data = dtrain_pass_all_dfs, 
                         label = y_train_pass_all_dfs, 
                         eta = eta_part_pass,
                         max_depth = max_depth_part_pass, 
                         alpha = alpha_part_pass,
                         lambda = lambda_part_pass,
                         min_child_weight = min_child_weight_part_pass,
                         colsample_bynode = colsample_bynode_part_pass,
                         colsample_bytree = colsample_bytree_part_pass,
                         colsample_bylevel = colsample_bylevel_part_pass,
                         nround = 350, # 350
                         objective = "reg:squarederror",
                         nthread = 2,
                         gamma = gamma_part_pass,
                         early_stopping_rounds = 50
)


breaks <- seq(-3, 13, by = 0.5)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_part_pass, newdata = dtest_pass_all_dfs)

resultant_df_part_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_part_pass) = c("Preds", "Vals")

resultant_df_part_pass$buckets <- cut(resultant_df_part_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

View(resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


summary(xgb_part_pass)

names_part_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_part_pass, model = xgb_part_pass)
importance_matrix_before_all

predictions <- predict(xgb_part_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)


# Convert predictions and actual values to a dataframe
resultant_df_part_pass <- data.frame(
  Preds = test_preds,
  Actuals = y_test_pass_all_dfs
)

# Calculate RMSE
rmse_value <- sqrt(mean((resultant_df_part_pass$Preds - resultant_df_part_pass$Actuals)^2))
print(paste("RMSE:", rmse_value))
# 6.068


xgb_part_pass <- xgboost(data = as.matrix(d_xpass_all), 
                         label = d_ypass_all, 
                         eta = eta_part_pass,
                         max_depth = max_depth_part_pass, 
                         alpha = alpha_part_pass,
                         lambda = lambda_part_pass,
                         min_child_weight = min_child_weight_part_pass,
                         colsample_bynode = colsample_bynode_part_pass,
                         colsample_bytree = colsample_bytree_part_pass,
                         colsample_bylevel = colsample_bylevel_part_pass,
                         nround = 350, # 350
                         objective = "reg:squarederror",
                         nthread = 2,
                         gamma = gamma_part_pass,
                         early_stopping_rounds = 50
)

xgb.save(xgb_part_pass, 'xgb_part_ypc_weather.model')

