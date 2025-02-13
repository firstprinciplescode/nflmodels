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


part_nfl <- load_participation(c(2016:2023), include_pass = T)
part_nfl$part_ind = 1

'%ni%' <- Negate('%in%')

options(scipen = 999)


### CHECKING TO SEE IF / WHEN THINGS ARE NULL

# DEFENDERS_IN_BOX

part_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0 & penalty == 0 & qb_spike == 0 & qb_kneel == 0 & !is.na(yards_gained) & is.na(defenders_in_box))

# THERE ARE SOME IN REGULAR PLAYS BUT SOLID MAJORITY ARE FIELD GOAL / PUNT FORMATION


# NUMBER_OF_PASS_RUSHERS

part_nfl %>% filter(play_type %in% c("pass") & two_point_attempt == 0 & penalty == 0 & qb_spike == 0 & qb_kneel == 0 & !is.na(yards_gained) & is.na(number_of_pass_rushers) & rush_attempt == 0 & is.na(run_gap))

# 7 ROWS LOL


# TIME_TO_THROW

part_nfl %>% filter(play_type %in% c("pass") & two_point_attempt == 0 & penalty == 0 & qb_spike == 0 & qb_kneel == 0 & !is.na(yards_gained) & is.na(time_to_throw) & rush_attempt == 0 & is.na(run_gap) & sack == 0)

# 2 ROWS LOL



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

part_nfl$middle_ind = ifelse(part_nfl$pass_location == 1, 1, 0)
part_nfl$outside_ind = ifelse(part_nfl$pass_location == 2, 1, 0)

part_nfl$run_location <- case_when(is.na(part_nfl$run_location) ~ 2, part_nfl$run_location %in% c("left", "right") ~ 1, part_nfl$run_location == "middle" ~ 0)
part_nfl$run_gap <- case_when(is.na(part_nfl$run_gap) ~ 0, part_nfl$run_gap == "guard" ~ 1, part_nfl$run_gap == "tackle" ~ 2, part_nfl$run_gap == "end" ~ 3)

part_nfl$guard_ind = ifelse(part_nfl$run_gap == 1, 1, 0)
part_nfl$tackle_ind = ifelse(part_nfl$run_gap == 2, 1, 0)
part_nfl$end_ind = ifelse(part_nfl$run_gap == 3, 1, 0)

part_nfl <- part_nfl %>% 
  mutate(
    outside_run_ind = as.integer(run_location %in% c("left", "right")),
    middle_run_ind = as.integer(run_location == "middle"),
    end_gap_ind = as.integer(run_gap == "end"),
    guard_gap_ind = as.integer(run_gap == "guard"),
    tackle_gap_ind = as.integer(run_gap == "tackle"),
    middle_gap_ind = as.integer(is.na(run_gap) & play_type == "run"),
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

part_nfl <- part_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0)

# WE MAY WANT TO CUT MORE PLAYS OUT DEPENDING ON WHAT OUTLIER CASES THERE ARE


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
    punt_ind_def = as.integer(defense_formation == "Punt"),
    no_route_ind = as.integer(is.na(route) & play_type == "pass" & sack == 0 & two_point_attempt == 0))
  

part_nfl$ngs_air_yards <- ifelse(!is.na(part_nfl$air_yards) & is.na(part_nfl$ngs_air_yards), part_nfl$air_yards, part_nfl$ngs_air_yards)


####
####


# Load the models
xgb_pass_rusher_no_weather <- xgb.load("xgb_pass_rusher_no_weather.model")
xgb_pass_rusher_weather <- xgb.load("xgb_pass_rusher_weather.model")


# Function to create DMatrix for pass rushers
create_dmatrix_pass_rushers <- function(data, weather_cols = NULL) {
  if (!is.null(weather_cols)) {
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  X <- data %>% select(
    season_type , yardline_100 , half_seconds_remaining , down , down_one_ind , down_two_ind , down_three_ind , mod_ydstogo , shotgun , no_huddle , last_timeout_ind , score_differential , posteam_timeouts_remaining , defteam_timeouts_remaining , roof , surface , posteam_ind , n_ol , n_te , n_wr , n_rb , n_dl , n_lb , n_db, defenders_in_box, temp, wind
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions for pass rushers
make_predictions_pass_rushers <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_pass_rushers(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_pass_rusher_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_pass_rushers(part_data)
  predictions_without_weather <- predict(xgb_pass_rusher_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_pass_rushers <- NA
  
  # Assign predictions to the new column based on the availability of weather data
  part_data$predicted_pass_rushers[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_pass_rushers[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}


# Apply the prediction function for pass rushers
prediction_results_pass_rushers <- make_predictions_pass_rushers(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_pass_rushers %>% select(nflverse_game_id, play_id, predicted_pass_rushers), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    number_of_pass_rushers = ifelse(is.na(number_of_pass_rushers) & play_type == "pass", predicted_pass_rushers, number_of_pass_rushers)
  )

part_nfl$number_of_pass_rushers[which(part_nfl$number_of_pass_rushers < 0)] = 0


#####
#####
#####


xgb_time_to_throw_snap_no_weather <- xgb.load("xgb_time_to_throw_at_snap_no_weather.model")
xgb_time_to_throw_snap_weather <- xgb.load('xgb_time_to_throw_at_snap_weather.model')


# Function to create DMatrix for snap time to throw predictions
create_dmatrix_snap_time_to_throw <- function(data, weather_cols = NULL) {
  if (!is.null(weather_cols)) {
    # Filter data to include only rows with weather information
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    # Filter data to include only rows without weather information
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select the appropriate columns
  X <- data %>% select(
    season_type, yardline_100, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind,
    mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, posteam_timeouts_remaining,
    defteam_timeouts_remaining, roof, surface, posteam_ind, n_ol, n_te, n_wr, n_rb, n_dl, n_lb, n_db, 
    temp, wind, number_of_pass_rushers
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make snap time to throw predictions
make_predictions_snap_time_to_throw <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_snap_time_to_throw(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_time_to_throw_snap_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_snap_time_to_throw(part_data)
  predictions_without_weather <- predict(xgb_time_to_throw_snap_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_time_to_throw <- NA
  
  # Assign predictions to the new column based on the availability of weather data
  part_data$predicted_time_to_throw[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_time_to_throw[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_snap_time_to_throw <- make_predictions_snap_time_to_throw(part_nfl)

# Merge predictions back to the original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_snap_time_to_throw %>% select(nflverse_game_id, play_id, predicted_time_to_throw), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(predicted_time_to_throw = ifelse(play_type == "pass" & rush_attempt == 0 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0, predicted_time_to_throw, NA))

part_nfl %>%
  filter(!is.na(predicted_time_to_throw) & play_type == "pass") %>%
  mutate(bin = cut(predicted_time_to_throw, breaks = seq(0, 5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ttt = mean(time_to_throw, na.rm = TRUE), .groups = 'drop', n = n())


######
######
######


xgb_time_to_throw_route_no_weather <- xgb.load('xgb_time_to_throw_route_no_weather.model')
xgb_time_to_throw_route_weather <- xgb.load('xgb_time_to_throw_route_weather.model')


# Function to create DMatrix
create_dmatrix <- function(data, weather_cols = NULL) {
  # Filtering based on weather columns or their absence
  if (!is.null(weather_cols)) {
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select relevant columns
  X <- data %>% select(
    season_type, yardline_100, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind,
    mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, posteam_timeouts_remaining,
    defteam_timeouts_remaining, roof, surface, posteam_ind, n_ol, n_te, n_wr, n_rb, n_dl, n_lb, n_db,
    defenders_in_box, temp, wind, number_of_pass_rushers, hitch_route_ind, go_route_ind, out_route_ind,
    slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
make_predictions_time_to_throw <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_time_to_throw_route_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix(part_data)
  predictions_without_weather <- predict(xgb_time_to_throw_route_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_time_to_throw_throw <- NA
  
  # Assign predictions based on the availability of weather data
  part_data$predicted_time_to_throw_throw[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_time_to_throw_throw[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_time_to_throw_route <- make_predictions_time_to_throw(part_nfl)

# Merge predictions back to the original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_time_to_throw_route %>% select(nflverse_game_id, play_id, predicted_time_to_throw_throw), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    time_to_throw = ifelse(
      is.na(time_to_throw) & (!is.na(air_yards) | !is.na(ngs_air_yards)) & sack == 0 & fumble == 0,
      predicted_time_to_throw_throw,
      time_to_throw
    )
  )


colnames(part_nfl)[which(colnames(part_nfl) == "predicted_time_to_throw")] <- "predicted_time_to_throw_snap"
colnames(part_nfl)[which(colnames(part_nfl) == "predicted_time_to_throw_throw")] <- "predicted_time_to_throw"


part_nfl %>%
  filter(!is.na(predicted_time_to_throw) & play_type == "pass") %>%
  mutate(bin = cut(predicted_time_to_throw, breaks = seq(0, 7, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ttt = mean(time_to_throw, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_before_xtd <- xgb.load('xgb_part_before_xtd.model')

# yardline_100, defenders_in_box, half_seconds_remaining, shotgun, no_huddle, qb_dropback, posteam_timeouts_remaining, defteam_timeouts_remaining, score_differential, mod_ydstogo, n_ol:n_st_def
# (pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_before_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, defenders_in_box, half_seconds_remaining, shotgun, no_huddle, qb_dropback, posteam_timeouts_remaining, defteam_timeouts_remaining, score_differential, mod_ydstogo, n_ol:n_st_def
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_before_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_before_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_before_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_before_xtd), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    before_xtd = ifelse((pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0, predicted_before_xtd, NA)
  )


part_nfl %>%
  filter(!is.na(before_xtd) & play_type == "pass") %>%
  mutate(bin = cut(before_xtd, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(td_rate = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_after_run_xtd <- xgb.load('xgb_part_after_run_xtd.model')

# yardline_100, defenders_in_box, half_seconds_remaining, shotgun, no_huddle, qb_dropback, posteam_timeouts_remaining, defteam_timeouts_remaining, score_differential, qb_scramble, mod_ydstogo, n_ol:n_st_def, outside_run_ind:middle_gap_ind, end_ind, tackle_ind, guard_ind
# two_point_attempt == 0 & play_type %in% c("run") & rush_attempt == 1 & qb_kneel == 0 & qb_spike == 0


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_after_run_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, defenders_in_box, mod_ydstogo, end_gap_ind, qb_dropback, half_seconds_remaining, score_differential, guard_gap_ind, n_wr, qb_scramble, n_db, n_st_def, tackle_gap_ind, n_te, n_lb, defteam_timeouts_remaining, n_dl, n_rb, posteam_timeouts_remaining, n_ol, no_huddle, n_st 
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_after_run_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_after_run_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_after_run_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_after_run_xtd), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    after_run_xtd = ifelse(two_point_attempt == 0 & play_type %in% c("run") & rush_attempt == 1 & qb_kneel == 0 & qb_spike == 0, predicted_after_run_xtd, NA))


prediction_results %>%
  filter(!is.na(predicted_after_run_xtd)) %>%
  mutate(bin = cut(predicted_after_run_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td_side = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())
# WEIRD BUT FOR RIGHT NOW GOING TO GO WITH IT

prediction_results %>%
  filter(!is.na(predicted_after_run_xtd)) %>%
  ggplot(aes(x = predicted_after_run_xtd, y = td_side)) + geom_point() + geom_smooth(method = "gam")



#####
#####
#####


xgb_after_pass_xtd <- xgb.load('xgb_part_after_pass_xtd.model')

# play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & !is.na(ngs_air_yards) & !is.na(air_yards) & qb_kneel == 0 & qb_spike == 0
# qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, flat_route_ind,  screen_route_ind, hitch_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, no_route_ind, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, ngs_air_yards


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_after_pass_xtd <- function(data) {
  X <- data %>% select(
    qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, flat_route_ind,  screen_route_ind, hitch_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, no_route_ind, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, ngs_air_yards
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_after_pass_xtd(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_after_pass_xtd, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_after_pass_xtd <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_after_pass_xtd), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    after_pass_xtd = ifelse(play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & !is.na(ngs_air_yards) & !is.na(air_yards) & qb_kneel == 0 & qb_spike == 0, predicted_after_pass_xtd, NA))


part_nfl %>%
  filter(!is.na(after_pass_xtd)) %>%
  mutate(bin = cut(after_pass_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td_side = mean(td_side, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####


xgb_xpass <- xgb.load('xgb_part_xpass_no_weather.model')

# (pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0
# defenders_in_box, yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential, n_ol, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_after_xpass <- function(data) {
  X <- data %>% select(
    defenders_in_box, yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential, n_ol, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_after_xpass(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_xpass, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_xpass <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_xpass), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_xpass = ifelse((pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0, predicted_xpass, NA))


part_nfl %>%
  filter(!is.na(predicted_xpass)) %>%
  mutate(bin = cut(predicted_xpass, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(pass_attempt = mean(pass_attempt, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_pressure_no_weather <- xgb.load('xgb_part_pressure_snap_no_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db

xgb_pressure_weather <- xgb.load('xgb_part_pressure_snap_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, temp, wind


# Function to create DMatrix
create_dmatrix_pressure <- function(data, weather_cols = NULL) {
  # Filtering based on weather columns or their absence
  if (!is.null(weather_cols)) {
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select relevant columns
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, temp, wind
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
make_predictions_pressure<- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_pressure(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_pressure_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_pressure(part_data)
  predictions_without_weather <- predict(xgb_pressure_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_pressure <- NA
  
  # Assign predictions based on the availability of weather data
  part_data$predicted_pressure[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_pressure[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_pressure <- make_predictions_pressure(part_nfl)

# Merge predictions back to the original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_pressure %>% select(nflverse_game_id, play_id, predicted_pressure), 
            by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_pressure = ifelse((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0, predicted_pressure, NA))


part_nfl %>%
  filter(!is.na(predicted_pressure)) %>%
  mutate(bin = cut(predicted_pressure, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(pressure_ind = mean(pressure_ind, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_sack_no_weather <- xgb.load('xgb_part_sack_no_weather.model')

# play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db

xgb_sack_weather <- xgb.load('xgb_part_sack_weather.model')

# play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, temp, wind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db


# Function to create DMatrix
create_dmatrix_sack <- function(data, weather_cols = NULL) {
  # Filtering based on weather columns or their absence
  if (!is.null(weather_cols)) {
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select relevant columns
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, temp, wind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
make_predictions_sack <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_sack(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_sack_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_sack(part_data)
  predictions_without_weather <- predict(xgb_sack_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_sack <- NA
  
  # Assign predictions based on the availability of weather data
  part_data$predicted_sack[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_sack[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_sack <- make_predictions_sack(part_nfl)

# Merge predictions back to the original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_sack %>% select(nflverse_game_id, play_id, predicted_sack), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_sack = ifelse(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0, predicted_sack, NA))


part_nfl %>%
  filter(!is.na(predicted_sack)) %>%
  mutate(bin = cut(predicted_sack, breaks = seq(0, 1, by = 0.02), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(sack = mean(sack, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_cp <- xgb.load('xgb_part_cp_no_weather.model')

# play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & qb_spike == 0 & qb_kneel == 0 & (!is.na(air_yards) & !is.na(ngs_air_yards)) 
# qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, flat_route_ind,  screen_route_ind, hitch_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, no_route_ind, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, ngs_air_yards


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_cp <- function(data) {
  X <- data %>% select(
    qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, flat_route_ind,  screen_route_ind, hitch_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, no_route_ind, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, ngs_air_yards
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_cp(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_cp, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_cp <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_cp), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_cp = ifelse(play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & qb_spike == 0 & qb_kneel == 0 & (!is.na(air_yards) & !is.na(ngs_air_yards)), predicted_cp, NA))


part_nfl %>%
  filter(!is.na(predicted_cp)) %>%
  mutate(bin = cut(predicted_cp, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(cp = mean(complete_pass, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_ypc <- xgb.load('xgb_part_ypc_no_weather.model')

# play_type == "run" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, end_ind, guard_ind, tackle_ind


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_ypc <- function(data) {
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, end_ind, guard_ind, tackle_ind
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_ypc(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_ypc, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_ypc <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_ypc), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_ypc = ifelse(play_type == "run" & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0, predicted_ypc, NA))


part_nfl %>%
  filter(!is.na(predicted_ypc)) %>%
  mutate(bin = cut(predicted_ypc, breaks = seq(-1.5, 8, by = .5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ypc = mean(yards_gained, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_ypa <- xgb.load('xgb_part_ypa_throw_no_weather.model')

# (play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp) & !is.na(yards_gained) & pass_attempt == 1 & rush_attempt == 0
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, temp, wind, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, ngs_air_yards


# Function to create DMatrix
# Function to create DMatrix without considering weather data
create_dmatrix_ypa <- function(data) {
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, ngs_air_yards
  ) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
# Function to make predictions using only the no-weather model
make_predictions <- function(part_data) {
  # Create DMatrix without considering weather data
  dtest <- create_dmatrix_ypa(part_data)
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_ypa, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_ypa <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(part_nfl)

# Merge predictions back to original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results %>% select(nflverse_game_id, play_id, predicted_ypa), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_ypa = ifelse((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(yards_gained) & pass_attempt == 1 & rush_attempt == 0, predicted_ypa, NA))


part_nfl %>%
  filter(!is.na(predicted_ypa)) %>%
  mutate(bin = cut(predicted_ypa, breaks = seq(-1.5, 15.5, by = .5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(ypa = mean(yards_gained, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


xgb_yac_no_weather <- xgb.load('xgb_part_yac_no_weather.model')

# (play_type == "pass") & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(yards_after_catch) & pass_attempt == 1 & rush_attempt == 0 & !is.na(yards_after_catch)
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, ngs_air_yards

xgb_yac_weather <- xgb.load('xgb_part_yac_weather.model')

# (play_type == "pass") & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(yards_after_catch) & pass_attempt == 1 & rush_attempt == 0 & !is.na(yards_after_catch) & !is.na(temp)
# yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, ngs_air_yards, temp, wind


# Function to create DMatrix
create_dmatrix_yac <- function(data, weather_cols = NULL) {
  # Filtering based on weather columns or their absence
  if (!is.null(weather_cols)) {
    data <- data %>% filter(!is.na(!!sym(weather_cols[1])) & !is.na(!!sym(weather_cols[2])))
  } else {
    data <- data %>% filter(is.na(temp) | is.na(wind))
  }
  
  # Select relevant columns
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, ngs_air_yards, temp, wind
  ) %>% select_if(~ !all(is.na(.))) %>% as.data.frame()
  
  return(xgb.DMatrix(data = as.matrix(X)))
}

# Function to make predictions
make_predictions_yac <- function(part_data) {
  # Data with weather info
  dtest_with_weather <- create_dmatrix_yac(part_data, c("temp", "wind"))
  predictions_with_weather <- predict(xgb_yac_weather, newdata = dtest_with_weather)
  
  # Data without weather info
  dtest_without_weather <- create_dmatrix_yac(part_data)
  predictions_without_weather <- predict(xgb_yac_no_weather, newdata = dtest_without_weather)
  
  # Initialize a new column with NA values
  part_data$predicted_yac <- NA
  
  # Assign predictions based on the availability of weather data
  part_data$predicted_yac[!is.na(part_data$temp) & !is.na(part_data$wind)] <- predictions_with_weather
  part_data$predicted_yac[is.na(part_data$temp) | is.na(part_data$wind)] <- predictions_without_weather
  
  return(part_data)
}

# Apply the prediction function
prediction_results_yac <- make_predictions_yac(part_nfl)

# Merge predictions back to the original part_nfl dataframe
part_nfl <- part_nfl %>%
  left_join(prediction_results_yac %>% select(nflverse_game_id, play_id, predicted_yac), by = c("nflverse_game_id", "play_id")) %>%
  mutate(
    predicted_yac = ifelse((play_type == "pass") & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(yards_after_catch) & pass_attempt == 1 & rush_attempt == 0 & !is.na(yards_after_catch), predicted_yac, NA))


part_nfl %>%
  filter(!is.na(predicted_yac)) %>%
  mutate(bin = cut(predicted_yac, breaks = seq(-1.5, 15.5, by = .5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(yac = mean(yards_after_catch, na.rm = TRUE), .groups = 'drop', n = n())


#####
#####
#####


part_nfl <- part_nfl %>% select(-predicted_after_pass_xtd)
part_nfl <- part_nfl %>% select(-predicted_after_run_xtd)
part_nfl <- part_nfl %>% select(-predicted_before_xtd)

part_nfl$drive_id <- paste0(part_nfl$nflverse_game_id, "-", part_nfl$posteam, "-", part_nfl$fixed_drive)

part_nfl$before_xtd[which(is.na(part_nfl$before_xtd))] <- 0

part_nfl$after_xtd <- coalesce(part_nfl$after_run_xtd, part_nfl$after_pass_xtd)
part_nfl$after_xtd[which(is.na(part_nfl$after_xtd))] <- 0


part_nfl <- part_nfl %>%
  group_by(drive_id) %>%
  mutate(
    before_remaining_xtd = 1,  # Start with 100% chance of potential TD at the beginning of each drive
    after_remaining_xtd = 1,
    before_xtd_new = before_xtd * before_remaining_xtd,  # Initial calculation for the first row in each drive
    after_xtd_new = after_xtd * after_remaining_xtd
  ) %>%
  mutate(
    # Calculate the remaining expected touchdowns for the next plays in the drive
    before_remaining_xtd = lag(before_remaining_xtd, default = 1) - lag(before_xtd_new, default = 0),
    after_remaining_xtd = lag(after_remaining_xtd, default = 1) - lag(after_xtd_new, default = 0),
    
    # Calculate the new expected touchdowns based on the remaining touchdowns
    before_xtd_new = before_xtd * before_remaining_xtd,
    after_xtd_new = after_xtd * after_remaining_xtd
  ) %>%
  ungroup()


part_xtd <- part_nfl %>% group_by(posteam, defteam, week, season) %>% 
  dplyr::summarise(before_old_xtd = sum(before_xtd),
                   before_new_xtd = sum(before_xtd_new),
                   after_old_xtd = sum(after_xtd),
                   after_new_xtd = sum(after_xtd_new),
                   actual_td = sum(touchdown),
                   actual_fg = sum(field_goal_attempt))

write.csv(part_xtd, 'part_xtd.csv')
