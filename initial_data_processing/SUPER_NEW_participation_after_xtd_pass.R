# NFL data
library(nflreadr)
library(nflfastR)

# Data manipulation & wrangling
library(tidyverse)
library(stringr)

# Clustering / unsupervised
library(flexclust)
library(factoextra)
library(NbClust)

# Modeling / ML
library(xgboost)
library(caret)
library(Metrics)
library(cvms)
library(pROC)
library(ROCR)
library(ParBayesianOptimization)
library(olsrr)
library(mgcv)
library(stats)

# mlr3
library(mlr3)
library(mlr3verse)
library(paradox)

# Utils
library(sqldf)
library(openxlsx)
library(readxl)
library(doParallel)
library(gridExtra)


attach("C:/Users/AndLi/Downloads/Blank Model/base_df_part_workspace.RData")

'%ni%' <- Negate('%in%')

options(scipen = 999)

part_nfl <- part_nfl


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

part_nfl <- part_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0)
# ONCE YOU REMOVE NON-RUN, NON-PASS --> THE # OF DEFENDERS IN BOX NA IS .3%. DON'T CARE.
# ONCE YOU REMOVE TWO_POINT_ATTEMT --> ONLY 6 PASSES WITH NA PASS_RUSHERS
# THERE ARE NO NAs --> TIME_TO_THROW FOR PASS


part_nfl <- part_nfl %>% filter(!is.na(defenders_in_box)) %>% filter(!(play_type == "pass" & is.na(number_of_pass_rushers)))

part_nfl$number_of_pass_rushers[which(part_nfl$number_of_pass_rushers < 0)] = 0

part_nfl$defenders_in_box[which(part_nfl$defenders_in_box < 0)] = 0


####
####


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
    no_route_ind = as.integer(is.na(route) & play_type == "pass" & sack == 0 & two_point_attempt == 0),
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


part_nfl$pressure_ind <- ifelse(is.na(part_nfl$pressure_ind), part_nfl$sack, part_nfl$pressure_ind)


####
####


xgboost_part_nfl <- part_nfl %>% filter(play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & !is.na(ngs_air_yards) & !is.na(air_yards)) %>% select(td_side, qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, ngs_air_yards)

xgboost_part_nfl_weather <- part_nfl %>% filter(play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & !is.na(ngs_air_yards) & !is.na(air_yards) & !is.na(temp)) %>% select(td_side, qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, ngs_air_yards, temp, wind)

xgboost_part_nfl$td_side <- as.factor(xgboost_part_nfl$td_side)
xgboost_part_nfl_weather$td_side <- as.factor(xgboost_part_nfl_weather$td_side)


####
####


# Define task
task_part_td <- TaskClassif$new(
  id = "part_run_no_temp_task",
  backend = xgboost_part_nfl,
  target = "td_side"
)

# Define binary classification learner
learner_part_td <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.16),
  gamma = p_dbl(lower = 0, upper = 9),
  max_depth = p_int(lower = 2, upper = 13),
  min_child_weight = p_dbl(lower = 0, upper = 16),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2800)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_part_td,  # should be TaskClassif
  learner = learner_part_td,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_part_td,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_part_td)

# View best hyperparameters
at$archive$best()


####
####
####


xgboost_part_nfl$td_side <- as.integer(as.factor(xgboost_part_nfl$td_side)) - 1

sample_split_all_dfs_pass <- sample.split(Y = xgboost_part_nfl$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = xgboost_part_nfl, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_part_nfl, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$td_side
y_test_pass_all_dfs <- test_set_pass_all_dfs$td_side


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_part_nfl %>% select(-td_side)
d_ypass_all = xgboost_part_nfl$td_side
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)



eta_part_pass = .034 # .025
gamma_part_pass = 8.534 # 0
max_depth_part_pass = 8 # 4
min_child_weight_part_pass = 2.846 # 0
alpha_part_pass = .129 # 0
lambda_part_pass = .939 # 0
colsample_bynode_part_pass = .542 # .275
colsample_bylevel_part_pass = .516 # 1
colsample_bytree_part_pass = .822 # 1

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
                         nround = 2378, # 330
                         objective = "binary:logistic",
                         nthread = 2,
                         gamma = gamma_part_pass,
                         early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_part_pass, newdata = dtest_pass_all_dfs)

resultant_df_part_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_part_pass) = c("Preds", "Vals")

resultant_df_part_pass$buckets <- cut(resultant_df_part_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_part_pass)

names_part_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_part_pass, model = xgb_part_pass)
importance_matrix_before_all

predictions <- predict(xgb_part_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pass_all_dfs))

# .9563


View(resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)

xgb.save(xgb_part_pass, 'xgb_part_after_xtd_pass.model')


####
####


# Define task
task_part_td_weather <- TaskClassif$new(
  id = "part_run_no_temp_task",
  backend = xgboost_part_nfl_weather,
  target = "td_side"
)

# Define binary classification learner
learner_part_td_weather <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.16),
  gamma = p_dbl(lower = 0, upper = 9),
  max_depth = p_int(lower = 2, upper = 13),
  min_child_weight = p_dbl(lower = 0, upper = 16),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2800)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_part_td_weather,  # should be TaskClassif
  learner = learner_part_td_weather,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_part_td_weather,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_part_td_weather)

# View best hyperparameters
at$archive$best()


####
####
####


xgboost_part_nfl_weather$td_side <- as.integer(as.factor(xgboost_part_nfl_weather$td_side)) - 1

sample_split_all_dfs_pass_weather <- sample.split(Y = xgboost_part_nfl_weather$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs_weather <- subset(x = xgboost_part_nfl_weather, sample_split_all_dfs_pass_weather == TRUE)
test_set_pass_all_dfs_weather <- subset(x = xgboost_part_nfl_weather, sample_split_all_dfs_pass_weather == FALSE)

train_set_pass_all_dfs_weather <- train_set_pass_all_dfs_weather %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))
test_set_pass_all_dfs_weather <- test_set_pass_all_dfs_weather %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))

X_train_pass_all_dfs_weather <- train_set_pass_all_dfs_weather %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs_weather <- test_set_pass_all_dfs_weather %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs_weather <- train_set_pass_all_dfs_weather$td_side
y_test_pass_all_dfs_weather <- test_set_pass_all_dfs_weather$td_side


dtrain_pass_all_dfs_weather = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs_weather), label = y_train_pass_all_dfs_weather)
dtest_pass_all_dfs_weather = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs_weather), label = y_test_pass_all_dfs_weather)

d_xpass_all_weather = xgboost_part_nfl_weather %>% select(-td_side)
d_ypass_all_weather = xgboost_part_nfl_weather$td_side
d_all_pass_all_weather = xgb.DMatrix(data = as.matrix(d_xpass_all_weather), label = d_ypass_all_weather)

watchlist_pass_all_dfs_weather = list(train=dtrain_pass_all_dfs_weather, test=dtest_pass_all_dfs_weather)


eta_part_pass_weather = .019 # .025
gamma_part_pass_weather = 6.355 # 0
max_depth_part_pass_weather = 7 # 4
min_child_weight_part_pass_weather = 13.599 # 0
alpha_part_pass_weather = .877 # 0
lambda_part_pass_weather = .748 # 0
colsample_bynode_part_pass_weather = .417 # .275
colsample_bylevel_part_pass_weather = .545 # 1
colsample_bytree_part_pass_weather = .911 # 1


xgb_part_pass_weather <- xgboost(data = dtrain_pass_all_dfs_weather, 
                         label = y_train_pass_all_dfs_weather, 
                         eta = eta_part_pass_weather,
                         max_depth = max_depth_part_pass_weather, 
                         alpha = alpha_part_pass_weather,
                         lambda = lambda_part_pass_weather,
                         min_child_weight = min_child_weight_part_pass_weather,
                         colsample_bynode = colsample_bynode_part_pass_weather,
                         colsample_bytree = colsample_bytree_part_pass_weather,
                         colsample_bylevel = colsample_bylevel_part_pass_weather,
                         nround = 1394, # 330
                         objective = "binary:logistic",
                         nthread = 2,
                         gamma = gamma_part_pass_weather,
                         early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds_weather = predict(xgb_part_pass_weather, newdata = dtest_pass_all_dfs_weather)

resultant_df_part_pass_weather = cbind(test_preds_weather, y_test_pass_all_dfs_weather) %>% as.data.frame()
colnames(resultant_df_part_pass_weather) = c("Preds", "Vals")

resultant_df_part_pass_weather$buckets <- cut(resultant_df_part_pass_weather$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_part_pass_weather)

names_part_pass_weather = colnames(dtrain_pass_all_dfs_weather)

importance_matrix_before_all_weather <- xgb.importance(names_part_pass_weather, model = xgb_part_pass_weather)
importance_matrix_before_all_weather

predictions_weather <- predict(xgb_part_pass_weather, newdata = dtest_pass_all_dfs_weather, type = "response")
predicted_classes_weather <- ifelse(predictions_weather > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes_weather), factor(y_test_pass_all_dfs_weather))

# .9563


View(resultant_df_part_pass_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


####
####


xgboost_part_nfl_comp <- part_nfl %>% filter(play_type == "pass" & pass_attempt == 1 & sack == 0 & two_point_attempt == 0 & !is.na(ngs_air_yards) & !is.na(air_yards) & !is.na(temp)) %>% select(td_side, qb_hit, time_to_throw, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_dl, n_lb, n_db, pressure_ind, ngs_air_yards)

sample_split_all_dfs_pass_comp <- sample.split(Y = xgboost_part_nfl_comp$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs_comp <- subset(x = xgboost_part_nfl_comp, sample_split_all_dfs_pass_comp == TRUE)
test_set_pass_all_dfs_comp <- subset(x = xgboost_part_nfl_comp, sample_split_all_dfs_pass_comp == FALSE)

train_set_pass_all_dfs_comp <- train_set_pass_all_dfs_comp %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))
test_set_pass_all_dfs_comp <- test_set_pass_all_dfs_comp %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind) & !is.na(ngs_air_yards) & !is.na(pressure_ind))

X_train_pass_all_dfs_comp <- train_set_pass_all_dfs_comp %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs_comp <- test_set_pass_all_dfs_comp %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs_comp <- train_set_pass_all_dfs_comp$td_side
y_test_pass_all_dfs_comp <- test_set_pass_all_dfs_comp$td_side


dtrain_pass_all_dfs_comp = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs_comp), label = y_train_pass_all_dfs_comp)
dtest_pass_all_dfs_comp = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs_comp), label = y_test_pass_all_dfs_comp)


##
##


# AUC
auc_no_weather <- auc(y_test_pass_all_dfs_comp, predict(xgb_part_pass, newdata = dtest_pass_all_dfs_comp))
auc_weather <- auc(y_test_pass_all_dfs_weather, test_preds_weather)

# Log loss
logloss_no_weather <- logLoss(y_test_pass_all_dfs_comp, predict(xgb_part_pass, newdata = dtest_pass_all_dfs_comp))
logloss_weather <- logLoss(y_test_pass_all_dfs_weather, test_preds_weather)

cat("AUC - No Weather:", round(auc_no_weather, 4), "\n")
cat("AUC - With Weather:", round(auc_weather, 4), "\n\n")

cat("LogLoss - No Weather:", round(logloss_no_weather, 4), "\n")
cat("LogLoss - With Weather:", round(logloss_weather, 4), "\n")

### AFTER THIS TEST, THE WEATHER AIN'T HELPING
