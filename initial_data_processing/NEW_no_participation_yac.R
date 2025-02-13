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

setwd("C:/Users/AndLi/Downloads/NFL Models")

### BEFORE - NO pbpICIPATION / PERSONNEL INFO

pbp_nfl <- load_pbp(c(2016:2023))

'%ni%' <- Negate('%in%')

colnames(pbp_nfl)

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


pbp_nfl_pass_no_temp <- pbp_nfl %>% filter(play_type == "pass" & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards) & !is.na(yards_after_catch)) %>% select(yards_after_catch, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, air_yards, down_one_ind, down_two_ind, down_three_ind)

pbp_nfl_pass_temp <- pbp_nfl %>% filter(play_type == "pass" & complete_pass == 1 & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & !is.na(temp) & qb_scramble == 0 & !is.na(air_yards) & !is.na(yards_after_catch)) %>% select(yards_after_catch, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind, air_yards, down_one_ind, down_two_ind, down_three_ind)


#######
#######
#######


sample_split_all_dfs_pass <- sample.split(Y = pbp_nfl_pass_no_temp$yards_after_catch, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = pbp_nfl_pass_no_temp, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = pbp_nfl_pass_no_temp, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(yards_after_catch)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(yards_after_catch)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-yards_after_catch) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-yards_after_catch) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$yards_after_catch
y_test_pass_all_dfs <- test_set_pass_all_dfs$yards_after_catch


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = pbp_nfl_pass_no_temp %>% select(-yards_after_catch)
d_ypass_all = pbp_nfl_pass_no_temp$yards_after_catch
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .036 # .036
gamma_pbp_pass = 0 # 0
max_depth_pbp_pass = 4 # 4
min_child_weight_pbp_pass = 2 # 2
alpha_pbp_pass = .2 # .2
lambda_pbp_pass = 0 # 0
colsample_bynode_pbp_pass = .6 # .6
colsample_bylevel_pbp_pass = .9 # .9
colsample_bytree_pbp_pass = .9 # .9

xgb_pbp_pass <- xgboost(data = dtrain_pass_all_dfs, 
                        label = y_train_pass_all_dfs, 
                        eta = eta_pbp_pass,
                        max_depth = max_depth_pbp_pass, 
                        alpha = alpha_pbp_pass,
                        lambda = lambda_pbp_pass,
                        min_child_weight = min_child_weight_pbp_pass,
                        colsample_bynode = colsample_bynode_pbp_pass,
                        colsample_bytree = colsample_bytree_pbp_pass,
                        colsample_bylevel = colsample_bylevel_pbp_pass,
                        nround = 725, # 725
                        objective = "reg:squarederror",
                        nthread = 2,
                        gamma = gamma_pbp_pass,
                        early_stopping_rounds = 50
)


breaks <- seq(-3, 13, by = 0.5)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs)

resultant_df_part_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_part_pass) = c("Preds", "Vals")

resultant_df_part_pass$buckets <- cut(resultant_df_part_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

View(resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


names_part_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_part_pass, model = xgb_pbp_pass)
importance_matrix_before_all


# Convert predictions and actual values to a dataframe
resultant_df_part_pass <- data.frame(
  Preds = test_preds,
  Actuals = y_test_pass_all_dfs
)

# Calculate RMSE
rmse_value <- sqrt(mean((resultant_df_part_pass$Preds - resultant_df_part_pass$Actuals)^2))
print(paste("RMSE:", rmse_value))
# 6.2315


xgb_pbp_pass <- xgboost(data = as.matrix(d_xpass_all), 
                        label = d_ypass_all, 
                        eta = eta_pbp_pass,
                        max_depth = max_depth_pbp_pass, 
                        alpha = alpha_pbp_pass,
                        lambda = lambda_pbp_pass,
                        min_child_weight = min_child_weight_pbp_pass,
                        colsample_bynode = colsample_bynode_pbp_pass,
                        colsample_bytree = colsample_bytree_pbp_pass,
                        colsample_bylevel = colsample_bylevel_pbp_pass,
                        nround = 725, # 725
                        objective = "reg:squarederror",
                        nthread = 2,
                        gamma = gamma_pbp_pass,
                        early_stopping_rounds = 50
)

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
  
  print(colnames(dtest))
  
  # Make predictions using the no-weather model
  predictions <- predict(xgb_pbp_pass, newdata = dtest)
  
  # Assign predictions to the new column
  part_data$predicted_yac <- predictions
  
  return(part_data)
}

# Apply the prediction function
prediction_results <- make_predictions(pbp_nfl_pass_no_temp)



xgb.save(xgb_pbp_pass, 'xgb_pbp_yac_no_weather.model')


#####
#####
#####


sample_split_all_dfs_pass <- sample.split(Y = pbp_nfl_pass_temp$yards_after_catch, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = pbp_nfl_pass_temp, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = pbp_nfl_pass_temp, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(yards_after_catch)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(yards_after_catch)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-yards_after_catch) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-yards_after_catch) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$yards_after_catch
y_test_pass_all_dfs <- test_set_pass_all_dfs$yards_after_catch


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = pbp_nfl_pass_temp %>% select(-yards_after_catch)
d_ypass_all = pbp_nfl_pass_temp$yards_after_catch
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)

eta_pbp_pass = .015 # .02
gamma_pbp_pass = 0 # 0
max_depth_pbp_pass = 4 # 4
min_child_weight_pbp_pass = 2 # 2
alpha_pbp_pass = .2 # .2
lambda_pbp_pass = 0 # 0
colsample_bynode_pbp_pass = .6 # .6
colsample_bylevel_pbp_pass = .9 # .9
colsample_bytree_pbp_pass = .9 # .9

xgb_pbp_pass <- xgboost(data = dtrain_pass_all_dfs, 
                        label = y_train_pass_all_dfs, 
                        eta = eta_pbp_pass,
                        max_depth = max_depth_pbp_pass, 
                        alpha = alpha_pbp_pass,
                        lambda = lambda_pbp_pass,
                        min_child_weight = min_child_weight_pbp_pass,
                        colsample_bynode = colsample_bynode_pbp_pass,
                        colsample_bytree = colsample_bytree_pbp_pass,
                        colsample_bylevel = colsample_bylevel_pbp_pass,
                        nround = 680, # 680
                        objective = "reg:squarederror",
                        nthread = 2,
                        gamma = gamma_pbp_pass,
                        early_stopping_rounds = 50
)

breaks <- seq(-3, 13, by = 0.5)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs)

resultant_df_part_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_part_pass) = c("Preds", "Vals")

resultant_df_part_pass$buckets <- cut(resultant_df_part_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

View(resultant_df_part_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


names_part_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_part_pass, model = xgb_pbp_pass)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)


# Convert predictions and actual values to a dataframe
resultant_df_part_pass <- data.frame(
  Preds = test_preds,
  Actuals = y_test_pass_all_dfs
)

# Calculate RMSE
rmse_value <- sqrt(mean((resultant_df_part_pass$Preds - resultant_df_part_pass$Actuals)^2))
print(paste("RMSE:", rmse_value))
# 6.332


#### WEATHER - DOESN'T MATTER, ESP AS ETA FALLS