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



pbp_nfl <- load_pbp(c(2016:2024))

'%ni%' <- Negate('%in%')


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


xgboost_pbp_nfl <- pbp_nfl %>% filter((play_type == "run") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0) %>% select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind)

xgboost_pbp_nfl_weather = pbp_nfl %>% filter((play_type == "run") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)) %>% select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, temp, wind,  end_ind, guard_ind, tackle_ind)


######
######


task_pbp_run_ypc <- TaskRegr$new(
  id = "pbp_run_ypc_task",
  backend = xgboost_pbp_nfl,  # same data, but make sure `yards_after_catch` is present and numeric
  target = "yards_gained"
)

learner_pbp_run_ypc <- lrn("regr.xgboost")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.16),
  gamma = p_dbl(lower = 0, upper = 8),
  max_depth = p_int(lower = 2, upper = 12),
  min_child_weight = p_dbl(lower = 0, upper = 17),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2400)
)


measure = msr("regr.rmse")  # or "regr.mae", "regr.rsq", etc.

at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_run_ypc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

at$train(task_pbp_run_ypc)

at$archive$best()


####
####


sample_split_all_dfs_pass <- sample.split(Y = xgboost_pbp_nfl$yards_gained, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = xgboost_pbp_nfl, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_pbp_nfl, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-yards_gained) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$yards_gained
y_test_pass_all_dfs <- test_set_pass_all_dfs$yards_gained


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_pbp_nfl %>% select(-yards_gained)
d_ypass_all = xgboost_pbp_nfl$yards_gained
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .057 # .027
gamma_pbp_pass = 3.298 # 2.25
max_depth_pbp_pass = 3 # 4
min_child_weight_pbp_pass = 11.47 # 5.25
alpha_pbp_pass = .546 # .2
lambda_pbp_pass = .818 # .35
colsample_bynode_pbp_pass = .054 # .8
colsample_bylevel_pbp_pass = .805 # 1
colsample_bytree_pbp_pass = .807 # 1

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
                        nround = 1968, # 675
                        objective = "reg:squarederror",
                        nthread = 2,
                        gamma = gamma_pbp_pass,
                        early_stopping_rounds = 50
)


breaks <- seq(-3, 16, by = 0.5)

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
# 6.121

xgb.save(xgb_pbp_pass, 'xgb_pbp_ypc.model')


###
###


task_pbp_run_ypc_temp <- TaskRegr$new(
  id = "pbp_run_ypc_weather_task",
  backend = xgboost_pbp_nfl_weather,  # same data, but make sure `yards_after_catch` is present and numeric
  target = "yards_gained"
)

learner_pbp_run_ypc <- lrn("regr.xgboost")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.16),
  gamma = p_dbl(lower = 0, upper = 8),
  max_depth = p_int(lower = 2, upper = 12),
  min_child_weight = p_dbl(lower = 0, upper = 17),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2400)
)


measure = msr("regr.rmse")  # or "regr.mae", "regr.rsq", etc.

at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_run_ypc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

at$train(task_pbp_run_ypc_temp)

at$archive$best()


####
####


sample_split_all_dfs_pass_temp <- sample.split(Y = xgboost_pbp_nfl_weather$yards_gained, SplitRatio = 0.8)
train_set_pass_all_dfs_temp <- subset(x = xgboost_pbp_nfl_weather, sample_split_all_dfs_pass_temp == TRUE)
test_set_pass_all_dfs_temp <- subset(x = xgboost_pbp_nfl_weather, sample_split_all_dfs_pass_temp == FALSE)

train_set_pass_all_dfs_temp <- train_set_pass_all_dfs_temp %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs_temp <- test_set_pass_all_dfs_temp %>% filter(!is.na(yards_gained)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs_temp <- train_set_pass_all_dfs_temp %>% select(-yards_gained) %>% as.data.frame()
X_test_pass_all_dfs_temp <- test_set_pass_all_dfs_temp %>% select(-yards_gained) %>% as.data.frame()
y_train_pass_all_dfs_temp <- train_set_pass_all_dfs_temp$yards_gained
y_test_pass_all_dfs_temp <- test_set_pass_all_dfs_temp$yards_gained


dtrain_pass_all_dfs_temp = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs_temp), label = y_train_pass_all_dfs_temp)
dtest_pass_all_dfs_temp = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs_temp), label = y_test_pass_all_dfs_temp)

d_xpass_all_temp = xgboost_pbp_nfl_weather %>% select(-yards_gained)
d_ypass_all_temp = xgboost_pbp_nfl_weather$yards_gained
d_all_pass_all_temp = xgb.DMatrix(data = as.matrix(d_xpass_all_temp), label = d_ypass_all_temp)

watchlist_pass_all_dfs_temp = list(train=dtrain_pass_all_dfs_temp, test=dtest_pass_all_dfs_temp)


eta_pbp_pass_temp = .011 # .027
gamma_pbp_pass_temp = 5.917 # 2.25
max_depth_pbp_pass_temp = 8 # 4
min_child_weight_pbp_pass_temp = 8.303 # 5.25
alpha_pbp_pass_temp = .762 # .2
lambda_pbp_pass_temp = .544 # .35
colsample_bynode_pbp_pass_temp = .369 # .8
colsample_bylevel_pbp_pass_temp = .344 # 1
colsample_bytree_pbp_pass_temp = .858 # 1

xgb_pbp_pass_temp <- xgboost(data = dtrain_pass_all_dfs_temp, 
                             label = y_train_pass_all_dfs_temp, 
                             eta = eta_pbp_pass_temp,
                             max_depth = max_depth_pbp_pass_temp, 
                             alpha = alpha_pbp_pass_temp,
                             lambda = lambda_pbp_pass_temp,
                             min_child_weight = min_child_weight_pbp_pass_temp,
                             colsample_bynode = colsample_bynode_pbp_pass_temp,
                             colsample_bytree = colsample_bytree_pbp_pass_temp,
                             colsample_bylevel = colsample_bylevel_pbp_pass_temp,
                             nround = 1658, # 675
                             objective = "reg:squarederror",
                             nthread = 2,
                             gamma = gamma_pbp_pass_temp,
                             early_stopping_rounds = 50
)


breaks <- seq(-3, 13, by = 0.5)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds_temp = predict(xgb_pbp_pass_temp, newdata = dtest_pass_all_dfs_temp)

resultant_df_part_pass_temp = cbind(test_preds, y_test_pass_all_dfs_temp) %>% as.data.frame()
colnames(resultant_df_part_pass_temp) = c("Preds", "Vals")

resultant_df_part_pass_temp$buckets <- cut(resultant_df_part_pass_temp$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_part_pass_temp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

View(resultant_df_part_pass_temp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


names_part_pass_temp = colnames(dtrain_pass_all_dfs_temp)

importance_matrix_before_all_temp <- xgb.importance(names_part_pass_temp, model = xgb_pbp_pass_temp)
importance_matrix_before_all_temp

predictions_temp <- predict(xgb_pbp_pass_temp, newdata = dtest_pass_all_dfs_temp, type = "response")


# Convert predictions and actual values to a dataframe
resultant_df_part_pass <- data.frame(
  Preds = test_preds_temp,
  Actuals = y_test_pass_all_dfs_temp
)

# Calculate RMSE
rmse_value_temp <- rmse(test_preds_temp, y_test_pass_all_dfs_temp)
print(paste("RMSE:", rmse_value_temp))
# 6.093


####
####


xgboost_pbp_nfl_comp <- pbp_nfl %>% filter((play_type == "run") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0 & !is.na(temp)) %>% select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind)

sample_split_pbp_comp <- sample.split(Y = xgboost_pbp_nfl_comp$yards_gained, SplitRatio = 0.8)
train_set_pbp_comp <- subset(x = xgboost_pbp_nfl_comp, sample_split_pbp_comp == TRUE)
test_set_pbp_comp <- subset(x = xgboost_pbp_nfl_comp, sample_split_pbp_comp == FALSE)

train_set_pbp_comp <- train_set_pbp_comp %>% filter(!is.na(yards_gained))
test_set_pbp_comp <- test_set_pbp_comp %>% filter(!is.na(yards_gained))

X_train_pbp_comp <- train_set_pbp_comp %>% select(-yards_gained) %>% as.data.frame()
X_test_pbp_comp <- test_set_pbp_comp %>% select(-yards_gained) %>% as.data.frame()
y_train_pbp_comp <- train_set_pbp_comp$yards_gained
y_test_pbp_comp <- test_set_pbp_comp$yards_gained

dtrain_pbp_comp = xgb.DMatrix(data = as.matrix(X_train_pbp_comp), label = y_train_pbp_comp)
dtest_pbp_comp = xgb.DMatrix(data =as.matrix(X_test_pbp_comp), label = y_test_pbp_comp)

test_preds_comp = predict(xgb_pbp_pass, newdata = dtest_pbp_comp)

rmse(test_preds_comp, y_test_pbp_comp)

# 6.074

# NO WEATHER AGAIN