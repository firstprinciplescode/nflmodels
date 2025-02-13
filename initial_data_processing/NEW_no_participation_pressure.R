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

attach("base_df_part_workspace.RData")

'%ni%' <- Negate('%in%')

options(scipen = 999)

part_nfl <- part_nfl

detach("file:base_df_part_workspace.RData")



attach("base_nfl_pbp_workspace.RData")

pbp_nfl <- pbp_nfl

detach("file:base_nfl_pbp_workspace.RData")

part_nfl$pressure_ind = case_when(part_nfl$was_pressure == T ~ 1, part_nfl$was_pressure == F ~ 0, is.na(part_nfl$was_pressure) ~ NA)


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

pbp_nfl$down_one_ind = ifelse(pbp_nfl$down == 1, 1, 0)
pbp_nfl$down_two_ind = ifelse(pbp_nfl$down == 2, 1, 0)
pbp_nfl$down_three_ind = ifelse(pbp_nfl$down == 3, 1, 0)
pbp_nfl$down_four_ind = ifelse(pbp_nfl$down == 4, 1, 0)


pbp_nfl_pressure <- left_join(pbp_nfl, part_nfl %>% select(nflverse_game_id, play_id, pressure_ind), by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))

rm(pbp_nfl)
rm(part_nfl)


xgboost_pbp_snap <- pbp_nfl_pressure %>% filter((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(pressure_ind)) %>% select(pressure_ind, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind)

xgboost_pbp_snap_weather <- pbp_nfl_pressure %>% filter((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp) & !is.na(pressure_ind)) %>% select(pressure_ind, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, temp, wind)


######
######
######


sample_split_all_dfs_pass <- sample.split(Y = xgboost_pbp_snap$pressure_ind, SplitRatio = 0.85)
train_set_pass_all_dfs <- subset(x = xgboost_pbp_snap, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_pbp_snap, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(pressure_ind)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(pressure_ind)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-pressure_ind) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-pressure_ind) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$pressure_ind
y_test_pass_all_dfs <- test_set_pass_all_dfs$pressure_ind


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_pbp_snap %>% select(-pressure_ind)
d_ypass_all = xgboost_pbp_snap$pressure_ind
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .035 # .035
gamma_pbp_pass = 4.5 # 4.5
max_depth_pbp_pass = 4 # 4
min_child_weight_pbp_pass = 5 # 5
alpha_pbp_pass = 1 # 
lambda_pbp_pass = 1 # 
colsample_bynode_pbp_pass = .75 # 
colsample_bylevel_pbp_pass = .8 # 
colsample_bytree_pbp_pass = .8 # 
# .7164
# .4469

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
                         nround = 650, # 650
                         objective = "binary:logistic",
                         nthread = 2,
                         gamma = gamma_pbp_pass,
                         early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs)

resultant_df_pbp_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_pbp_pass) = c("Preds", "Vals")

resultant_df_pbp_pass$buckets <- cut(resultant_df_pbp_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_pbp_pass)

names_pbp_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_pbp_pass, model = xgb_pbp_pass)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")

View(resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


resultant_df_part_pass <- data.frame(
  Preds = test_preds,
  Actuals = y_test_pass_all_dfs
)

# Calculate RMSE
rmse_value <- sqrt(mean((resultant_df_part_pass$Preds - resultant_df_part_pass$Actuals)^2))
print(paste("RMSE:", rmse_value))


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
                         nround = 650, # 650
                         objective = "binary:logistic",
                         nthread = 2,
                         gamma = gamma_pbp_pass,
                         early_stopping_rounds = 50
)

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")

plot(predictions, y_test_pass_all_dfs)
abline(lm(y_test_pass_all_dfs ~ predictions))

resultant_df_pbp_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_pbp_pass) = c("Preds", "Vals")

resultant_df_pbp_pass$buckets <- cut(resultant_df_pbp_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_pbp_pass)

names_pbp_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_pbp_pass, model = xgb_pbp_pass)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")

View(resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


xgb.save(xgb_pbp_pass, 'xgb_pbp_pressure_no_weather.model')


######
###### DECLARING NO WEATHER FOR THIS
######


sample_split_all_dfs_pass <- sample.split(Y = xgboost_pbp_snap_weather$pressure_ind, SplitRatio = 0.85)
train_set_pass_all_dfs <- subset(x = xgboost_pbp_snap_weather, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = xgboost_pbp_snap_weather, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(pressure_ind)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(pressure_ind)) %>% filter(!is.na(score_differential) & !is.na(down_three_ind))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-pressure_ind) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-pressure_ind) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$pressure_ind
y_test_pass_all_dfs <- test_set_pass_all_dfs$pressure_ind


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = xgboost_pbp_snap_weather %>% select(-pressure_ind)
d_ypass_all = xgboost_pbp_snap_weather$pressure_ind
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .035 # .035
gamma_pbp_pass = 4.5 # 4.5
max_depth_pbp_pass = 4 # 4
min_child_weight_pbp_pass = 5 # 5
alpha_pbp_pass = 1 # 
lambda_pbp_pass = 1 # 
colsample_bynode_pbp_pass = .75 # 
colsample_bylevel_pbp_pass = .8 # 
colsample_bytree_pbp_pass = .8 # 

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
                         nround = 650, # 650
                         objective = "binary:logistic",
                         nthread = 2,
                         gamma = gamma_pbp_pass,
                         early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs)

resultant_df_pbp_pass = cbind(test_preds, y_test_pass_all_dfs) %>% as.data.frame()
colnames(resultant_df_pbp_pass) = c("Preds", "Vals")

resultant_df_pbp_pass$buckets <- cut(resultant_df_pbp_pass$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_pbp_pass)

names_pbp_pass = colnames(dtrain_pass_all_dfs)

importance_matrix_before_all <- xgb.importance(names_pbp_pass, model = xgb_pbp_pass)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pass_all_dfs))



View(resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


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
                        nround = 375, # 375
                        objective = "binary:logistic",
                        nthread = 2,
                        gamma = gamma_pbp_pass,
                        early_stopping_rounds = 50
)