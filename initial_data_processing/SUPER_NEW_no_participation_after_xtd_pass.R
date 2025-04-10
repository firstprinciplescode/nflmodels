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



### BEFORE - NO PARTICIPATION / PERSONNEL INFO

pbp_nfl <- load_pbp(c(2016:2024))

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

pbp_nfl$down_one_ind = ifelse(pbp_nfl$down == 1, 1, 0)
pbp_nfl$down_two_ind = ifelse(pbp_nfl$down == 2, 1, 0)
pbp_nfl$down_three_ind = ifelse(pbp_nfl$down == 3, 1, 0)
pbp_nfl$down_four_ind = ifelse(pbp_nfl$down == 4, 1, 0)


pbp_nfl <- pbp_nfl %>% filter(play_type %in% c("run", "pass") & two_point_attempt == 0)



# yardline_100
# half_seconds_remaining
# shotgun
# no_huddle
# mod_ydstogo
# qb_dropback
# air_yards
# pass_length
# middle_ind
# outside_ind
# score_differential
# qb_hit
# roof
# surface
# posteam_timeouts_remaining
# defteam_timeouts_remaining


pbp_nfl_pass_no_temp <- pbp_nfl %>% filter(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards)) %>% select(td_side, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining)

pbp_nfl_pass_temp <- pbp_nfl %>% filter(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & !is.na(temp) & qb_scramble == 0 & !is.na(air_yards)) %>% select(td_side, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind)


pbp_nfl_pass_no_temp$td_side <- as.factor(pbp_nfl_pass_no_temp$td_side)
pbp_nfl_pass_temp$td_side <- as.factor(pbp_nfl_pass_temp$td_side)


# Define task
task_pbp_pass_no_temp <- TaskClassif$new(
  id = "pbp_pass_no_temp_task",
  backend = pbp_nfl_pass_no_temp,
  target = "td_side"
)

# Define binary classification learner
learner_pbp_pass_no_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 7),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 12),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2500)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_pbp_pass_no_temp,  # should be TaskClassif
  learner = learner_pbp_pass_no_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_pass_no_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_pbp_pass_no_temp)

# View best hyperparameters
at$archive$best()


###
###
###

# Make sure it's a factor, then use as.integer() - 1
pbp_nfl_pass_no_temp$td_side <- as.integer(as.factor(pbp_nfl_pass_no_temp$td_side)) - 1


sample_split_all_dfs_pass <- sample.split(Y = pbp_nfl_pass_no_temp$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = pbp_nfl_pass_no_temp, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = pbp_nfl_pass_no_temp, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$td_side
y_test_pass_all_dfs <- test_set_pass_all_dfs$td_side


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = pbp_nfl_pass_no_temp %>% select(-td_side)
d_ypass_all = pbp_nfl_pass_no_temp$td_side
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .046 # .064
gamma_pbp_pass = 3.51 # 4.25
max_depth_pbp_pass = 3 # 6
min_child_weight_pbp_pass = 2.347 # 0
alpha_pbp_pass = .154 # 0
lambda_pbp_pass = .121 # .75
colsample_bynode_pbp_pass = .459 # 1
colsample_bylevel_pbp_pass = .668 # 1
colsample_bytree_pbp_pass = .704 # 1

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
                        nround = 943, # 325
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

# .9566
# .143


View(resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


xgb.save(xgb_pbp_pass, 'C:/Users/AndLi/Downloads/Blank Model/xgb_no_part_after_xtd_pass.model')



#####
#####
#####



# Define task
task_pbp_pass_temp <- TaskClassif$new(
  id = "pbp_pass_temp_task",
  backend = pbp_nfl_pass_temp,
  target = "td_side"
)

# Define binary classification learner
learner_pbp_pass_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 7),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 12),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2500)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_pbp_pass_temp,  # should be TaskClassif
  learner = learner_pbp_pass_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_pass_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_pbp_pass_temp)

# View best hyperparameters
at$archive$best()



####
####
####



# Make sure it's a factor, then use as.integer() - 1
pbp_nfl_pass_temp$td_side <- as.integer(as.factor(pbp_nfl_pass_temp$td_side)) - 1


sample_split_all_dfs_pass <- sample.split(Y = pbp_nfl_pass_temp$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = pbp_nfl_pass_temp, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = pbp_nfl_pass_temp, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$td_side
y_test_pass_all_dfs <- test_set_pass_all_dfs$td_side


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = pbp_nfl_pass_temp %>% select(-td_side)
d_ypass_all = pbp_nfl_pass_temp$td_side
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .046 # .064
gamma_pbp_pass = .107 # 4.25
max_depth_pbp_pass = 3 # 6
min_child_weight_pbp_pass = 11.286 # 0
alpha_pbp_pass = .839 # 0
lambda_pbp_pass = .356 # .75
colsample_bynode_pbp_pass = .87 # 1
colsample_bylevel_pbp_pass = .719 # 1
colsample_bytree_pbp_pass = .919 # 1

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
                        nround = 264, # 325
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

importance_matrix_temp_all <- xgb.importance(names_pbp_pass, model = xgb_pbp_pass)
importance_matrix_temp_all

predictions <- predict(xgb_pbp_pass, newdata = dtest_pass_all_dfs, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pass_all_dfs))

# .9566
# .143


View(resultant_df_pbp_pass %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)

# TEMP, WIND --> .5% COMBINED GAIN. NO THANKS