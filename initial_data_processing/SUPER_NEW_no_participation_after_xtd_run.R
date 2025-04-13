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



pbp_nfl <- pbp_nfl %>% filter(qb_spike == 0 & qb_kneel == 0)
pbp_nfl <- pbp_nfl %>% filter(play_type == "run") %>% filter(!is.na(run_gap)) %>% filter(!is.na(run_location))

pbp_nfl_run <- pbp_nfl %>% select(td_side, season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% as.data.frame()

pbp_nfl_run_weather <- pbp_nfl %>% filter(!is.na(wind)) %>% select(td_side, season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining, wind, temp) %>% as.data.frame()



pbp_nfl_run$td_side <- as.factor(pbp_nfl_run$td_side)
pbp_nfl_run_weather$td_side <- as.factor(pbp_nfl_run_weather$td_side)


###
###
###


# Define task
task_pbp_run_no_temp <- TaskClassif$new(
  id = "pbp_run_no_temp_task",
  backend = pbp_nfl_run,
  target = "td_side"
)

# Define binary classification learner
learner_pbp_run_no_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 7),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 15),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2300)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_pbp_run_no_temp,  # should be TaskClassif
  learner = learner_pbp_run_no_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_run_no_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_pbp_run_no_temp)

# View best hyperparameters
at$archive$best()


####
####
####



# Make sure it's a factor, then use as.integer() - 1
pbp_nfl_run$td_side <- as.integer(as.factor(pbp_nfl_run$td_side)) - 1


sample_split_all_dfs_pass <- sample.split(Y = pbp_nfl_run$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs <- subset(x = pbp_nfl_run, sample_split_all_dfs_pass == TRUE)
test_set_pass_all_dfs <- subset(x = pbp_nfl_run, sample_split_all_dfs_pass == FALSE)

train_set_pass_all_dfs <- train_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs <- test_set_pass_all_dfs %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs <- train_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs <- test_set_pass_all_dfs %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs <- train_set_pass_all_dfs$td_side
y_test_pass_all_dfs <- test_set_pass_all_dfs$td_side


dtrain_pass_all_dfs = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs), label = y_train_pass_all_dfs)
dtest_pass_all_dfs = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs), label = y_test_pass_all_dfs)

d_xpass_all = pbp_nfl_run %>% select(-td_side)
d_ypass_all = pbp_nfl_run$td_side
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_pass_all_dfs = list(train=dtrain_pass_all_dfs, test=dtest_pass_all_dfs)


eta_pbp_pass = .008 # .064
gamma_pbp_pass = 5.575 # 4.25
max_depth_pbp_pass = 7 # 6
min_child_weight_pbp_pass = 2.818 # 0
alpha_pbp_pass = .718 # 0
lambda_pbp_pass = .059 # .75
colsample_bynode_pbp_pass = .782 # 1
colsample_bylevel_pbp_pass = .637 # 1
colsample_bytree_pbp_pass = .954 # 1

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
                        nround = 1025, # 325
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


xgb.save(xgb_pbp_pass, 'C:/Users/AndLi/Downloads/Blank Model/xgb_no_part_after_xtd_run.model')



####
####
####



# Define task
task_pbp_run_temp <- TaskClassif$new(
  id = "pbp_run_temp_task",
  backend = pbp_nfl_run_weather,
  target = "td_side"
)

# Define binary classification learner
learner_pbp_run_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 7),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 15),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2300)
)

# Define tuning instance (optional standalone)
instance <- TuningInstanceBatchSingleCrit$new(
  task = task_pbp_run_temp,  # should be TaskClassif
  learner = learner_pbp_run_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),  # or msr("classif.logloss")
  search_space = param_set,
  terminator = trm("evals", n_evals = 38)
)

# Run Bayesian optimization with auto_tuner
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_pbp_run_no_temp,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 29)
)

# Train the tuning instance
at$train(task_pbp_run_temp)

# View best hyperparameters
at$archive$best()



####
####
####



# Make sure it's a factor, then use as.integer() - 1
pbp_nfl_run_weather$td_side <- as.integer(as.factor(pbp_nfl_run_weather$td_side)) - 1


sample_split_all_dfs_pass_weather <- sample.split(Y = pbp_nfl_run_weather$td_side, SplitRatio = 0.8)
train_set_pass_all_dfs_weather <- subset(x = pbp_nfl_run_weather, sample_split_all_dfs_pass_weather == TRUE)
test_set_pass_all_dfs_weather <- subset(x = pbp_nfl_run_weather, sample_split_all_dfs_pass_weather == FALSE)

train_set_pass_all_dfs_weather <- train_set_pass_all_dfs_weather %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))
test_set_pass_all_dfs_weather <- test_set_pass_all_dfs_weather %>% filter(!is.na(td_side)) %>% filter(!is.na(score_differential))

X_train_pass_all_dfs_weather <- train_set_pass_all_dfs_weather %>% select(-td_side) %>% as.data.frame()
X_test_pass_all_dfs_weather <- test_set_pass_all_dfs_weather %>% select(-td_side) %>% as.data.frame()
y_train_pass_all_dfs_weather <- train_set_pass_all_dfs_weather$td_side
y_test_pass_all_dfs_weather <- test_set_pass_all_dfs_weather$td_side


dtrain_pass_all_dfs_weather = xgb.DMatrix(data = as.matrix(X_train_pass_all_dfs_weather), label = y_train_pass_all_dfs_weather)
dtest_pass_all_dfs_weather = xgb.DMatrix(data =as.matrix(X_test_pass_all_dfs_weather), label = y_test_pass_all_dfs_weather)

d_xpass_all_weather = pbp_nfl_run_weather %>% select(-td_side)
d_ypass_all_weather = pbp_nfl_run_weather$td_side
d_all_pass_all_weather = xgb.DMatrix(data = as.matrix(d_xpass_all_weather), label = d_ypass_all_weather)

watchlist_pass_all_dfs_weather = list(train=dtrain_pass_all_dfs_weather, test=dtest_pass_all_dfs_weather)


eta_pbp_pass_weather = .114 # .064
gamma_pbp_pass_weather = 4.879 # 4.25
max_depth_pbp_pass_weather = 6 # 6
min_child_weight_pbp_pass_weather = 13.01 # 0
alpha_pbp_pass_weather = .955 # 0
lambda_pbp_pass_weather = .986 # .75
colsample_bynode_pbp_pass_weather = .321 # 1
colsample_bylevel_pbp_pass_weather = .424 # 1
colsample_bytree_pbp_pass_weather = .876 # 1

xgb_pbp_pass_weather <- xgboost(data = dtrain_pass_all_dfs_weather, 
                        label = y_train_pass_all_dfs_weather, 
                        eta = eta_pbp_pass_weather,
                        max_depth = max_depth_pbp_pass_weather, 
                        alpha = alpha_pbp_pass_weather,
                        lambda = lambda_pbp_pass_weather,
                        min_child_weight = min_child_weight_pbp_pass_weather,
                        colsample_bynode = colsample_bynode_pbp_pass_weather,
                        colsample_bytree = colsample_bytree_pbp_pass_weather,
                        colsample_bylevel = colsample_bylevel_pbp_pass_weather,
                        nround = 775, # 325
                        objective = "binary:logistic",
                        nthread = 2,
                        gamma = gamma_pbp_pass_weather,
                        early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds_weather = predict(xgb_pbp_pass_weather, newdata = dtest_pass_all_dfs_weather)

resultant_df_pbp_pass_weather = cbind(test_preds_weather, y_test_pass_all_dfs_weather) %>% as.data.frame()
colnames(resultant_df_pbp_pass_weather) = c("Preds", "Vals")

resultant_df_pbp_pass_weather$buckets <- cut(resultant_df_pbp_pass_weather$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pass_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))

summary(xgb_pbp_pass_weather)

names_pbp_pass_weather = colnames(dtrain_pass_all_dfs_weather)

importance_matrix_before_all_weather <- xgb.importance(names_pbp_pass_weather, model = xgb_pbp_pass_weather)
importance_matrix_before_all_weather

predictions_weather <- predict(xgb_pbp_pass_weather, newdata = dtest_pass_all_dfs_weather, type = "response")
predicted_classes_weather <- ifelse(predictions_weather > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes_weather), factor(y_test_pass_all_dfs_weather))

# .9566
# .143


View(resultant_df_pbp_pass_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


####
####


pbp_nfl_run_comp <- pbp_nfl %>% filter(!is.na(temp)) %>% select(td_side, season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% as.data.frame()

pbp_nfl_run_comp$td_side <- as.integer(as.factor(pbp_nfl_run_comp$td_side)) - 1

sample_split_pbp_comp <- sample.split(Y = pbp_nfl_run_comp$td_side, SplitRatio = 0.8)
train_set_pbp_comp <- subset(x = pbp_nfl_run_comp, sample_split_pbp_comp == TRUE)
test_set_pbp_comp <- subset(x = pbp_nfl_run_comp, sample_split_pbp_comp == FALSE)

train_set_pbp_comp <- train_set_pbp_comp %>% filter(!is.na(td_side))
test_set_pbp_comp <- test_set_pbp_comp %>% filter(!is.na(td_side))

X_train_pbp_comp <- train_set_pbp_comp %>% select(-td_side) %>% as.data.frame()
X_test_pbp_comp <- test_set_pbp_comp %>% select(-td_side) %>% as.data.frame()
y_train_pbp_comp <- train_set_pbp_comp$td_side
y_test_pbp_comp <- test_set_pbp_comp$td_side


dtrain_pbp_comp = xgb.DMatrix(data = as.matrix(X_train_pbp_comp), label = y_train_pbp_comp)
dtest_pbp_comp = xgb.DMatrix(data =as.matrix(X_test_pbp_comp), label = y_test_pbp_comp)


####
####


# AUC
auc_no_temp <- auc(y_test_pbp_comp, predict(xgb_pbp_pass, newdata = dtest_pbp_comp))
auc_temp <- auc(y_test_pass_all_dfs_weather, test_preds_weather)

# Log loss
logloss_no_temp <- logLoss(y_test_pass_all_dfs, test_preds)
logloss_temp <- logLoss(y_test_pass_all_dfs_weather, test_preds_weather)

cat("AUC - No temp:", round(auc_no_temp, 4), "\n")
cat("AUC - With temp:", round(auc_temp, 4), "\n\n")

cat("LogLoss - No temp:", round(logloss_no_temp, 4), "\n")
cat("LogLoss - With temp:", round(logloss_temp, 4), "\n")

### AUC WORSE, LOGLOSS BETTER, LET'S JUST GO WITH NO