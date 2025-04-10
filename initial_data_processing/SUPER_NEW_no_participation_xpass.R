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


xgboost_pbp_nfl <- pbp_nfl %>% filter((pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0) %>% select(pass_attempt, yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential)

xgboost_pbp_nfl_weather <- pbp_nfl %>% filter((pass_attempt == 1 | rush_attempt == 1) & play_type %in% c("run", "pass") & two_point_attempt == 0 & qb_kneel == 0 & qb_spike == 0 & !is.na(temp)) %>% select(pass_attempt, yardline_100, half_seconds_remaining, posteam_timeouts_remaining, defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential, temp, wind)

xgboost_pbp_nfl$pass_attempt <- as.factor(xgboost_pbp_nfl$pass_attempt)
xgboost_pbp_nfl_weather$pass_attempt <- as.factor(xgboost_pbp_nfl_weather$pass_attempt)


####
####
####


# Define task
task_pbp_run_no_temp <- TaskClassif$new(
  id = "pbp_run_no_temp_task",
  backend = xgboost_pbp_nfl,
  target = "pass_attempt"
)

# Define binary classification learner
learner_pbp_run_no_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.15),
  gamma = p_dbl(lower = 0, upper = 7),
  max_depth = p_int(lower = 2, upper = 11),
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
xgboost_pbp_nfl$pass_attempt <- as.integer(as.factor(xgboost_pbp_nfl$pass_attempt)) - 1


sample_split_pbp <- sample.split(Y = xgboost_pbp_nfl$pass_attempt, SplitRatio = 0.8)
train_set_pbp <- subset(x = xgboost_pbp_nfl, sample_split_pbp == TRUE)
test_set_pbp <- subset(x = xgboost_pbp_nfl, sample_split_pbp == FALSE)

train_set_pbp <- train_set_pbp %>% filter(!is.na(pass_attempt))
test_set_pbp <- test_set_pbp %>% filter(!is.na(pass_attempt))

X_train_pbp <- train_set_pbp %>% select(-pass_attempt) %>% as.data.frame()
X_test_pbp <- test_set_pbp %>% select(-pass_attempt) %>% as.data.frame()
y_train_pbp <- train_set_pbp$pass_attempt
y_test_pbp <- test_set_pbp$pass_attempt


dtrain_pbp = xgb.DMatrix(data = as.matrix(X_train_pbp), label = y_train_pbp)
dtest_pbp = xgb.DMatrix(data =as.matrix(X_test_pbp), label = y_test_pbp)

d_xpbp_all = xgboost_pbp_nfl %>% select(-pass_attempt)
d_ypbp_all = xgboost_pbp_nfl$pass_attempt
d_all_pbp_all = xgb.DMatrix(data = as.matrix(d_xpbp_all), label = d_ypbp_all)

watchlist_pbp = list(train=dtrain_pbp, test=dtest_pbp)


eta_pbp_pbp = .027 # .094
gamma_pbp_pbp = 4.378 # .125
max_depth_pbp_pbp = 7 # 5
min_child_weight_pbp_pbp = 2.86 # 0
alpha_pbp_pbp = .966 # 0
lambda_pbp_pbp = .642 # 0
colsample_bynode_pbp_pbp = .888 # .85
colsample_bylevel_pbp_pbp = .269 # 1
colsample_bytree_pbp_pbp = .975 # 1

xgb_pbp_pbp <- xgboost(data = dtrain_pbp, 
                       label = y_train_pbp, 
                       eta = eta_pbp_pbp,
                       max_depth = max_depth_pbp_pbp, 
                       alpha = alpha_pbp_pbp,
                       lambda = lambda_pbp_pbp,
                       min_child_weight = min_child_weight_pbp_pbp,
                       colsample_bynode = colsample_bynode_pbp_pbp,
                       colsample_bytree = colsample_bytree_pbp_pbp,
                       colsample_bylevel = colsample_bylevel_pbp_pbp,
                       nround = 2089, # 505
                       objective = "binary:logistic",
                       nthread = 2,
                       gamma = gamma_pbp_pbp,
                       early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pbp, newdata = dtest_pbp)

resultant_df_pbp_pbp = cbind(test_preds, y_test_pbp) %>% as.data.frame()
colnames(resultant_df_pbp_pbp) = c("Preds", "Vals")

resultant_df_pbp_pbp$buckets <- cut(resultant_df_pbp_pbp$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


names_pbp_pbp = colnames(dtrain_pbp)

importance_matrix_before_all <- xgb.importance(names_pbp_pbp, model = xgb_pbp_pbp)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pbp, newdata = dtest_pbp, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pbp))

View(resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


xgb.save(xgb_pbp_pbp, 'xgb_pbp_xpass_no_weather.model')


####
####
####



# Define task
task_pbp_run_no_temp <- TaskClassif$new(
  id = "pbp_run_no_temp_task",
  backend = xgboost_pbp_nfl_weather,
  target = "pass_attempt"
)

# Define binary classification learner
learner_pbp_run_no_temp <- lrn("classif.xgboost", predict_type = "prob")

# Define parameter search space
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.155),
  gamma = p_dbl(lower = 0, upper = 8),
  max_depth = p_int(lower = 2, upper = 12),
  min_child_weight = p_dbl(lower = 0, upper = 16),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 2500)
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
  terminator = trm("evals", n_evals = 28)
)

# Train the tuning instance
at$train(task_pbp_run_no_temp)

# View best hyperparameters
at$archive$best()



####
####
####


# Make sure it's a factor, then use as.integer() - 1
xgboost_pbp_nfl_weather$pass_attempt <- as.integer(as.factor(xgboost_pbp_nfl_weather$pass_attempt)) - 1


sample_split_pbp <- sample.split(Y = xgboost_pbp_nfl_weather$pass_attempt, SplitRatio = 0.8)
train_set_pbp <- subset(x = xgboost_pbp_nfl_weather, sample_split_pbp == TRUE)
test_set_pbp <- subset(x = xgboost_pbp_nfl_weather, sample_split_pbp == FALSE)

train_set_pbp <- train_set_pbp %>% filter(!is.na(pass_attempt))
test_set_pbp <- test_set_pbp %>% filter(!is.na(pass_attempt))

X_train_pbp <- train_set_pbp %>% select(-pass_attempt) %>% as.data.frame()
X_test_pbp <- test_set_pbp %>% select(-pass_attempt) %>% as.data.frame()
y_train_pbp <- train_set_pbp$pass_attempt
y_test_pbp <- test_set_pbp$pass_attempt


dtrain_pbp = xgb.DMatrix(data = as.matrix(X_train_pbp), label = y_train_pbp)
dtest_pbp = xgb.DMatrix(data =as.matrix(X_test_pbp), label = y_test_pbp)

d_xpbp_all = xgboost_pbp_nfl_weather %>% select(-pass_attempt)
d_ypbp_all = xgboost_pbp_nfl_weather$pass_attempt
d_all_pbp_all = xgb.DMatrix(data = as.matrix(d_xpbp_all), label = d_ypbp_all)

watchlist_pbp = list(train=dtrain_pbp, test=dtest_pbp)


eta_pbp_pbp = .058 # .094
gamma_pbp_pbp = 4.465 # .125
max_depth_pbp_pbp = 7 # 5
min_child_weight_pbp_pbp = 6.71 # 0
alpha_pbp_pbp = .254 # 0
lambda_pbp_pbp = .349 # 0
colsample_bynode_pbp_pbp = .98 # .85
colsample_bylevel_pbp_pbp = .277 # 1
colsample_bytree_pbp_pbp = .781 # 1

xgb_pbp_pbp <- xgboost(data = dtrain_pbp, 
                       label = y_train_pbp, 
                       eta = eta_pbp_pbp,
                       max_depth = max_depth_pbp_pbp, 
                       alpha = alpha_pbp_pbp,
                       lambda = lambda_pbp_pbp,
                       min_child_weight = min_child_weight_pbp_pbp,
                       colsample_bynode = colsample_bynode_pbp_pbp,
                       colsample_bytree = colsample_bytree_pbp_pbp,
                       colsample_bylevel = colsample_bylevel_pbp_pbp,
                       nround = 690, # 505
                       objective = "binary:logistic",
                       nthread = 2,
                       gamma = gamma_pbp_pbp,
                       early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pbp, newdata = dtest_pbp)

resultant_df_pbp_pbp = cbind(test_preds, y_test_pbp) %>% as.data.frame()
colnames(resultant_df_pbp_pbp) = c("Preds", "Vals")

resultant_df_pbp_pbp$buckets <- cut(resultant_df_pbp_pbp$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


names_pbp_pbp = colnames(dtrain_pbp)

importance_matrix_before_all <- xgb.importance(names_pbp_pbp, model = xgb_pbp_pbp)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pbp, newdata = dtest_pbp, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pbp))

View(resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)

# apparently not in it at all? i guess not needed