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


xgboost_pbp_nfl <- pbp_nfl %>% filter((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0) %>% select(sack, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind)

xgboost_pbp_nfl_weather = pbp_nfl %>% filter((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)) %>% select(sack, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, temp, wind)


xgboost_pbp_nfl$sack <- as.factor(xgboost_pbp_nfl$sack)
xgboost_pbp_nfl_weather$sack <- as.factor(xgboost_pbp_nfl_weather$sack)

###
###
###




# Define task
task_pbp_run_no_temp <- TaskClassif$new(
  id = "pbp_run_no_temp_task",
  backend = xgboost_pbp_nfl,
  target = "sack"
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
xgboost_pbp_nfl$sack <- as.integer(as.factor(xgboost_pbp_nfl$sack)) - 1


sample_split_pbp <- sample.split(Y = xgboost_pbp_nfl$sack, SplitRatio = 0.8)
train_set_pbp <- subset(x = xgboost_pbp_nfl, sample_split_pbp == TRUE)
test_set_pbp <- subset(x = xgboost_pbp_nfl, sample_split_pbp == FALSE)

train_set_pbp <- train_set_pbp %>% filter(!is.na(sack))
test_set_pbp <- test_set_pbp %>% filter(!is.na(sack))

X_train_pbp <- train_set_pbp %>% select(-sack) %>% as.data.frame()
X_test_pbp <- test_set_pbp %>% select(-sack) %>% as.data.frame()
y_train_pbp <- train_set_pbp$sack
y_test_pbp <- test_set_pbp$sack


dtrain_pbp = xgb.DMatrix(data = as.matrix(X_train_pbp), label = y_train_pbp)
dtest_pbp = xgb.DMatrix(data =as.matrix(X_test_pbp), label = y_test_pbp)

d_xpbp_all = xgboost_pbp_nfl %>% select(-sack)
d_ypbp_all = xgboost_pbp_nfl$sack
d_all_pbp_all = xgb.DMatrix(data = as.matrix(d_xpbp_all), label = d_ypbp_all)

watchlist_pbp = list(train=dtrain_pbp, test=dtest_pbp)


eta_pbp_pbp = .093 # .094
gamma_pbp_pbp = 6.3 # .125
max_depth_pbp_pbp = 2 # 5
min_child_weight_pbp_pbp = 3.023 # 0
alpha_pbp_pbp = .746 # 0
lambda_pbp_pbp = .177 # 0
colsample_bynode_pbp_pbp = .338 # .85
colsample_bylevel_pbp_pbp = .867 # 1
colsample_bytree_pbp_pbp = .625 # 1

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
                       nround = 1779, # 505
                       objective = "binary:logistic",
                       nthread = 2,
                       gamma = gamma_pbp_pbp,
                       early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.005)

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


xgb.save(xgb_pbp_pbp, 'xgb_pbp_sack_no_weather.model')


####
####
####



# Define task
task_pbp_run_no_temp <- TaskClassif$new(
  id = "pbp_run_no_temp_task",
  backend = xgboost_pbp_nfl_weather,
  target = "sack"
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
xgboost_pbp_nfl_weather$sack <- as.integer(as.factor(xgboost_pbp_nfl_weather$sack)) - 1


sample_split_pbp_weather <- sample.split(Y = xgboost_pbp_nfl_weather$sack, SplitRatio = 0.8)
train_set_pbp_weather <- subset(x = xgboost_pbp_nfl_weather, sample_split_pbp == TRUE)
test_set_pbp_weather <- subset(x = xgboost_pbp_nfl_weather, sample_split_pbp == FALSE)

train_set_pbp_weather <- train_set_pbp_weather %>% filter(!is.na(sack))
test_set_pbp_weather <- test_set_pbp_weather %>% filter(!is.na(sack))

X_train_pbp_weather <- train_set_pbp_weather %>% select(-sack) %>% as.data.frame()
X_test_pbp_weather <- test_set_pbp_weather %>% select(-sack) %>% as.data.frame()
y_train_pbp_weather <- train_set_pbp_weather$sack
y_test_pbp_weather <- test_set_pbp_weather$sack


dtrain_pbp_weather = xgb.DMatrix(data = as.matrix(X_train_pbp_weather), label = y_train_pbp_weather)
dtest_pbp_weather = xgb.DMatrix(data =as.matrix(X_test_pbp_weather), label = y_test_pbp_weather)

d_xpbp_all_weather = xgboost_pbp_nfl_weather %>% select(-sack)
d_ypbp_all_weather = xgboost_pbp_nfl_weather$sack
d_all_pbp_all_weather = xgb.DMatrix(data = as.matrix(d_xpbp_all_weather), label = d_ypbp_all_weather)

watchlist_pbp_weather = list(train=dtrain_pbp_weather, test=dtest_pbp_weather)


eta_pbp_pbp_weather = .15 # .094
gamma_pbp_pbp_weather = 7.86 # .125
max_depth_pbp_pbp_weather = 6 # 5
min_child_weight_pbp_pbp_weather = 12.06 # 0
alpha_pbp_pbp_weather = .387 # 0
lambda_pbp_pbp_weather = .031 # 0
colsample_bynode_pbp_pbp_weather = .902 # .85
colsample_bylevel_pbp_pbp_weather = .197 # 1
colsample_bytree_pbp_pbp_weather = .879 # 1

xgb_pbp_pbp_weather <- xgboost(data = dtrain_pbp_weather, 
                       label = y_train_pbp_weather, 
                       eta = eta_pbp_pbp_weather,
                       max_depth = max_depth_pbp_pbp_weather, 
                       alpha = alpha_pbp_pbp_weather,
                       lambda = lambda_pbp_pbp_weather,
                       min_child_weight = min_child_weight_pbp_pbp_weather,
                       colsample_bynode = colsample_bynode_pbp_pbp_weather,
                       colsample_bytree = colsample_bytree_pbp_pbp_weather,
                       colsample_bylevel = colsample_bylevel_pbp_pbp_weather,
                       nround = 880, # 505
                       objective = "binary:logistic",
                       nthread = 2,
                       gamma = gamma_pbp_pbp_weather,
                       early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.01)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds_weather = predict(xgb_pbp_pbp_weather, newdata = dtest_pbp_weather)

resultant_df_pbp_pbp_weather = cbind(test_preds_weather, y_test_pbp_weather) %>% as.data.frame()
colnames(resultant_df_pbp_pbp_weather) = c("Preds", "Vals")

resultant_df_pbp_pbp_weather$buckets <- cut(resultant_df_pbp_pbp$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pbp_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


names_pbp_pbp_weather = colnames(dtrain_pbp_weather)

importance_matrix_before_all_weather <- xgb.importance(names_pbp_pbp_weather, model = xgb_pbp_pbp_weather)
importance_matrix_before_all_weather

predictions_weather <- predict(xgb_pbp_pbp_weather, newdata = dtest_pbp, type = "response")
predicted_classes_weather <- ifelse(predictions_weather > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes_weather), factor(y_test_pbp_weather))

View(resultant_df_pbp_pbp_weather %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)

# apparently not in it at all? i guess not needed



#####
#####


xgboost_pbp_nfl_comp <- pbp_nfl %>% filter((play_type == "pass") & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & !is.na(temp)) %>% select(sack, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind)


# Make sure it's a factor, then use as.integer() - 1
xgboost_pbp_nfl_comp$sack <- as.integer(as.factor(xgboost_pbp_nfl_comp$sack)) - 1

sample_split_pbp_comp <- sample.split(Y = xgboost_pbp_nfl_comp$sack, SplitRatio = 0.8)
train_set_pbp_comp <- subset(x = xgboost_pbp_nfl_comp, sample_split_pbp_comp == TRUE)
test_set_pbp_comp <- subset(x = xgboost_pbp_nfl_comp, sample_split_pbp_comp == FALSE)

train_set_pbp_comp <- train_set_pbp_comp %>% filter(!is.na(sack))
test_set_pbp_comp <- test_set_pbp_comp %>% filter(!is.na(sack))

X_train_pbp_comp <- train_set_pbp_comp %>% select(-sack) %>% as.data.frame()
X_test_pbp_comp <- test_set_pbp_comp %>% select(-sack) %>% as.data.frame()
y_train_pbp_comp <- train_set_pbp_comp$sack
y_test_pbp_comp <- test_set_pbp_comp$sack


dtrain_pbp_comp = xgb.DMatrix(data = as.matrix(X_train_pbp_comp), label = y_train_pbp_comp)
dtest_pbp_comp = xgb.DMatrix(data =as.matrix(X_test_pbp_comp), label = y_test_pbp_comp)


####
####



# AUC
auc_no_weather <- auc(y_test_pbp_comp, predict(xgb_pbp_pbp, newdata = dtest_pbp_comp))
auc_weather <- auc(y_test_pbp_weather, test_preds_weather)

# Log loss
logloss_no_weather <- logLoss(y_test_pbp, test_preds)
logloss_weather <- logLoss(y_test_pbp_weather, test_preds_weather)

cat("AUC - No Weather:", round(auc_no_weather, 4), "\n")
cat("AUC - With Weather:", round(auc_weather, 4), "\n\n")

cat("LogLoss - No Weather:", round(logloss_no_weather, 4), "\n")
cat("LogLoss - With Weather:", round(logloss_weather, 4), "\n")

### AFTER THIS TEST, THE WEATHER AIN'T HELPING
