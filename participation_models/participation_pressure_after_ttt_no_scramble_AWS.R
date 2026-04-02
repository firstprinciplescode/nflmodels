part_nfl %>%
  filter(sack == 1) %>%
  select(time_to_throw)

# SO ... PRESSURE WILL HAVE TO BE SOME COMBO OF P(PRESSURE | SACK) * (SACK / QB_DROPBACK) + P(PRESSURE | THROW) + !IS.NA(AIR_YARDS) / QB_DROPBACK)

# ALSO ... SCRAMBLES ... WILL HAVE TO BE DEALT WITH DIFFERENTLY TOO

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
# library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)
library(aws.s3)
library(DBI)
library(noctua)
library(xgboost)
library(dplyr)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(paradox)
library(mlr3mbo)
library(caret)
library(MLmetrics)
library(rgenoud)
library(ranger)
library(mlr3hyperband)


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================


# MAKE SURE TO GO INTO PREPARE_TASK AND CHANGE THAT SHIT


prepare_task <- function(df, task_id) {
  df$pressure_ind <- as.factor(df$pressure_ind)
  
  TaskClassif$new(
    id = task_id,
    backend = df,
    target = "pressure_ind"
  )
}

tune_xgb_model <- function(task, n_evals = 125) {
  learner <- lrn("classif.xgboost", 
                 predict_type = "prob",
                 objective = "binary:logistic",
                 nthread = 4,
                 eval_metric = "logloss")
  
  search_space <- ps(
    eta = p_dbl(lower = 0.001, upper = 0.15),
    gamma = p_dbl(lower = 0, upper = 5),
    max_depth = p_int(lower = 1, upper = 15),
    min_child_weight = p_dbl(lower = 0, upper = 15),
    alpha = p_dbl(lower = 0, upper = 1),
    lambda = p_dbl(lower = 0, upper = 1),
    colsample_bynode = p_dbl(lower = 0.1, upper = 1),
    colsample_bylevel = p_dbl(lower = 0.1, upper = 1),
    colsample_bytree = p_dbl(lower = 0.1, upper = 1),
    nrounds = p_int(lower = 100, upper = 15000, tags = "budget")  # budget tag for hyperband
  )
  
  resampling <- rsmp("cv", folds = 3)
  measure <- msr("classif.logloss")
  
  # Use hyperband if available, otherwise random search
  tuner <- tryCatch(
    tnr("hyperband", eta = 3),
    error = function(e) {
      cat("Hyperband not available, using random search\n")
      tnr("random_search")
    }
  )
  
  instance <- TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = trm("evals", n_evals = n_evals)
  )
  
  cat("Starting hyperparameter tuning (", n_evals, "max evals, 4-fold CV)...\n")
  cat("Optimizing for Log Loss (probability calibration)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "pressure_ind") {
  df[[target_col]] <- as.integer(as.factor(df[[target_col]])) - 1
  
  set.seed(42)
  split <- sample.split(Y = df[[target_col]], SplitRatio = 0.8)
  train <- subset(df, split == TRUE) %>% 
    filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  test <- subset(df, split == FALSE) %>% 
    filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  
  X_train <- train %>% select(-!!sym(target_col)) %>% as.data.frame()
  X_test <- test %>% select(-!!sym(target_col)) %>% as.data.frame()
  y_train <- train[[target_col]]
  y_test <- test[[target_col]]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  cat("Training final model with best hyperparameters...\n")
  model <- xgboost(
    data = dtrain,
    label = y_train,
    eta = best_params$eta,
    gamma = best_params$gamma,
    max_depth = best_params$max_depth,
    min_child_weight = best_params$min_child_weight,
    alpha = best_params$alpha,
    lambda = best_params$lambda,
    colsample_bynode = best_params$colsample_bynode,
    colsample_bylevel = best_params$colsample_bylevel,
    colsample_bytree = best_params$colsample_bytree,
    nround = best_params$nrounds,
    objective = "binary:logistic",
    nthread = 4,
    early_stopping_rounds = 50,
    verbose = 1
  )
  
  list(
    model = model,
    dtrain = dtrain,
    dtest = dtest,
    y_train = y_train,
    y_test = y_test,
    feature_names = colnames(X_train)
  )
}

evaluate_model <- function(model, dtest, y_test, model_name = "Model") {
  preds <- predict(model, newdata = dtest)
  pred_classes <- ifelse(preds > 0.5, 1, 0)
  
  auc_score <- auc(y_test, preds)
  logloss_score <- logLoss(y_test, preds)
  cm <- confusionMatrix(factor(pred_classes), factor(y_test))
  
  breaks <- seq(0, 1, by = 0.04)
  labels <- sprintf("%.3f", head(breaks, -1))
  
  results_df <- data.frame(
    Preds = preds,
    Vals = y_test,
    buckets = cut(preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)
  )
  
  calibration <- results_df %>% 
    group_by(buckets) %>% 
    dplyr::summarize(n = n(), mean = mean(Vals), .groups = "drop")
  
  cat("\n===", model_name, "===\n")
  cat("LogLoss:", round(logloss_score, 6), "(PRIMARY - lower is better)\n")
  cat("AUC:", round(auc_score, 6), "(ranking ability)\n")
  cat("Accuracy:", round(cm$overall['Accuracy'], 4), "\n\n")
  cat("Calibration:\n")
  print(calibration)
  
  list(
    auc = auc_score,
    logloss = logloss_score,
    confusion_matrix = cm,
    calibration = calibration
  )
}


# ============================================================================
# DATA PREPARATION
# ============================================================================


xgboost_part_base <- part_nfl %>% 
  filter(two_point_attempt == 0 & fumble == 0 & qb_kneel == 0 & !is.na(defenders_in_box) & play_type %in% c("pass") & pass_attempt == 1 & qb_scramble == 0 & rush_attempt == 0 & !is.na(pressure_ind) & !is.na(time_to_throw) & !is.na(air_yards)) %>% 
  select(pressure_ind, qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind)

xgboost_part_base_weather <- part_nfl %>% 
  filter(two_point_attempt == 0 & fumble == 0 & qb_kneel == 0 & !is.na(defenders_in_box) & play_type %in% c("pass") & pass_attempt == 1 & qb_scramble == 0 & rush_attempt == 0 & !is.na(pressure_ind) & !is.na(time_to_throw) & !is.na(air_yards) & !is.na(wind) & !is.na(temp)) %>% 
  select(pressure_ind, qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, air_yards, temp, wind)


###
###


task_before_xtd_no_temp <- prepare_task(xgboost_part_base, "pressure_no_temp")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV LogLoss:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xgboost_part_base, tuning_before_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Pressure"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)


###
###


task_before_xtd_temp <- prepare_task(xgboost_part_base_weather, "pressure_temp")
tuning_before_xtd_temp <- tune_xgb_model(task_before_xtd_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_temp$best_params)
cat("\nBest CV LogLoss:", round(tuning_before_xtd_temp$best_score, 4), "\n\n")

final_model_temp <- train_final_model(xgboost_part_base_weather, tuning_before_xtd_temp$best_params)

eval_temp <- evaluate_model(
  final_model_temp$model,
  final_model_temp$dtest,
  final_model_temp$y_test,
  "Pass TD (Temp)"
)

importance_temp <- xgb.importance(final_model_temp$feature_names, model = final_model_temp$model)
cat("\nFeature Importance:\n")
print(importance_temp)


####
####


xgboost_part_base_weather$pressure_ind <- as.integer(as.factor(xgboost_part_base_weather$pressure_ind)) - 1

split_common <- sample.split(Y = xgboost_part_base_weather$pressure_ind, SplitRatio = 0.8)
test_common <- subset(xgboost_part_base_weather, split_common == FALSE) %>% 
  filter(!is.na(pressure_ind), !is.na(score_differential))


# No temp model - exclude temp/wind
X_test_no_temp <- test_common %>% 
  select(qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind)

# Temp model - include temp/wind
X_test_temp <- test_common %>% 
  filter(!is.na(wind) & !is.na(temp)) %>% 
  select(qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, cover3_ind, cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind, air_yards, temp, wind)


y_test_common <- test_common$pressure_ind

dtest_no_temp <- xgb.DMatrix(data = as.matrix(X_test_no_temp), label = y_test_common)
dtest_temp <- xgb.DMatrix(data = as.matrix(X_test_temp), label = y_test_common)

# ============================================================================
# GET PREDICTIONS
# ============================================================================

preds_no_temp <- predict(final_model_no_temp$model, newdata = dtest_no_temp)
preds_temp <- predict(final_model_temp$model, newdata = dtest_temp)

# ============================================================================
# COMPARE METRICS
# ============================================================================

logloss_no_temp <- logLoss(y_test_common, preds_no_temp)
logloss_temp <- logLoss(y_test_common, preds_temp)

auc_no_temp <- auc(y_test_common, preds_no_temp)
auc_temp <- auc(y_test_common, preds_temp)

cat("\n========================================\n")
cat("MODEL COMPARISON (SAME TEST SET)\n")
cat("========================================\n\n")

cat("No Temp Model:\n")
cat("  LogLoss:", round(logloss_no_temp, 6), "\n")
cat("  AUC:    ", round(auc_no_temp, 6), "\n\n")

cat("With Temp Model:\n")
cat("  LogLoss:", round(logloss_temp, 6), "\n")
cat("  AUC:    ", round(auc_temp, 6), "\n\n")

cat("Improvements:\n")
cat("  LogLoss change:", round((logloss_no_temp - logloss_temp) * 1000, 3), "points (×1000)\n")
cat("  AUC change:    ", round((auc_temp - auc_no_temp) * 1000, 3), "points (×1000)\n\n")

# Percentage improvement
logloss_pct <- abs((logloss_no_temp - logloss_temp) / logloss_no_temp * 100)
auc_pct <- (auc_temp - auc_no_temp) / auc_no_temp * 100

cat("  LogLoss % improvement:", round(logloss_pct, 2), "%\n")
cat("  AUC % improvement:    ", round(auc_pct, 2), "%\n\n")


# ============================================================================
# CALIBRATION COMPARISON (CRITICAL FOR XTD)
# ============================================================================


breaks <- seq(0, 1, by = 0.04)
labels <- sprintf("%.3f", head(breaks, -1))

cal_no_temp <- data.frame(
  Preds = preds_no_temp,
  Vals = y_test_common
) %>% 
  mutate(buckets = cut(Preds, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>%
  group_by(buckets) %>% 
  summarize(
    n = n(), 
    pred_prob = mean(Preds),  # Average prediction in bucket
    actual_no_temp = mean(Vals),
    .groups = "drop"
  )

cal_temp <- data.frame(
  Preds = preds_temp,
  Vals = y_test_common
) %>% 
  mutate(buckets = cut(Preds, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>%
  group_by(buckets) %>% 
  summarize(
    n = n(), 
    pred_prob = mean(Preds),  # Average prediction in bucket
    actual_temp = mean(Vals),
    .groups = "drop"
  )

# Merge calibration tables
cal_comparison <- full_join(
  cal_no_temp %>% select(buckets, n, pred_prob, actual_no_temp),
  cal_temp %>% select(buckets, actual_temp),
  by = "buckets"
) %>%
  mutate(
    error_no_temp = abs(pred_prob - actual_no_temp),
    error_temp = abs(pred_prob - actual_temp),
    temp_better = error_temp < error_no_temp
  )

cat("========================================\n")
cat("CALIBRATION COMPARISON\n")
cat("========================================\n\n")

print(cal_comparison %>% select(buckets, n, pred_prob, actual_no_temp, actual_temp, temp_better))

# Summary stats
mean_error_no_temp <- mean(cal_comparison$error_no_temp, na.rm = TRUE)
mean_error_temp <- mean(cal_comparison$error_temp, na.rm = TRUE)

cat("\nMean Calibration Error:\n")
cat("  No Temp:", round(mean_error_no_temp, 5), "\n")
cat("  Temp:   ", round(mean_error_temp, 5), "\n")
cat("  Better: ", ifelse(mean_error_temp < mean_error_no_temp, "TEMP MODEL", "NO TEMP MODEL"), "\n\n")


# Save locally
model_path <- paste0(getwd(), '/xgb_part_pressure_after_ttt_pass_att_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)


# Upload to S3
put_object(
  file = model_path,
  object = "models/xgb_part_pressure_after_ttt_pass_att_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")
