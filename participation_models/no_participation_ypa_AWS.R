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


# REGRESSION VERSION
prepare_task <- function(df, task_id, target_col = "yards_gained") {
  # No need to convert to factor for regression
  
  TaskRegr$new(  # Changed from TaskClassif
    id = task_id,
    backend = df,
    target = target_col  # Made flexible
  )
}

tune_xgb_model <- function(task, n_evals = 125) {
  learner <- lrn("regr.xgboost",  # Changed from classif.xgboost
                 predict_type = "response",  # Changed from "prob"
                 objective = "reg:squarederror",  # Regression objective
                 nthread = 1,
                 eval_metric = "rmse")  # Changed from logloss
  
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
    nrounds = p_int(lower = 100, upper = 15000, tags = "budget")
  )
  
  resampling <- rsmp("cv", folds = 3)
  measure <- msr("regr.rmse")  # Changed from classif.logloss
  tuner <- tnr("hyperband", eta = 3)
  
  instance <- TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = trm("evals", n_evals = n_evals)
  )
  
  cat("Starting hyperparameter tuning with Hyperband (", n_evals, "max evals, 3-fold CV)...\n")
  cat("Optimizing for RMSE (regression)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "yards_gained") {
  # NO conversion to factor/integer - keep as numeric
  
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
    objective = "reg:squarederror",  # Regression
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
  
  # Regression metrics
  rmse_score <- sqrt(mean((preds - y_test)^2))
  mae_score <- mean(abs(preds - y_test))
  r2_score <- 1 - (sum((y_test - preds)^2) / sum((y_test - mean(y_test))^2))
  
  cat("\n===", model_name, "===\n")
  cat("RMSE:", round(rmse_score, 4), "\n")
  cat("MAE: ", round(mae_score, 4), "\n")
  cat("R²:  ", round(r2_score, 4), "\n\n")
  
  # Residual plot
  resid_df <- data.frame(
    Predicted = preds,
    Actual = y_test,
    Residual = y_test - preds
  )
  
  cat("Residual Summary:\n")
  print(summary(resid_df$Residual))
  
  list(
    rmse = rmse_score,
    mae = mae_score,
    r2 = r2_score,
    residuals = resid_df
  )
}


# ============================================================================
# DATA PREPARATION
# ============================================================================


xgboost_pbp_base <- pbp_base %>% 
  filter(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards) & qb_scramble == 0) %>% 
  select(yards_gained, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining)

xgboost_pbp_base_weather <- pbp_base %>% 
  filter(play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards) & qb_scramble == 0 & !is.na(temp) & !is.na(wind)) %>% 
  select(yards_gained, yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind)


###
###


task_before_xtd_no_temp <- prepare_task(xgboost_pbp_base, "ypa_no_temp")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xgboost_pbp_base, tuning_before_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Pass TD (Temp)"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)


###
###


task_before_xtd_temp <- prepare_task(xgboost_pbp_base_weather, "ypa_no_temp")
tuning_before_xtd_temp <- tune_xgb_model(task_before_xtd_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_temp$best_score, 4), "\n\n")

final_model_temp <- train_final_model(xgboost_pbp_base_weather, tuning_before_xtd_temp$best_params)

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



split_common <- sample.split(Y = xgboost_pbp_base_weather$yards_gained, SplitRatio = 0.8)
test_common <- subset(xgboost_pbp_base_weather, split_common == FALSE) %>% 
  filter(!is.na(yards_gained), !is.na(score_differential))

# No temp model - exclude temp/wind
X_test_no_temp <- test_common %>% 
  select(yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% 
  as.data.frame()

# Temp model - include temp/wind
X_test_temp <- test_common %>% 
  select(yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, air_yards, pass_length, middle_ind, outside_ind, score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind) %>% 
  as.data.frame()

y_test_common <- test_common$yards_gained

dtest_no_temp <- xgb.DMatrix(data = as.matrix(X_test_no_temp), label = y_test_common)
dtest_temp <- xgb.DMatrix(data = as.matrix(X_test_temp), label = y_test_common)

# ============================================================================
# GET PREDICTIONS
# ============================================================================
preds_no_temp <- predict(final_model_no_temp$model, newdata = dtest_no_temp)
preds_temp <- predict(final_model_temp$model, newdata = dtest_temp)

# ============================================================================
# COMPARE METRICS (REGRESSION)
# ============================================================================
rmse_no_temp <- sqrt(mean((preds_no_temp - y_test_common)^2))
rmse_temp <- sqrt(mean((preds_temp - y_test_common)^2))

mae_no_temp <- mean(abs(preds_no_temp - y_test_common))
mae_temp <- mean(abs(preds_temp - y_test_common))

r2_no_temp <- 1 - (sum((y_test_common - preds_no_temp)^2) / sum((y_test_common - mean(y_test_common))^2))
r2_temp <- 1 - (sum((y_test_common - preds_temp)^2) / sum((y_test_common - mean(y_test_common))^2))

cat("\n========================================\n")
cat("MODEL COMPARISON (SAME TEST SET)\n")
cat("========================================\n\n")

cat("No Temp Model:\n")
cat("  RMSE:", round(rmse_no_temp, 4), "yards\n")
cat("  MAE: ", round(mae_no_temp, 4), "yards\n")
cat("  R²:  ", round(r2_no_temp, 4), "\n\n")

cat("With Temp Model:\n")
cat("  RMSE:", round(rmse_temp, 4), "yards\n")
cat("  MAE: ", round(mae_temp, 4), "yards\n")
cat("  R²:  ", round(r2_temp, 4), "\n\n")

cat("Improvements:\n")
cat("  RMSE change:", round((rmse_no_temp - rmse_temp), 4), "yards (lower is better)\n")
cat("  MAE change: ", round((mae_no_temp - mae_temp), 4), "yards (lower is better)\n")
cat("  R² change:  ", round((r2_temp - r2_no_temp), 4), "(higher is better)\n\n")

# Percentage improvement
rmse_pct <- abs((rmse_no_temp - rmse_temp) / rmse_no_temp * 100)
mae_pct <- abs((mae_no_temp - mae_temp) / mae_no_temp * 100)
r2_pct <- (r2_temp - r2_no_temp) / abs(r2_no_temp) * 100

cat("  RMSE % improvement:", round(rmse_pct, 2), "%\n")
cat("  MAE % improvement: ", round(mae_pct, 2), "%\n")
cat("  R² % improvement:  ", round(r2_pct, 2), "%\n\n")

# ============================================================================
# RESIDUAL ANALYSIS (REPLACES CALIBRATION FOR REGRESSION)
# ============================================================================

residuals_no_temp <- y_test_common - preds_no_temp
residuals_temp <- y_test_common - preds_temp

cat("========================================\n")
cat("RESIDUAL ANALYSIS\n")
cat("========================================\n\n")

cat("No Temp Model Residuals:\n")
print(summary(residuals_no_temp))

cat("\nWith Temp Model Residuals:\n")
print(summary(residuals_temp))

# Mean absolute residual by prediction bin (like calibration)
bins <- cut(preds_no_temp, breaks = quantile(preds_no_temp, probs = seq(0, 1, 0.1)), 
            include.lowest = TRUE)

resid_comparison <- data.frame(
  bin = bins,
  resid_no_temp = abs(residuals_no_temp),
  resid_temp = abs(residuals_temp)
) %>%
  group_by(bin) %>%
  summarize(
    n = n(),
    mean_pred = mean(preds_no_temp[bins == unique(bin)], na.rm = TRUE),
    mae_no_temp = mean(resid_no_temp, na.rm = TRUE),
    mae_temp = mean(resid_temp, na.rm = TRUE),
    temp_better = mae_temp < mae_no_temp,
    .groups = "drop"
  )

cat("\nMean Absolute Error by Prediction Decile:\n")
print(resid_comparison)

mean_abs_error_no_temp <- mean(abs(residuals_no_temp))
mean_abs_error_temp <- mean(abs(residuals_temp))

cat("\nOverall Mean Absolute Residual:\n")
cat("  No Temp:", round(mean_abs_error_no_temp, 4), "yards\n")
cat("  Temp:   ", round(mean_abs_error_temp, 4), "yards\n")
cat("  Better: ", ifelse(mean_abs_error_temp < mean_abs_error_no_temp, "TEMP MODEL", "NO TEMP MODEL"), "\n\n")


# Save locally
model_path <- paste0(getwd(), '/xgb_pbp_ypa_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)

# final_model_no_temp <- xgb.load("~/xgb_part_before_xtd_no_temp.model")

# Upload to S3
put_object(
  file = model_path,
  object = "models/xgb_pbp_ypa_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")


# Save locally
model_path <- paste0(getwd(), '/xgb_pbp_ypa_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)

# Upload to S3
put_object(
  file = model_path,
  object = "models/xgb_pbp_ypa_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")
