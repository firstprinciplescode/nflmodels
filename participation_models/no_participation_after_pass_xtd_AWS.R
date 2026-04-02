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
library(future)
library(mlr3hyperband)


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

prepare_task <- function(df, task_id) {
  # Convert target to factor for mlr3
  df$td_side <- as.factor(df$td_side)
  
  TaskClassif$new(
    id = task_id,
    backend = df,
    target = "td_side"
  )
}

tune_xgb_model <- function(task, n_evals = 200) {  # Increased from 150
  learner <- lrn("classif.xgboost", 
                 predict_type = "prob",
                 objective = "binary:logistic",
                 nthread = 4,  # Use all cores
                 eval_metric = "logloss")
  
  search_space <- ps(
    eta = p_dbl(lower = 0.01, upper = 0.1),
    gamma = p_dbl(lower = 0, upper = 5),
    max_depth = p_int(lower = 1, upper = 15),
    min_child_weight = p_dbl(lower = 0, upper = 15),
    alpha = p_dbl(lower = 0, upper = 1),
    lambda = p_dbl(lower = 0, upper = 1),
    colsample_bynode = p_dbl(lower = 0.1, upper = 1),
    colsample_bylevel = p_dbl(lower = 0.1, upper = 1),
    colsample_bytree = p_dbl(lower = 0.1, upper = 1),
    nrounds = p_int(lower = 100, upper = 10000)
  )
  
  resampling <- rsmp("cv", folds = 5)  # Increased from 3
  measure <- msr("classif.logloss")
  tuner <- tnr("random_search")
  
  instance <- TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = trm("evals", n_evals = n_evals)
  )
  
  cat("Starting hyperparameter tuning with Random Search (", n_evals, "evals, 5-fold CV)...\n")
  cat("Using parallel processing with 4 cores\n")
  cat("Optimizing for Log Loss (probability calibration)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "td_side") {
  # Prepare data - convert to binary for xgboost
  df[[target_col]] <- as.integer(as.factor(df[[target_col]])) - 1
  
  # 80/20 split
  set.seed(42)  # For reproducibility
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
  
  # Train with best params
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
    nthread = 2,
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
  
  # Metrics
  auc_score <- auc(y_test, preds)
  logloss_score <- logLoss(y_test, preds)
  cm <- confusionMatrix(factor(pred_classes), factor(y_test))
  
  # Calibration buckets - THIS IS CRITICAL FOR XTD
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
  cat("LogLoss:", round(logloss_score, 4), "(PRIMARY - lower is better)\n")
  cat("AUC:", round(auc_score, 4), "(ranking ability)\n")
  cat("Accuracy:", round(cm$overall['Accuracy'], 4), "(not very meaningful for rare events)\n\n")
  cat("Calibration check (predicted prob vs actual TD rate):\n")
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

# Model 1: No temp/wind data
pbp_base_pass_no_temp <- pbp_base %>% 
  filter(
    play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & 
      sack == 0 & pass_attempt == 1 & qb_scramble == 0 & !is.na(air_yards)
  ) %>% 
  select(
    td_side, yardline_100, half_seconds_remaining, shotgun, no_huddle, 
    mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
    score_differential, qb_hit, roof, surface, 
    posteam_timeouts_remaining, defteam_timeouts_remaining
  )

# Model 2: With temp/wind data
pbp_base_pass_temp <- pbp_base %>% 
  filter(
    play_type == "pass" & qb_spike == 0 & qb_kneel == 0 & 
      sack == 0 & pass_attempt == 1 & !is.na(temp) & qb_scramble == 0 & !is.na(air_yards)
  ) %>% 
  select(
    td_side, yardline_100, half_seconds_remaining, shotgun, no_huddle, 
    mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
    score_differential, qb_hit, roof, surface, 
    posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind
  )

cat("Pass (no temp) rows:", nrow(pbp_base_pass_no_temp), "\n")
cat("Pass (temp) rows:", nrow(pbp_base_pass_temp), "\n\n")

# ============================================================================
# MODEL 1: PASS TD (NO TEMP)
# ============================================================================

### cat("\n========================================\n")
### cat("MODEL 1: PASS TD (NO TEMP)\n")
### cat("========================================\n\n")

task_pass_no_temp <- prepare_task(pbp_base_pass_no_temp, "pass_no_temp")

# Tune with Hyperband
tuning_result_no_temp <- tune_xgb_model(task_pass_no_temp, n_evals = 100)

cat("\nBest hyperparameters found:\n")
print(tuning_result_no_temp$best_params)
cat("\nBest CV LogLoss:", round(tuning_result_no_temp$best_score, 4), "\n\n")

# Train final model - FIXED: use pbp_base_pass_no_temp, not pbp_nfl_pass_no_temp
final_model_no_temp <- train_final_model(pbp_base_pass_no_temp, tuning_result_no_temp$best_params)

# Evaluate
eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Pass TD (No Temp)"
)

# Feature importance
importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)



# ============================================================================
# MODEL 2: PASS TD (TEMP)
# ============================================================================

### cat("\n========================================\n")
### cat("MODEL 2: PASS TD (TEMP)\n")
### cat("========================================\n\n")


tune_xgb_model <- function(task, n_evals = 200) {  # Increased from 100
  learner <- lrn("classif.xgboost", 
                 predict_type = "prob",
                 objective = "binary:logistic",
                 nthread = 1,  # 1 per learner since we're parallelizing CV folds
                 eval_metric = "logloss")
  
  search_space <- ps(
    eta = p_dbl(lower = 0.01, upper = 0.1),
    gamma = p_dbl(lower = 0, upper = 5),
    max_depth = p_int(lower = 2, upper = 6),
    min_child_weight = p_dbl(lower = 0, upper = 15),
    alpha = p_dbl(lower = 0, upper = 1),
    lambda = p_dbl(lower = 0, upper = 1),
    colsample_bynode = p_dbl(lower = 0.4, upper = 1),
    colsample_bylevel = p_dbl(lower = 0.4, upper = 1),
    colsample_bytree = p_dbl(lower = 0.4, upper = 1),
    nrounds = p_int(lower = 100, upper = 1500, tags = "budget")
  )
  
  resampling <- rsmp("cv", folds = 5)  # Increased from 3
  measure <- msr("classif.logloss")
  tuner <- tnr("hyperband", eta = 3)
  
  instance <- TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = trm("evals", n_evals = n_evals)
  )
  
  cat("Starting hyperparameter tuning with Hyperband (", n_evals, "max evals, 5-fold CV)...\n")
  cat("Using 4 cores for parallel CV folds\n")
  cat("Optimizing for Log Loss (probability calibration)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "td_side") {
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
    nthread = 4,  # Use all cores for final model training
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

task_pass_temp <- prepare_task(pbp_base_pass_temp, "pass_temp")
tuning_result_temp <- tune_xgb_model(task_pass_temp, n_evals = 200)

cat("\nBest hyperparameters found:\n")
print(tuning_result_temp$best_params)
cat("\nBest CV LogLoss:", round(tuning_result_temp$best_score, 4), "\n\n")

final_model_temp <- train_final_model(pbp_base_pass_temp, tuning_result_temp$best_params)

eval_temp <- evaluate_model(
  final_model_temp$model,
  final_model_temp$dtest,
  final_model_temp$y_test,
  "Pass TD (With Temp)"
)

importance_temp <- xgb.importance(final_model_temp$feature_names, model = final_model_temp$model)
cat("\nFeature Importance:\n")
print(importance_temp)



### MODEL TESTING - NO TEMP v TEMP

pbp_base_pass_temp$td_side <- as.integer(as.factor(pbp_base_pass_temp$td_side)) - 1

split_common <- sample.split(Y = pbp_base_pass_temp$td_side, SplitRatio = 0.8)
test_common <- subset(pbp_base_pass_temp, split_common == FALSE) %>% 
  filter(!is.na(td_side), !is.na(score_differential))


# No temp model - exclude temp/wind
X_test_no_temp <- test_common %>% 
  select(yardline_100, half_seconds_remaining, shotgun, no_huddle, 
         mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
         score_differential, qb_hit, roof, surface, 
         posteam_timeouts_remaining, defteam_timeouts_remaining) %>% 
  as.data.frame()

# Temp model - include temp/wind
X_test_temp <- test_common %>% 
  select(yardline_100, half_seconds_remaining, shotgun, no_huddle, 
         mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
         score_differential, qb_hit, roof, surface, 
         posteam_timeouts_remaining, defteam_timeouts_remaining, temp, wind) %>% 
  as.data.frame()

y_test_common <- test_common$td_side

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
  Vals = y_test_common,
  buckets = cut(preds_no_temp, breaks, labels = labels, include.lowest = TRUE, right = FALSE)
) %>% 
  group_by(buckets) %>% 
  summarize(
    n = n(), 
    pred_prob = as.numeric(as.character(buckets)),
    actual_no_temp = mean(Vals),
    .groups = "drop"
  )

cal_temp <- data.frame(
  Preds = preds_temp,
  Vals = y_test_common,
  buckets = cut(preds_temp, breaks, labels = labels, include.lowest = TRUE, right = FALSE)
) %>% 
  group_by(buckets) %>% 
  summarize(
    n = n(),
    pred_prob = as.numeric(as.character(buckets)),
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


# Add this to your training script:
model_path <- '/home/claude/xgb_no_participation_pass_td_no_temp.model'
xgb.save(final_model_no_temp$model, model_path)

# Upload to S3
system(paste0("aws s3 cp ", model_path, " s3://your-pff-bucket/models/xgb_pass_td_no_temp.model"))

cat("Model saved to S3!\n")


# xgb.save(final_model_temp$model, '/home/claude/xgb_pass_td_temp.model')
# saveRDS(tuning_result_temp, '/home/claude/tuning_pass_temp.rds')
# cat("\nModel saved!\n")


# Save locally
model_path <- paste0(getwd(), '/xgb_pass_td_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)

# Upload to S3
put_object(
  file = model_path,
  object = "models/xgb_pass_td_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")


# Clean up
# rm(task_pass_no_temp, tuning_result_no_temp, final_model_no_temp)
# gc()