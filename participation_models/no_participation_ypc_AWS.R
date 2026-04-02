# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

prepare_task <- function(df, task_id, target_col = "yards_gained") {
  TaskRegr$new(
    id = task_id,
    backend = df,
    target = target_col
  )
}

tune_xgb_model <- function(task, n_evals = 125) {
  learner <- lrn("regr.xgboost",
                 predict_type = "response",
                 objective = "reg:squarederror",
                 nthread = 1,
                 eval_metric = "rmse")
  
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
  measure <- msr("regr.rmse")
  tuner <- tnr("hyperband", eta = 3)
  
  instance <- TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = trm("evals", n_evals = n_evals)
  )
  
  cat("Starting hyperparameter tuning (", n_evals, "max evals, 3-fold CV)...\n")
  cat("Optimizing for RMSE (regression)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "yards_gained") {
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
    objective = "reg:squarederror",
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
  
  rmse_score <- sqrt(mean((preds - y_test)^2))
  mae_score <- mean(abs(preds - y_test))
  r2_score <- 1 - (sum((y_test - preds)^2) / sum((y_test - mean(y_test))^2))
  
  cat("\n===", model_name, "===\n")
  cat("RMSE:", round(rmse_score, 4), "\n")
  cat("MAE: ", round(mae_score, 4), "\n")
  cat("R²:  ", round(r2_score, 4), "\n\n")
  
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
  filter(play_type == "run" & qb_scramble == 0 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & rush_attempt == 1 & pass_attempt == 0) %>% 
  mutate(
    wind = ifelse(is.na(wind), 0, wind),
    temp = ifelse(is.na(temp), 70, temp)
  ) %>%
  select(yards_gained, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind, temp, wind, rain_ind, snow_ind)


# ============================================================================
# TRAIN MODEL
# ============================================================================

task_ypc <- prepare_task(xgboost_pbp_base, "ypc")
tuning_ypc <- tune_xgb_model(task_ypc, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_ypc$best_params)
cat("\nBest CV RMSE:", round(tuning_ypc$best_score, 4), "\n\n")

final_model <- train_final_model(xgboost_pbp_base, tuning_ypc$best_params)

eval_results <- evaluate_model(
  final_model$model,
  final_model$dtest,
  final_model$y_test,
  "YPC Model"
)

importance <- xgb.importance(final_model$feature_names, model = final_model$model)
cat("\nFeature Importance:\n")
print(importance)


# ============================================================================
# SAVE MODEL TO S3
# ============================================================================

model_path <- paste0(getwd(), '/xgb_pbp_ypc.model')
xgb.save(final_model$model, model_path)

put_object(
  file = model_path,
  object = "models/xgb_pbp_ypc.model",
  bucket = "nfl-pff-data-lucas"
)
cat("Model saved to S3!\n")

artifacts <- list(
  feature_names = final_model$feature_names,
  feature_importance = importance,
  best_params = tuning_ypc$best_params,
  trained_date = Sys.time()
)

artifacts_path <- paste0(getwd(), '/xgb_pbp_ypc_artifacts.rds')
saveRDS(artifacts, artifacts_path)

put_object(
  file = artifacts_path,
  object = "models/xgb_pbp_ypc_artifacts.rds",
  bucket = "nfl-pff-data-lucas"
)
cat("Artifacts saved to S3!\n")
