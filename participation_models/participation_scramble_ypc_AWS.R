
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
  set.seed(42)
  split <- sample.split(Y = df[[target_col]], SplitRatio = 0.8)
  train <- subset(df, split == TRUE) %>% filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  test  <- subset(df, split == FALSE) %>% filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  
  X_train <- train %>% select(-!!sym(target_col)) %>% as.data.frame()
  X_test  <- test  %>% select(-!!sym(target_col)) %>% as.data.frame()
  y_train <- train[[target_col]]
  y_test  <- test[[target_col]]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)
  
  params <- list(
    eta = best_params$eta, gamma = best_params$gamma,
    max_depth = best_params$max_depth, min_child_weight = best_params$min_child_weight,
    alpha = best_params$alpha, lambda = best_params$lambda,
    colsample_bynode = best_params$colsample_bynode,
    colsample_bylevel = best_params$colsample_bylevel,
    colsample_bytree = best_params$colsample_bytree,
    objective = "reg:squarederror", eval_metric = "rmse", nthread = 4
  )
  
  cat("Training final model with best hyperparameters...\n")
  model <- xgb.train(
    params = params, data = dtrain, nrounds = best_params$nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50, verbose = 1
  )
  
  list(model = model, dtrain = dtrain, dtest = dtest,
       y_train = y_train, y_test = y_test, feature_names = colnames(X_train))
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


###
###


xgboost_part_nfl <- part_nfl %>% 
  filter(qb_scramble == 1 & rush_attempt == 1 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & pass_attempt == 0) %>% 
  select(yards_gained, qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, guard_gap_ind, tackle_gap_ind, end_gap_ind, middle_gap_ind)


###
###


task_before_xtd_no_temp <- prepare_task(xgboost_part_nfl, "ypc_no_temp")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xgboost_part_nfl, tuning_before_xtd_no_temp$best_params)

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


# 1. Save the model
# Save locally
model_path <- paste0(getwd(), '/xgb_part_scramble_ypc_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)

# Upload to S3
put_object(
  file = model_path,
  object = "models/xgb_part_scramble_ypc_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")


model_name <- "xgb_part_scramble_ypc_no_temp"

# Save artifacts
artifacts <- list(
  feature_names = final_model_no_temp$feature_names,
  feature_importance = importance_no_temp,
  trained_date = Sys.time()
)

artifacts_path <- paste0(getwd(), '/', model_name, '_artifacts.rds')
saveRDS(artifacts, artifacts_path)

put_object(
  file = artifacts_path,
  object = paste0("models/", model_name, "_artifacts.rds"),
  bucket = "nfl-pff-data-lucas"
)





