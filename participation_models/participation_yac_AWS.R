# MAKE SURE TO GO INTO PREPARE_TASK AND CHANGE THAT SHIT

# REGRESSION VERSION
prepare_task <- function(df, task_id, target_col = "yards_after_catch") {
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
  
  cat("Starting hyperparameter tuning with Hyperband (", n_evals, "max evals, 3-fold CV)...\n")
  cat("Optimizing for RMSE (regression)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "yards_after_catch") {
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

# Base features (no weather)
base_features <- c("yardline_100", "season_type", "half_seconds_remaining", "down",
                   "down_one_ind", "down_two_ind", "down_three_ind",
                   "defenders_in_box", "number_of_pass_rushers", "mod_ydstogo",
                   "shotgun", "no_huddle", "score_differential", "surface",
                   "posteam_ind", "n_ol", "n_te", "n_rb", "n_wr", "n_dl",
                   "n_lb", "n_db", "cover3_ind", "cover2_ind", "cover0_ind",
                   "cover1_ind", "cover4_ind", "twoman_ind", "cover6_ind",
                   "time_to_throw", "hitch_route_ind", "go_route_ind",
                   "out_route_ind", "slant_route_ind", "cross_route_ind",
                   "post_route_ind", "corner_route_ind", "in_route_ind",
                   "wheel_route_ind", "air_yards", "pressure_ind")

weather_features <- c("temp", "wind")

base_filter <- part_nfl %>%
  filter(play_type == "pass" & complete_pass == 1 & sack == 0 &
           qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 &
           pass_attempt == 1 & rush_attempt == 0 & !is.na(yards_after_catch))

xgboost_part_base <- base_filter %>%
  select(yards_after_catch, all_of(base_features))

xgboost_part_base_weather <- base_filter %>%
  filter(!is.na(temp) & !is.na(wind)) %>%
  select(yards_after_catch, all_of(base_features), all_of(weather_features))


# ============================================================================
# TRAIN NO-TEMP MODEL
# ============================================================================

task_no_temp <- prepare_task(xgboost_part_base, "yac_no_temp")
tuning_no_temp <- tune_xgb_model(task_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xgboost_part_base, tuning_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "YAC (No Weather)"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)


# ============================================================================
# TRAIN TEMP MODEL
# ============================================================================

task_temp <- prepare_task(xgboost_part_base_weather, "yac_temp")
tuning_temp <- tune_xgb_model(task_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_temp$best_score, 4), "\n\n")

final_model_temp <- train_final_model(xgboost_part_base_weather, tuning_temp$best_params)

eval_temp <- evaluate_model(
  final_model_temp$model,
  final_model_temp$dtest,
  final_model_temp$y_test,
  "YAC (With Weather)"
)

importance_temp <- xgb.importance(final_model_temp$feature_names, model = final_model_temp$model)
cat("\nFeature Importance:\n")
print(importance_temp)


# ============================================================================
# COMMON TEST SET COMPARISON
# ============================================================================

split_common <- sample.split(Y = xgboost_part_base_weather$yards_after_catch, SplitRatio = 0.8)
test_common <- subset(xgboost_part_base_weather, split_common == FALSE) %>%
  filter(!is.na(yards_after_catch), !is.na(score_differential))

y_test_common <- test_common$yards_after_catch

X_test_no_temp <- test_common %>% select(all_of(base_features)) %>% as.data.frame()
X_test_temp <- test_common %>% select(all_of(base_features), all_of(weather_features)) %>% as.data.frame()

dtest_no_temp <- xgb.DMatrix(data = as.matrix(X_test_no_temp), label = y_test_common)
dtest_temp <- xgb.DMatrix(data = as.matrix(X_test_temp), label = y_test_common)

preds_no_temp <- predict(final_model_no_temp$model, newdata = dtest_no_temp)
preds_temp <- predict(final_model_temp$model, newdata = dtest_temp)

rmse_no_temp <- sqrt(mean((preds_no_temp - y_test_common)^2))
rmse_temp <- sqrt(mean((preds_temp - y_test_common)^2))

mae_no_temp <- mean(abs(preds_no_temp - y_test_common))
mae_temp <- mean(abs(preds_temp - y_test_common))

r2_no_temp <- 1 - (sum((y_test_common - preds_no_temp)^2) / sum((y_test_common - mean(y_test_common))^2))
r2_temp <- 1 - (sum((y_test_common - preds_temp)^2) / sum((y_test_common - mean(y_test_common))^2))

cat("\n========================================\n")
cat("MODEL COMPARISON (SAME TEST SET)\n")
cat("========================================\n\n")

cat("No Weather Model:\n")
cat("  RMSE:", round(rmse_no_temp, 4), "yards\n")
cat("  MAE: ", round(mae_no_temp, 4), "yards\n")
cat("  R²:  ", round(r2_no_temp, 4), "\n\n")

cat("With Weather Model:\n")
cat("  RMSE:", round(rmse_temp, 4), "yards\n")
cat("  MAE: ", round(mae_temp, 4), "yards\n")
cat("  R²:  ", round(r2_temp, 4), "\n\n")

cat("Improvements:\n")
cat("  RMSE change:", round((rmse_no_temp - rmse_temp), 4), "yards (lower is better)\n")
cat("  MAE change: ", round((mae_no_temp - mae_temp), 4), "yards (lower is better)\n")
cat("  R² change:  ", round((r2_temp - r2_no_temp), 4), "(higher is better)\n\n")

rmse_pct <- abs((rmse_no_temp - rmse_temp) / rmse_no_temp * 100)
mae_pct <- abs((mae_no_temp - mae_temp) / mae_no_temp * 100)
r2_pct <- (r2_temp - r2_no_temp) / abs(r2_no_temp) * 100

cat("  RMSE % improvement:", round(rmse_pct, 2), "%\n")
cat("  MAE % improvement: ", round(mae_pct, 2), "%\n")
cat("  R² % improvement:  ", round(r2_pct, 2), "%\n\n")


# ============================================================================
# RESIDUAL ANALYSIS BY DECILE (FIXED)
# ============================================================================

residuals_no_temp <- y_test_common - preds_no_temp
residuals_temp <- y_test_common - preds_temp

cat("========================================\n")
cat("RESIDUAL ANALYSIS\n")
cat("========================================\n\n")

cat("No Weather Model Residuals:\n")
print(summary(residuals_no_temp))

cat("\nWith Weather Model Residuals:\n")
print(summary(residuals_temp))

resid_comparison <- data.frame(
  pred_no_temp = preds_no_temp,
  pred_temp = preds_temp,
  resid_no_temp = abs(residuals_no_temp),
  resid_temp = abs(residuals_temp)
) %>%
  mutate(bin = ntile(pred_no_temp, 10)) %>%
  group_by(bin) %>%
  summarize(
    n = n(),
    mean_pred = mean(pred_no_temp),
    mae_no_temp = mean(resid_no_temp),
    mae_temp = mean(resid_temp),
    temp_better = mae_temp < mae_no_temp,
    .groups = "drop"
  )

cat("\nMean Absolute Error by Prediction Decile:\n")
print(resid_comparison)


# ============================================================================
# AUTO-SELECT WINNER & SAVE
# ============================================================================

temp_wins <- rmse_temp < rmse_no_temp

if (temp_wins) {
  cat("\n>>> WEATHER MODEL WINS - deploying with weather features\n\n")
  winner_model <- final_model_temp$model
  winner_features <- final_model_temp$feature_names
  model_suffix <- "temp"
} else {
  cat("\n>>> NO-WEATHER MODEL WINS - deploying without weather features\n\n")
  winner_model <- final_model_no_temp$model
  winner_features <- final_model_no_temp$feature_names
  model_suffix <- "no_temp"
}

# Save model
model_filename <- paste0("xgb_part_yac_", model_suffix, ".model")
model_path <- paste0(getwd(), "/", model_filename)
xgb.save(winner_model, model_path)

put_object(
  file = model_path,
  object = paste0("models/", model_filename),
  bucket = "nfl-pff-data-lucas"
)
cat("Model saved to S3: models/", model_filename, "\n")

# Save feature names
features_filename <- paste0("xgb_part_yac_", model_suffix, "_features.rds")
features_path <- paste0(getwd(), "/", features_filename)
saveRDS(winner_features, features_path)

put_object(
  file = features_path,
  object = paste0("models/", features_filename),
  bucket = "nfl-pff-data-lucas"
)
cat("Feature names saved to S3: models/", features_filename, "\n")
