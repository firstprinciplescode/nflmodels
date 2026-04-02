# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

prepare_task <- function(df, task_id) {
  df$complete_pass <- as.factor(df$complete_pass)
  
  TaskClassif$new(
    id = task_id,
    backend = df,
    target = "complete_pass"
  )
}

tune_xgb_model <- function(task, n_evals = 125) {
  learner <- lrn("classif.xgboost", 
                 predict_type = "prob",
                 objective = "binary:logistic",
                 nthread = 1,
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
    nrounds = p_int(lower = 100, upper = 10000, tags = "budget")
  )
  
  resampling <- rsmp("cv", folds = 3)
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
  
  cat("Starting hyperparameter tuning with Hyperband (", n_evals, "max evals, 3-fold CV)...\n")
  cat("Optimizing for Log Loss (probability calibration)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "complete_pass") {
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
# DATA PREPARATION (pbp_base - NO PARTICIPATION)
# ============================================================================

xgboost_pbp_base <- pbp_base %>% 
  filter(play_type == "pass" & qb_spike == 0 & qb_scramble == 0 & fumble == 0 & 
           qb_kneel == 0 & sack == 0 & pass_attempt == 1 & two_point_attempt == 0 & 
           !is.na(air_yards)) %>%
  mutate(
    wind = ifelse(is.na(wind), 0, wind),
    temp = ifelse(is.na(temp), 70, temp)
  ) %>%
  select(complete_pass, yardline_100, half_seconds_remaining, shotgun, no_huddle, 
         mod_ydstogo, qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
         score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, 
         defteam_timeouts_remaining, down_one_ind, down_two_ind, down_three_ind, 
         down_four_ind, temp, wind, rain_ind, snow_ind)

cat("Completion probability (no participation) rows:", nrow(xgboost_pbp_base), "\n\n")


# ============================================================================
# TRAIN MODEL
# ============================================================================

task_pbp_cp <- prepare_task(xgboost_pbp_base, "pbp_cp_weather")
tuning_pbp_cp <- tune_xgb_model(task_pbp_cp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_pbp_cp$best_params)
cat("\nBest CV LogLoss:", round(tuning_pbp_cp$best_score, 4), "\n\n")

final_model <- train_final_model(xgboost_pbp_base, tuning_pbp_cp$best_params)

eval_results <- evaluate_model(
  final_model$model,
  final_model$dtest,
  final_model$y_test,
  "Completion Probability - No Participation (with weather)"
)

importance <- xgb.importance(final_model$feature_names, model = final_model$model)
cat("\nFeature Importance:\n")
print(importance)


# ============================================================================
# SAVE MODEL TO S3
# ============================================================================

model_name <- "xgb_pbp_cp"

model_path <- paste0(getwd(), '/', model_name, '.model')
xgb.save(final_model$model, model_path)

put_object(
  file = model_path,
  object = paste0("models/", model_name, ".model"),
  bucket = "nfl-pff-data-lucas"
)
cat("Model saved to S3!\n")

# Save artifacts
artifacts <- list(
  feature_names = final_model$feature_names,
  feature_importance = importance,
  best_params = tuning_pbp_cp$best_params,
  trained_date = Sys.time()
)

artifacts_path <- paste0(getwd(), '/', model_name, '_artifacts.rds')
saveRDS(artifacts, artifacts_path)

put_object(
  file = artifacts_path,
  object = paste0("models/", model_name, "_artifacts.rds"),
  bucket = "nfl-pff-data-lucas"
)
cat("Artifacts saved to S3!\n")