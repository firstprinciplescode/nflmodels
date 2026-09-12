# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

prepare_task <- function(df, task_id, target_col = "td_side") {
  df[[target_col]] <- as.factor(df[[target_col]])
  
  TaskClassif$new(
    id = task_id,
    backend = df,
    target = target_col,
    positive = "1"
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
    nrounds = p_int(lower = 100, upper = 15000, tags = "budget")
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
  cat("Optimizing for LogLoss (classification)\n\n")
  
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model <- function(df, best_params, target_col = "td_side") {
  set.seed(42)
  split <- sample.split(Y = df[[target_col]], SplitRatio = 0.8)
  train <- subset(df, split == TRUE) %>% filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  test  <- subset(df, split == FALSE) %>% filter(!is.na(!!sym(target_col)), !is.na(score_differential))
  
  X_train <- train %>% select(-!!sym(target_col)) %>% as.data.frame()
  X_test  <- test %>% select(-!!sym(target_col)) %>% as.data.frame()
  y_train <- as.numeric(as.character(train[[target_col]]))
  y_test  <- as.numeric(as.character(test[[target_col]]))
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  params <- list(
    eta = best_params$eta,
    gamma = best_params$gamma,
    max_depth = best_params$max_depth,
    min_child_weight = best_params$min_child_weight,
    alpha = best_params$alpha,
    lambda = best_params$lambda,
    colsample_bynode = best_params$colsample_bynode,
    colsample_bylevel = best_params$colsample_bylevel,
    colsample_bytree = best_params$colsample_bytree,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nthread = 4
  )
  
  cat("Training final model with best hyperparameters...\n")
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_params$nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50,
    verbose = 1
  )
  
  list(model = model, dtrain = dtrain, dtest = dtest,
       y_train = y_train, y_test = y_test, feature_names = colnames(X_train))
}

evaluate_model <- function(model, dtest, y_test, model_name = "Model") {
  preds <- predict(model, newdata = dtest)
  
  logloss_score <- -mean(y_test * log(pmax(preds, 1e-15)) + (1 - y_test) * log(pmax(1 - preds, 1e-15)))
  brier_score <- mean((preds - y_test)^2)
  base_rate <- mean(y_test)
  
  cat("\n===", model_name, "===\n")
  cat("LogLoss:  ", round(logloss_score, 5), "\n")
  cat("Brier:    ", round(brier_score, 5), "\n")
  cat("Base rate:", round(base_rate, 5), "\n")
  cat("Mean pred:", round(mean(preds), 5), "\n")
  cat("Ratio (actual/pred):", round(base_rate / mean(preds), 4),
      " <- this model is SUMMED into an xTD total, so this is the number that matters\n\n")
  
  bins <- cut(preds, unique(quantile(preds, seq(0, 1, 0.1))), include.lowest = TRUE)
  calib <- data.frame(bin = bins, pred = preds, actual = y_test) %>%
    group_by(bin) %>%
    summarise(n = n(), mean_pred = mean(pred), actual_rate = mean(actual), .groups = "drop") %>%
    mutate(ratio = actual_rate / mean_pred)
  cat("Calibration by decile:\n")
  print(as.data.frame(calib))
  
  list(logloss = logloss_score, brier = brier_score, base_rate = base_rate,
       mean_pred = mean(preds), ratio = base_rate / mean(preds), calibration = calib)
}


# ============================================================================
# DATA PREP
# ============================================================================
# Same play universe as no_participation_scramble_ypc_AWS.R -- scramble runs
# only. This is the universe no_participation_after_run_xtd.R currently scores
# with designed-run logic and gets wrong by 30% (actual 0.0354, pred 0.0273
# over 9,288 plays). Same feature set as the scramble YPC model, which is the
# right set: no defenders_in_box / personnel counts (participation-only), and
# the direction indicators kept because a scramble still has a direction.
# qb_hit is NOT included -- on a scramble it is largely determined by whether
# he got in, so it leaks the outcome.

xgboost_pbp_base <- pbp_base %>%
  filter(qb_scramble == 1 & rush_attempt == 1 & qb_spike == 0 & qb_kneel == 0 &
           two_point_attempt == 0 & pass_attempt == 0) %>%
  mutate(
    wind = ifelse(is.na(wind), 0, wind),
    temp = ifelse(is.na(temp), 70, temp)
  ) %>%
  select(td_side, yardline_100, season_type, half_seconds_remaining, down,
         down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle,
         score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind,
         outside_ind, temp, wind, rain_ind, snow_ind)

cat("Scramble xTD rows:", nrow(xgboost_pbp_base),
    " TDs:", sum(xgboost_pbp_base$td_side, na.rm = TRUE),
    " base rate:", round(mean(xgboost_pbp_base$td_side, na.rm = TRUE), 5), "\n\n")


# ============================================================================
# TRAIN MODEL
# ============================================================================

task_scramble_xtd <- prepare_task(xgboost_pbp_base, "pbp_scramble_xtd_weather")
tuning_scramble_xtd <- tune_xgb_model(task_scramble_xtd, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_scramble_xtd$best_params)
cat("\nBest CV LogLoss:", round(tuning_scramble_xtd$best_score, 5), "\n\n")

final_model <- train_final_model(xgboost_pbp_base, tuning_scramble_xtd$best_params)

eval_results <- evaluate_model(
  final_model$model,
  final_model$dtest,
  final_model$y_test,
  "Scramble xTD (with weather)"
)

importance <- xgb.importance(final_model$feature_names, model = final_model$model)
cat("\nFeature Importance:\n")
print(importance)


# ============================================================================
# SAVE MODEL TO S3
# ============================================================================

model_name <- "xgb_pbp_scramble_xtd"

model_path <- paste0(getwd(), '/', model_name, '.model')
xgb.save(final_model$model, model_path)

put_object(
  file = model_path,
  object = paste0("models/", model_name, ".model"),
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")

artifacts <- list(feature_names = final_model$feature_names,
                  feature_importance = importance,
                  trained_date = Sys.time())
artifacts_path <- paste0(getwd(), '/', model_name, '_artifacts.rds')
saveRDS(artifacts, artifacts_path)
system(paste0("aws s3 cp ", artifacts_path, " s3://nfl-pff-data-lucas/models/", model_name, "_artifacts.rds"))

cat("Artifacts saved to S3!\n")