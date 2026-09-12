# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

# CLASSIFICATION VERSION
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
  X_test  <- test  %>% select(-!!sym(target_col)) %>% as.data.frame()
  y_train <- as.numeric(as.character(train[[target_col]]))
  y_test  <- as.numeric(as.character(test[[target_col]]))
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)
  
  params <- list(
    eta = best_params$eta, gamma = best_params$gamma,
    max_depth = best_params$max_depth, min_child_weight = best_params$min_child_weight,
    alpha = best_params$alpha, lambda = best_params$lambda,
    colsample_bynode = best_params$colsample_bynode,
    colsample_bylevel = best_params$colsample_bylevel,
    colsample_bytree = best_params$colsample_bytree,
    objective = "binary:logistic", eval_metric = "logloss", nthread = 4
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
  
  # calibration by decile -- the only thing that matters for a model whose
  # predictions get summed into an xTD total
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


###
###


# Scramble runs only -- the exact universe participation_after_run_xtd excludes
# via qb_scramble == 0 and participation_after_pass_xtd excludes via
# play_type == "pass". Feature set is the scramble_ypc set with the gap
# indicators kept (a scramble still has a direction) and qb_hit dropped
# (post-outcome on a TD play).
xgboost_part_nfl <- part_nfl %>%
  filter(qb_scramble == 1 & rush_attempt == 1 & qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0 & pass_attempt == 0) %>%
  select(td_side, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, guard_gap_ind, tackle_gap_ind, end_gap_ind, middle_gap_ind)

cat("scramble plays:", nrow(xgboost_part_nfl),
    " TDs:", sum(xgboost_part_nfl$td_side, na.rm = TRUE),
    " base rate:", round(mean(xgboost_part_nfl$td_side, na.rm = TRUE), 4), "\n")


###
###


task_scramble_xtd_no_temp <- prepare_task(xgboost_part_nfl, "scramble_xtd_no_temp")
tuning_scramble_xtd_no_temp <- tune_xgb_model(task_scramble_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_scramble_xtd_no_temp$best_params)
cat("\nBest CV LogLoss:", round(tuning_scramble_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xgboost_part_nfl, tuning_scramble_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Scramble xTD (No Temp)"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)


###
###


# 1. Save the model
model_path <- paste0(getwd(), '/xgb_part_scramble_xtd_no_temp.model')
xgb.save(final_model_no_temp$model, model_path)

put_object(
  file = model_path,
  object = "models/xgb_part_scramble_xtd_no_temp.model",
  bucket = "nfl-pff-data-lucas"
)

cat("Model saved to S3!\n")


model_name <- "xgb_part_scramble_xtd_no_temp"
model_path <- paste0(getwd(), '/', model_name, '.model')
system(paste0("aws s3 cp ", model_path, " s3://nfl-pff-data-lucas/models/", model_name, ".model"))

artifacts <- list(feature_names = final_model_no_temp$feature_names,
                  feature_importance = importance_no_temp,
                  trained_date = Sys.time())
artifacts_path <- paste0(getwd(), '/', model_name, '_artifacts.rds')
saveRDS(artifacts, artifacts_path)
system(paste0("aws s3 cp ", artifacts_path, " s3://nfl-pff-data-lucas/models/", model_name, "_artifacts.rds"))