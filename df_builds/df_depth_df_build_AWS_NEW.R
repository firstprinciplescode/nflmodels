# ============================================================================
# COMPLETE DEPTH OFFENSE SCRIPT - FAST LOO VERSION
# ============================================================================

library(tidyverse)
library(dplyr)
library(aws.s3)
library(xgboost)
library(caTools)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(paradox)
library(mlr3hyperband)
library(conflicted)
library(purrr)

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, 
                 dplyr::summarise, dplyr::mutate, dplyr::desc, dplyr::summarize,
                 purrr::compact)

# ============================================================================
# ATHENA QUERY FUNCTION
# ============================================================================

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  cat("Query ID:", query_id, "\n")
  
  # Poll for completion
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  cat("Final status:", status, "\n")
  
  if (status != "SUCCEEDED") {
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  # Get results location
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- trimws(system(result_cmd, intern = TRUE))
  cat("S3 path:", s3_path, "\n")
  
  # Check if file exists
  Sys.sleep(2)  # Give S3 a moment
  check_cmd <- sprintf("aws s3 ls %s", s3_path)
  cat("File check:", system(check_cmd, intern = TRUE), "\n")
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

# ============================================================================
# MODEL FUNCTIONS (UPDATED FOR NEW XGBOOST API)
# ============================================================================

prepare_task_depth <- function(df, task_id, target_col = "mixed_xtd") {
  TaskRegr$new(
    id = task_id,
    backend = df,
    target = target_col
  )
}

tune_xgb_model_depth <- function(task, n_evals = 125) {
  learner <- lrn("regr.xgboost",
                 predict_type = "response",
                 objective = "reg:squarederror",
                 nthread = 1,
                 eval_metric = "rmse")
  
  search_space <- ps(
    eta = p_dbl(lower = 0.001, upper = 0.15),
    gamma = p_dbl(lower = 0, upper = 6),
    max_depth = p_int(lower = 1, upper = 15),
    min_child_weight = p_dbl(lower = 0, upper = 15),
    alpha = p_dbl(lower = 0, upper = 1),
    lambda = p_dbl(lower = 0, upper = 1),
    colsample_bynode = p_dbl(lower = 0.1, upper = 1),
    colsample_bylevel = p_dbl(lower = 0.1, upper = 1),
    colsample_bytree = p_dbl(lower = 0.1, upper = 1),
    nrounds = p_int(lower = 100, upper = 20000, tags = "budget")
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
  tuner$optimize(instance)
  
  list(
    best_params = instance$result_learner_param_vals,
    best_score = instance$result_y,
    archive = instance$archive
  )
}

train_final_model_depth <- function(df, best_params, target_col = "mixed_xtd") {
  set.seed(42)
  split <- sample.split(Y = df[[target_col]], SplitRatio = 0.8)
  train <- subset(df, split == TRUE) %>% filter(!is.na(!!sym(target_col)))
  test <- subset(df, split == FALSE) %>% filter(!is.na(!!sym(target_col)))
  
  X_train <- train %>% select(-!!sym(target_col)) %>% as.data.frame()
  X_test <- test %>% select(-!!sym(target_col)) %>% as.data.frame()
  y_train <- train[[target_col]]
  y_test <- test[[target_col]]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = best_params$eta,
    gamma = best_params$gamma,
    max_depth = best_params$max_depth,
    min_child_weight = best_params$min_child_weight,
    alpha = best_params$alpha,
    lambda = best_params$lambda,
    colsample_bynode = best_params$colsample_bynode,
    colsample_bylevel = best_params$colsample_bylevel,
    colsample_bytree = best_params$colsample_bytree,
    nthread = 4
  )
  
  cat("Training final model with best hyperparameters...\n")
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_params$nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50,
    print_every_n = 50
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

evaluate_model_depth <- function(model, dtest, y_test, model_name = "Model") {
  preds <- predict(model, newdata = dtest)
  
  rmse_score <- sqrt(mean((preds - y_test)^2))
  mae_score <- mean(abs(preds - y_test))
  r2_score <- 1 - (sum((y_test - preds)^2) / sum((y_test - mean(y_test))^2))
  
  cat("\n===", model_name, "===\n")
  cat("RMSE:", round(rmse_score, 4), "\n")
  cat("MAE: ", round(mae_score, 4), "\n")
  cat("R²:  ", round(r2_score, 4), "\n\n")
  
  list(rmse = rmse_score, mae = mae_score, r2 = r2_score)
}

# ============================================================================
# DATA LOADING
# ============================================================================

cat("Loading QB list...\n")
# ============================================================================
# DATA LOADING
# ============================================================================

run_athena_query <- function(sql, max_wait = 120) {
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  if (status != "SUCCEEDED") {
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- system(result_cmd, intern = TRUE)
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

cat("Loading QB list...\n")
qbgrp_list_depth <- run_athena_query("
    SELECT qbgrp_ssn, COUNT(*) as game_count
    FROM nfl_data.combined_grade_epa_summary
    GROUP BY qbgrp_ssn
    HAVING COUNT(*) >= 10
")

cat("Loading depth data...\n")
query_depth <- run_athena_query("
      SELECT  
          epa.qbgrp_ssn,
          epa.good_off_ind,
          pa.*
      FROM nfl_data.vw_combined_depth pa
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pa.team_name = epa.posteam
          AND pa.week = epa.week 
          AND pa.season = epa.season 
  ")

query_depth <- as_tibble(query_depth) %>%
  filter(qbgrp_ssn %in% qbgrp_list_depth$qbgrp_ssn)

exclude_cols_depth <- c("qbgrp_ssn", "good_off_ind", "team_name", "week", "season")
numeric_cols_depth <- setdiff(names(query_depth)[sapply(query_depth, is.numeric)], exclude_cols_depth)
cat("Processing", length(numeric_cols_depth), "numeric features\n\n")

query_depth <- query_depth %>% arrange(qbgrp_ssn, week, season)

# ============================================================================
# FAST LEAVE-ONE-OUT
# ============================================================================

cat("Precomputing statistics...\n")

good_stats_depth <- query_depth %>%
  filter(good_off_ind == 'Good') %>%
  group_by(qbgrp_ssn) %>%
  summarise(
    n_good = n(),
    across(all_of(numeric_cols_depth), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

bad_stats_depth <- query_depth %>%
  filter(good_off_ind == 'Bad') %>%
  group_by(qbgrp_ssn) %>%
  summarise(
    n_bad = n(),
    across(all_of(numeric_cols_depth), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

all_good_means_depth <- query_depth %>%
  filter(good_off_ind == 'Good') %>%
  group_by(qbgrp_ssn) %>%
  summarise(across(all_of(numeric_cols_depth), mean, na.rm = TRUE), .groups = 'drop')

all_bad_means_depth <- query_depth %>%
  filter(good_off_ind == 'Bad') %>%
  group_by(qbgrp_ssn) %>%
  summarise(across(all_of(numeric_cols_depth), mean, na.rm = TRUE), .groups = 'drop')

cat("Processing", nrow(query_depth), "games with fast LOO...\n")

xtd_depth_df_list <- map(1:nrow(query_depth), function(i) {
  
  if(i %% 500 == 0) cat("Processing game", i, "of", nrow(query_depth), "\n")
  
  row <- query_depth[i, ]
  rel_ssn <- row$qbgrp_ssn
  is_good <- row$good_off_ind == 'Good'
  
  df_good <- all_good_means_depth
  df_bad <- all_bad_means_depth
  
  this_good_stats <- good_stats_depth %>% filter(qbgrp_ssn == rel_ssn)
  this_bad_stats <- bad_stats_depth %>% filter(qbgrp_ssn == rel_ssn)
  
  if(nrow(this_good_stats) == 0 || nrow(this_bad_stats) == 0) return(list(data = NULL, idx = i))
  
  if(is_good) {
    n <- this_good_stats$n_good
    if(n <= 1) return(list(data = NULL, idx = i))
    
    for(col in numeric_cols_depth) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_good_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_good[df_good$qbgrp_ssn == rel_ssn, col] <- new_mean
    }
  } else {
    n <- this_bad_stats$n_bad
    if(n <= 1) return(list(data = NULL, idx = i))
    
    for(col in numeric_cols_depth) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_bad_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_bad[df_bad$qbgrp_ssn == rel_ssn, col] <- new_mean
    }
  }
  
  df_good_ordered <- df_good %>% arrange(qbgrp_ssn)
  df_bad_ordered <- df_bad %>% 
    filter(qbgrp_ssn %in% df_good_ordered$qbgrp_ssn) %>% 
    arrange(qbgrp_ssn)
  df_good_ordered <- df_good_ordered %>% filter(qbgrp_ssn %in% df_bad_ordered$qbgrp_ssn)
  
  df_diff <- df_good_ordered[, -1] - df_bad_ordered[, -1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  
  qb_ssn <- df_good_ordered[, 1, drop = FALSE]
  
  df_good2 <- df_good_ordered[, -1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_bad2 <- df_bad_ordered[, -1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_diff2 <- df_diff %>% as.data.frame() %>% mutate(across(everything(), scale))
  
  df_good2 <- cbind(qb_ssn, df_good2)
  df_bad2 <- cbind(qb_ssn, df_bad2)
  df_diff2 <- cbind(qb_ssn, df_diff2)
  
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  
  qb_good_row <- df_good2 %>% filter(qbgrp_ssn_Good == rel_ssn)
  qb_bad_row <- df_bad2 %>% filter(qbgrp_ssn_Bad == rel_ssn)
  qb_diff_row <- df_diff2 %>% filter(qbgrp_ssn == rel_ssn)
  
  if(nrow(qb_good_row) == 0 || nrow(qb_bad_row) == 0 || nrow(qb_diff_row) == 0) return(list(data = NULL, idx = i))
  
  result <- cbind(qb_good_row, qb_bad_row, qb_diff_row) %>% 
    select(-c(qbgrp_ssn_Bad, qbgrp_ssn))
  
  list(data = result, idx = i)
})

# Extract successful results and their indices
success_indices_depth <- sapply(xtd_depth_df_list, function(x) if(!is.null(x$data)) x$idx else NA)
success_indices_depth <- success_indices_depth[!is.na(success_indices_depth)]

xtd_depth_df <- map(xtd_depth_df_list, function(x) x$data) %>% 
  purrr::compact() %>% 
  list_rbind()

cat("\n=== SUMMARY ===\n")
cat("Total games processed:", nrow(query_depth), "\n")
cat("Successful feature rows:", nrow(xtd_depth_df), "\n")
cat("Failed rows:", nrow(query_depth) - nrow(xtd_depth_df), "\n")

# ============================================================================
# JOIN WITH XTD TRAINING
# ============================================================================

xtd_training_depth <- s3read_using(
  read.csv,
  object = "mixed_xtd/xtd_training.csv",
  bucket = "nfl-pff-data-lucas"
)

# Only join with rows that succeeded
xtd_depth_df <- cbind(xtd_depth_df, query_depth[success_indices_depth, ] %>% select(team_name, week, season))

xtd_depth_df_train <- left_join(
  xtd_depth_df, 
  xtd_training_depth %>% select(posteam:season, mixed_xtd), 
  by = c("team_name" = "posteam", "week" = "week", "season" = "season")
) %>% filter(!is.na(mixed_xtd))

xtd_depth_df_train_2 <- xtd_depth_df_train %>%
  select(where(is.numeric)) %>%
  select(-c(week, season))

# ============================================================================
# MODEL TRAINING
# ============================================================================

task_depth <- prepare_task_depth(xtd_depth_df_train_2, "depth_characteristics")
tuning_depth <- tune_xgb_model_depth(task_depth, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_depth$best_params)
cat("\nBest CV RMSE:", round(tuning_depth$best_score, 4), "\n\n")

final_model_depth <- train_final_model_depth(xtd_depth_df_train_2, tuning_depth$best_params)

eval_depth <- evaluate_model_depth(
  final_model_depth$model,
  final_model_depth$dtest,
  final_model_depth$y_test,
  "Depth Characteristics"
)

importance_depth <- xgb.importance(final_model_depth$feature_names, model = final_model_depth$model)
cat("\nFeature Importance:\n")
print(importance_depth)

# ============================================================================
# COMPARISON FUNCTION & SAVE
# ============================================================================

df_depth_scaled_z <- xtd_depth_df %>%
  select(-c(team_name, week, season)) %>%
  group_by(qbgrp_ssn_Good) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rename(qbgrp_ssn = qbgrp_ssn_Good)

importance_matrix_depth <- importance_depth

comparison_depth_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  relevant_features <- importance_matrix_depth$Feature
  relevant_features <- relevant_features[relevant_features %in% colnames(df_depth_scaled_z)]
  
  relevant_team <- df_depth_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others <- df_depth_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others_qbs <- df_depth_scaled_z %>% 
    filter(qbgrp_ssn != qbgrp_ssn2) %>% 
    pull(qbgrp_ssn)
  
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  weights <- importance_matrix_depth %>%
    filter(Feature %in% relevant_features) %>%
    arrange(match(Feature, relevant_features)) %>%
    pull(Gain)
  
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  result_qb <- rowSums(weighted_diff)
  result_qb <- data.frame(QB = all_others_qbs, Score = result_qb)
  
  if (!is.null(year)) {
    result_qb <- result_qb %>%
      filter(substr(QB, nchar(QB) - 3, nchar(QB)) == as.character(year))
  }
  
  result_qb %>% filter(Score <= threshold) %>% arrange(Score)
}

# Test it
comparison_depth_func("NEMaye-2025", .9)

# Save dependencies
list_dependencies_depth <- list(
  df_depth_scaled_z = df_depth_scaled_z,
  importance_matrix_depth = importance_matrix_depth
)

save(list_dependencies_depth, file = "list_dependencies_depth.RData")
put_object(
  file = "list_dependencies_depth.RData",
  object = "qbgrp_def_functions/list_dependencies_depth.RData",
  bucket = "nfl-pff-data-lucas"
)

save(comparison_depth_func, file = "comparison_depth_func.rds")
put_object(
  file = "comparison_depth_func.rds",
  object = "qbgrp_def_functions/comparison_depth_func.rds",
  bucket = "nfl-pff-data-lucas"
)

save.image("~/df_depth_df_build_workspace_AWS.RData")
cat("\nDONE.\n")
