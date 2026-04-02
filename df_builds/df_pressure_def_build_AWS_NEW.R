# ============================================================================
# COMPLETE PRESSURE DEFENSE SCRIPT - FAST LOO VERSION
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

# ============================================================================
# MODEL FUNCTIONS (UPDATED FOR NEW XGBOOST API)
# ============================================================================

prepare_task_pressure <- function(df, task_id, target_col = "mixed_xtd") {
  TaskRegr$new(
    id = task_id,
    backend = df,
    target = target_col
  )
}

tune_xgb_model_pressure <- function(task, n_evals = 125) {
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

train_final_model_pressure <- function(df, best_params, target_col = "mixed_xtd") {
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
    learning_rate = best_params$eta,
    min_split_loss = best_params$gamma,
    max_depth = best_params$max_depth,
    min_child_weight = best_params$min_child_weight,
    reg_alpha = best_params$alpha,
    reg_lambda = best_params$lambda,
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

evaluate_model_pressure <- function(model, dtest, y_test, model_name = "Model") {
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

cat("Loading defense list...\n")
def_list_pressure <- run_athena_query("
    SELECT def_ssn, COUNT(*) as game_count
    FROM nfl_data.combined_grade_epa_summary
    GROUP BY def_ssn
    HAVING COUNT(*) >= 10
")

cat("Loading pressure data...\n")
query_pressure <- run_athena_query("
      SELECT  
          epa.def_ssn,
          epa.good_epa_def_ind,
          pressure.*
      FROM nfl_data.vw_combined_pressure pressure
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pressure.team_name = epa.posteam
          AND pressure.week = epa.week 
          AND pressure.season = epa.season 
  ")

query_pressure <- as_tibble(query_pressure) %>%
  filter(def_ssn %in% def_list_pressure$def_ssn)

exclude_cols_pressure <- c("def_ssn", "good_epa_def_ind", "team_name", "week", "season")
numeric_cols_pressure <- setdiff(names(query_pressure)[sapply(query_pressure, is.numeric)], exclude_cols_pressure)
cat("Processing", length(numeric_cols_pressure), "numeric features\n\n")

query_pressure <- query_pressure %>% arrange(def_ssn, week, season)

# ============================================================================
# FAST LEAVE-ONE-OUT
# ============================================================================

cat("Precomputing statistics...\n")

good_stats_pressure <- query_pressure %>%
  filter(good_epa_def_ind == 'Good') %>%
  group_by(def_ssn) %>%
  summarise(
    n_good = n(),
    across(all_of(numeric_cols_pressure), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

bad_stats_pressure <- query_pressure %>%
  filter(good_epa_def_ind == 'Bad') %>%
  group_by(def_ssn) %>%
  summarise(
    n_bad = n(),
    across(all_of(numeric_cols_pressure), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

all_good_means_pressure <- query_pressure %>%
  filter(good_epa_def_ind == 'Good') %>%
  group_by(def_ssn) %>%
  summarise(across(all_of(numeric_cols_pressure), mean, na.rm = TRUE), .groups = 'drop')

all_bad_means_pressure <- query_pressure %>%
  filter(good_epa_def_ind == 'Bad') %>%
  group_by(def_ssn) %>%
  summarise(across(all_of(numeric_cols_pressure), mean, na.rm = TRUE), .groups = 'drop')

cat("Processing", nrow(query_pressure), "games with fast LOO...\n")

xtd_pressure_df_list <- map(1:nrow(query_pressure), function(i) {
  
  if(i %% 500 == 0) cat("Processing game", i, "of", nrow(query_pressure), "\n")
  
  row <- query_pressure[i, ]
  rel_ssn <- row$def_ssn
  is_good <- row$good_epa_def_ind == 'Good'
  
  df_good <- all_good_means_pressure
  df_bad <- all_bad_means_pressure
  
  this_good_stats <- good_stats_pressure %>% filter(def_ssn == rel_ssn)
  this_bad_stats <- bad_stats_pressure %>% filter(def_ssn == rel_ssn)
  
  if(nrow(this_good_stats) == 0 || nrow(this_bad_stats) == 0) return(list(data = NULL, idx = i))
  
  if(is_good) {
    n <- this_good_stats$n_good
    if(n <= 1) return(list(data = NULL, idx = i))
    
    for(col in numeric_cols_pressure) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_good_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_good[df_good$def_ssn == rel_ssn, col] <- new_mean
    }
  } else {
    n <- this_bad_stats$n_bad
    if(n <= 1) return(list(data = NULL, idx = i))
    
    for(col in numeric_cols_pressure) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_bad_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_bad[df_bad$def_ssn == rel_ssn, col] <- new_mean
    }
  }
  
  df_good_ordered <- df_good %>% arrange(def_ssn)
  df_bad_ordered <- df_bad %>% 
    filter(def_ssn %in% df_good_ordered$def_ssn) %>% 
    arrange(def_ssn)
  df_good_ordered <- df_good_ordered %>% filter(def_ssn %in% df_bad_ordered$def_ssn)
  
  df_diff <- df_good_ordered[, -1] - df_bad_ordered[, -1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  
  def_ssn_col <- df_good_ordered[, 1, drop = FALSE]
  
  df_good2 <- df_good_ordered[, -1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_bad2 <- df_bad_ordered[, -1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_diff2 <- df_diff %>% as.data.frame() %>% mutate(across(everything(), scale))
  
  df_good2 <- cbind(def_ssn_col, df_good2)
  df_bad2 <- cbind(def_ssn_col, df_bad2)
  df_diff2 <- cbind(def_ssn_col, df_diff2)
  
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  
  def_good_row <- df_good2 %>% filter(def_ssn_Good == rel_ssn)
  def_bad_row <- df_bad2 %>% filter(def_ssn_Bad == rel_ssn)
  def_diff_row <- df_diff2 %>% filter(def_ssn == rel_ssn)
  
  if(nrow(def_good_row) == 0 || nrow(def_bad_row) == 0 || nrow(def_diff_row) == 0) return(list(data = NULL, idx = i))
  
  result <- cbind(def_good_row, def_bad_row, def_diff_row) %>% 
    select(-c(def_ssn_Bad, def_ssn))
  
  list(data = result, idx = i)
})

# Extract successful results and their indices
success_indices_pressure <- sapply(xtd_pressure_df_list, function(x) if(!is.null(x$data)) x$idx else NA)
success_indices_pressure <- success_indices_pressure[!is.na(success_indices_pressure)]

xtd_pressure_df <- map(xtd_pressure_df_list, function(x) x$data) %>% 
  purrr::compact() %>% 
  list_rbind()

cat("\n=== SUMMARY ===\n")
cat("Total games processed:", nrow(query_pressure), "\n")
cat("Successful feature rows:", nrow(xtd_pressure_df), "\n")
cat("Failed rows:", nrow(query_pressure) - nrow(xtd_pressure_df), "\n")

# ============================================================================
# JOIN WITH XTD TRAINING
# ============================================================================

xtd_training_pressure <- s3read_using(
  read.csv,
  object = "mixed_xtd/xtd_training.csv",
  bucket = "nfl-pff-data-lucas"
)

# Only join with rows that succeeded
xtd_pressure_df <- cbind(xtd_pressure_df, query_pressure[success_indices_pressure, ] %>% select(team_name, week, season))

xtd_pressure_df_train <- left_join(
  xtd_pressure_df, 
  xtd_training_pressure %>% select(posteam:season, mixed_xtd), 
  by = c("team_name" = "posteam", "week" = "week", "season" = "season")
) %>% filter(!is.na(mixed_xtd))

xtd_pressure_df_train_2 <- xtd_pressure_df_train %>%
  select(where(is.numeric)) %>%
  select(-c(week, season))

# ============================================================================
# MODEL TRAINING
# ============================================================================

task_pressure <- prepare_task_pressure(xtd_pressure_df_train_2, "pressure_characteristics")
tuning_pressure <- tune_xgb_model_pressure(task_pressure, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_pressure$best_params)
cat("\nBest CV RMSE:", round(tuning_pressure$best_score, 4), "\n\n")

final_model_pressure <- train_final_model_pressure(xtd_pressure_df_train_2, tuning_pressure$best_params)

eval_pressure <- evaluate_model_pressure(
  final_model_pressure$model,
  final_model_pressure$dtest,
  final_model_pressure$y_test,
  "Pressure Characteristics"
)

importance_pressure <- xgb.importance(final_model_pressure$feature_names, model = final_model_pressure$model)
cat("\nFeature Importance:\n")
print(importance_pressure)

# ============================================================================
# COMPARISON FUNCTION & SAVE
# ============================================================================

df_pressure_def_scaled_z <- xtd_pressure_df %>%
  select(-c(team_name, week, season)) %>%
  group_by(def_ssn_Good) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rename(def_ssn = def_ssn_Good)

importance_matrix_pressure_def <- importance_pressure

comparison_pressure_def_func <- function(def_ssn2, threshold, year = NULL) {
  relevant_features <- importance_matrix_pressure_def$Feature
  relevant_features <- relevant_features[relevant_features %in% colnames(df_pressure_def_scaled_z)]
  
  relevant_team <- df_pressure_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others <- df_pressure_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others_defs <- df_pressure_def_scaled_z %>% 
    filter(def_ssn != def_ssn2) %>% 
    pull(def_ssn)
  
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  weights <- importance_matrix_pressure_def %>%
    filter(Feature %in% relevant_features) %>%
    arrange(match(Feature, relevant_features)) %>%
    pull(Gain)
  
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  result_def <- rowSums(weighted_diff)
  result_def <- data.frame(Defense = all_others_defs, Score = result_def)
  
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Defense, nchar(Defense) - 3, nchar(Defense)) == as.character(year))
  }
  
  result_def %>% filter(Score <= threshold) %>% arrange(Score)
}

# Test it
comparison_pressure_def_func("PHI2025", .9)

# Save dependencies
list_dependencies_pressure_def <- list(
  df_pressure_def_scaled_z = df_pressure_def_scaled_z,
  importance_matrix_pressure_def = importance_matrix_pressure_def
)

save(list_dependencies_pressure_def, file = "list_dependencies_pressure_def.RData")
put_object(
  file = "list_dependencies_pressure_def.RData",
  object = "def_functions/list_dependencies_pressure_def.RData",
  bucket = "nfl-pff-data-lucas"
)

save(comparison_pressure_def_func, file = "comparison_pressure_def_func.rds")
put_object(
  file = "comparison_pressure_def_func.rds",
  object = "def_functions/comparison_pressure_def_func.rds",
  bucket = "nfl-pff-data-lucas"
)

save.image("~/df_pressure_def_build_workspace_AWS.RData")
cat("\nDONE.\n")