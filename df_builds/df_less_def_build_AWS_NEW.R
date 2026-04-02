# ============================================================================
# PASSING TIP (LESS) DEFENSE BUILD - FAST VERSION
# ============================================================================
# NOTE: Uses good_def_ind (not good_epa_def_ind)
# NOTE: Output column stays "QB" for compatibility with other comparison functions
# ============================================================================

conflict_prefer_all("dplyr", quiet = TRUE)

# Uses existing Athena connection via con (noctua/RAthena)

# ============================================================================
# MODEL FUNCTIONS (UPDATED FOR NEW XGBOOST API)
# ============================================================================

prepare_task <- function(df, task_id, target_col = "mixed_xtd") {
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

train_final_model <- function(df, best_params, target_col = "mixed_xtd") {
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

evaluate_model <- function(model, dtest, y_test, model_name = "Model") {
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

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
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
  
  if (status != "SUCCEEDED") {
    # Get error message
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
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

def_list <- run_athena_query("
    SELECT def_ssn, COUNT(*) as game_count
    FROM nfl_data.combined_grade_epa_summary
    GROUP BY def_ssn
    HAVING COUNT(*) >= 14
")

query_less <- run_athena_query("
      SELECT  
          epa.def_ssn,
          epa.good_def_ind,
          pa.*
      FROM nfl_data.vw_passing_tip pa
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pa.team_name = epa.posteam
          AND pa.week = epa.week 
          AND pa.season = epa.season 
  ")

query_less <- as_tibble(query_less) %>%
  filter(def_ssn %in% def_list$def_ssn)

exclude_cols <- c("def_ssn", "good_def_ind", "team_name", "week", "season")
numeric_cols <- setdiff(names(query_less)[sapply(query_less, is.numeric)], exclude_cols)
cat("Processing", length(numeric_cols), "numeric features\n\n")

query_less <- query_less %>% arrange(def_ssn, week, season)

# ============================================================================
# CHANGE START: FAST LEAVE-ONE-OUT (replaces the slow for loop)
# ============================================================================

cat("Precomputing statistics...\n")

good_stats <- query_less %>%
  filter(good_def_ind == 'Good') %>%
  group_by(def_ssn) %>%
  summarise(
    n_good = n(),
    across(all_of(numeric_cols), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

bad_stats <- query_less %>%
  filter(good_def_ind == 'Bad') %>%
  group_by(def_ssn) %>%
  summarise(
    n_bad = n(),
    across(all_of(numeric_cols), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

# Precompute overall means for ALL defenses
all_good_means <- query_less %>%
  filter(good_def_ind == 'Good') %>%
  group_by(def_ssn) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')

all_bad_means <- query_less %>%
  filter(good_def_ind == 'Bad') %>%
  group_by(def_ssn) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')

cat("Processing", nrow(query_less), "games with fast LOO...\n")

xtd_less_df <- map_dfr(1:nrow(query_less), function(i) {
  
  if(i %% 500 == 0) cat("Processing game", i, "of", nrow(query_less), "\n")
  
  row <- query_less[i, ]
  rel_ssn <- row$def_ssn
  is_good <- row$good_def_ind == 'Good'
  
  # Start with all defenses' means
  df_good <- all_good_means
  df_bad <- all_bad_means
  
  # Get this defense's stats
  this_good_stats <- good_stats %>% filter(def_ssn == rel_ssn)
  this_bad_stats <- bad_stats %>% filter(def_ssn == rel_ssn)
  
  # Skip if defense doesn't have both good and bad games
  if(nrow(this_good_stats) == 0 || nrow(this_bad_stats) == 0) return(NULL)
  
  # Adjust means for THIS defense only (remove current game's contribution)
  # Formula: new_mean = (sum - removed_value) / (n - 1)
  if(is_good) {
    n <- this_good_stats$n_good
    if(n <= 1) return(NULL)  # Can't leave one out if only 1 game
    
    for(col in numeric_cols) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_good_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_good[df_good$def_ssn == rel_ssn, col] <- new_mean
    }
  } else {
    n <- this_bad_stats$n_bad
    if(n <= 1) return(NULL)
    
    for(col in numeric_cols) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_bad_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_bad[df_bad$def_ssn == rel_ssn, col] <- new_mean
    }
  }
  
  # Calculate differences (align defenses that exist in both)
  df_good_ordered <- df_good %>% arrange(def_ssn)
  df_bad_ordered <- df_bad %>% 
    filter(def_ssn %in% df_good_ordered$def_ssn) %>% 
    arrange(def_ssn)
  df_good_ordered <- df_good_ordered %>% filter(def_ssn %in% df_bad_ordered$def_ssn)
  
  df_diff <- df_good_ordered[, -1] - df_bad_ordered[, -1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  
  def_ssn_col <- df_good_ordered[, 1, drop = FALSE]
  
  # Scale features
  df_good2 <- df_good_ordered[, -1] %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  df_bad2 <- df_bad_ordered[, -1] %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  df_diff2 <- df_diff %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  # Add defense identifier back
  df_good2 <- cbind(def_ssn_col, df_good2)
  df_bad2 <- cbind(def_ssn_col, df_bad2)
  df_diff2 <- cbind(def_ssn_col, df_diff2)
  
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  
  # Extract row for the held-out game's defense
  def_good_row <- df_good2 %>% filter(def_ssn_Good == rel_ssn)
  def_bad_row <- df_bad2 %>% filter(def_ssn_Bad == rel_ssn)
  def_diff_row <- df_diff2 %>% filter(def_ssn == rel_ssn)
  
  if(nrow(def_good_row) == 0 || nrow(def_bad_row) == 0 || nrow(def_diff_row) == 0) return(NULL)
  
  cbind(def_good_row, def_bad_row, def_diff_row) %>% 
    select(-c(def_ssn_Bad, def_ssn))
  
})

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("COMPLETE - Created", nrow(xtd_less_df), "game feature rows\n")
cat(paste(rep("=", 80), collapse=""), "\n")

# ============================================================================
# CHANGE END
# ============================================================================

# ============================================================================
# JOIN WITH XTD TRAINING
# ============================================================================

xtd_training <- s3read_using(
  read.csv,
  object = "mixed_xtd/xtd_training.csv",
  bucket = "nfl-pff-data-lucas"
)

xtd_less_df <- cbind(xtd_less_df, query_less %>% select(team_name, week, season))

xtd_less_df_train <- left_join(
  xtd_less_df, 
  xtd_training %>% select(posteam:season, mixed_xtd), 
  by = c("team_name" = "posteam", "week" = "week", "season" = "season")
) %>% filter(!is.na(mixed_xtd))

xtd_less_df_train_2 <- xtd_less_df_train %>%
  select(where(is.numeric)) %>%
  select(-c(week, season))

# ============================================================================
# MODEL TRAINING
# ============================================================================

task_before_xtd_no_temp <- prepare_task(xtd_less_df_train_2, "less_def_characteristics")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xtd_less_df_train_2, tuning_before_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Less Defense Characteristics"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)

# ============================================================================
# COMPARISON FUNCTION & SAVE
# ============================================================================

df_less_def_scaled_z <- xtd_less_df %>%
  select(-c(team_name, week, season)) %>%
  group_by(def_ssn_Good) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rename(def_ssn = def_ssn_Good)

importance_matrix_less_def <- importance_no_temp

# NOTE: Output column stays "QB" for compatibility with other comparison functions
comparison_less_def_func <- function(def_ssn2, threshold, year = NULL) {
  relevant_features <- importance_matrix_less_def$Feature
  relevant_features <- relevant_features[relevant_features %in% colnames(df_less_def_scaled_z)]
  
  relevant_team <- df_less_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others <- df_less_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others_qbs <- df_less_def_scaled_z %>% 
    filter(def_ssn != def_ssn2) %>% 
    pull(def_ssn)
  
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  weights <- importance_matrix_less_def %>%
    filter(Feature %in% relevant_features) %>%
    arrange(match(Feature, relevant_features)) %>%
    pull(Gain)
  
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  result_qb <- rowSums(weighted_diff)
  result_qb <- data.frame(QB = all_others_qbs, Score = result_qb)  # Keeps "QB" for compatibility
  
  if (!is.null(year)) {
    result_qb <- result_qb %>%
      filter(substr(QB, nchar(QB) - 3, nchar(QB)) == as.character(year))
  }
  
  result_qb %>% filter(Score <= threshold) %>% arrange(Score)
}

# Test it
comparison_less_def_func("PHI2025", .9)

# Save dependencies
list_dependencies_less_def <- list(
  df_less_def_scaled_z = df_less_def_scaled_z,
  importance_matrix_less_def = importance_matrix_less_def
)

save(list_dependencies_less_def, file = "list_dependencies_less_def.RData")
put_object(
  file = "list_dependencies_less_def.RData",
  object = "def_functions/list_dependencies_less_def.RData",
  bucket = "nfl-pff-data-lucas"
)

save(comparison_less_def_func, file = "comparison_less_def_func.rds")
put_object(
  file = "comparison_less_def_func.rds",
  object = "def_functions/comparison_less_def_func.rds",
  bucket = "nfl-pff-data-lucas"
)

save.image("~/df_less_def_build_workspace_AWS.RData")