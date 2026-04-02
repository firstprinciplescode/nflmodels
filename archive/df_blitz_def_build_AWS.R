# Get all defenses with >14 games
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

conflict_prefer_all("dplyr", quiet = TRUE)

def_list <- run_athena_query("
    SELECT def_ssn, COUNT(*) as game_count
    FROM nfl_data.combined_grade_epa_summary
    GROUP BY def_ssn
    HAVING COUNT(*) >= 10
")

query_blitz <- run_athena_query("
      SELECT  
          epa.def_ssn,
          epa.good_epa_def_ind,
          blitz.*
      FROM nfl_data.vw_combined_blitz blitz
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON blitz.team_name = epa.posteam
          AND blitz.week = epa.week 
          AND blitz.season = epa.season 
  ")

query_blitz <- as_tibble(query_blitz) %>%
  filter(def_ssn %in% def_list$def_ssn)

# Identify numeric columns to process (exclude identifiers)
exclude_cols <- c("def_ssn", "good_epa_def_ind", "team_name", "week", "season")
numeric_cols <- setdiff(names(query_blitz)[sapply(query_blitz, is.numeric)], exclude_cols)
cat("Processing", length(numeric_cols), "numeric features\n\n")

# Initialize output
# xtd_blitz_df <- data.frame()

query_blitz <- query_blitz %>%
  arrange(def_ssn, week, season)

# Leave-one-game-out loop
for(i in 1:nrow(query_blitz)) {
  
  if(i %% 100 == 0) cat("Processing game", i, "of", nrow(query_blitz), "\n")
  
  rel_ssn <- query_blitz[i, ]$def_ssn
  df_press <- query_blitz[-i, ]
  
  # Calculate means by defense for Good performance
  df_good <- df_press %>%
    filter(good_epa_def_ind == 'Good') %>%
    group_by(def_ssn) %>%
    summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')
  
  # Calculate means by defense for Bad performance
  df_bad <- df_press %>%
    filter(good_epa_def_ind == 'Bad') %>%
    group_by(def_ssn) %>%
    summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')
  
  # Calculate differences
  df_diff <- df_good[, -1] - df_bad[, -1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  
  def_ssn <- df_good[, 1]
  
  # Scale features
  df_good2 <- df_good[, -1] %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  df_bad2 <- df_bad[, -1] %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  df_diff2 <- df_diff %>% 
    as.data.frame() %>% 
    mutate(across(everything(), scale))
  
  # Add defense identifier back
  df_good2 <- cbind(def_ssn, df_good2)
  df_bad2 <- cbind(def_ssn, df_bad2)
  df_diff2 <- cbind(def_ssn, df_diff2)
  
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  
  # Extract features for the defense from the held-out game
  def_good_row <- df_good2 %>% filter(def_ssn_Good == rel_ssn)
  def_bad_row <- df_bad2 %>% filter(def_ssn_Bad == rel_ssn)
  def_diff_row <- df_diff2 %>% filter(def_ssn == rel_ssn)
  
  # Check for failures and WARN
  if(nrow(def_good_row) == 0) {
    warning(paste("ITERATION", i, "- Defense", rel_ssn, "has NO GOOD GAMES in training data after removing game", i))
  }
  if(nrow(def_bad_row) == 0) {
    warning(paste("ITERATION", i, "- Defense", rel_ssn, "has NO BAD GAMES in training data after removing game", i))
  }
  if(nrow(def_diff_row) == 0) {
    warning(paste("ITERATION", i, "- Defense", rel_ssn, "missing from diff calculation"))
  }
  
  # Only combine if all exist
  if(nrow(def_good_row) > 0 && nrow(def_bad_row) > 0 && nrow(def_diff_row) > 0) {
    xtd_blitz_df <- rbind(xtd_blitz_df, 
                          cbind(def_good_row, def_bad_row, def_diff_row) %>% 
                            select(-c(def_ssn_Bad, def_ssn)))
  }
  
  print(paste(i, rel_ssn))
}

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("COMPLETE - Created", nrow(xtd_blitz_df), "game feature rows\n")
cat("Total games processed:", nrow(query_blitz), "\n")
cat("Successful feature rows:", nrow(xtd_blitz_df), "\n")
cat("Failed rows:", nrow(query_blitz) - nrow(xtd_blitz_df), "\n")
cat(paste(rep("=", 80), collapse=""), "\n")



xtd_training <- s3read_using(
  read.csv,
  object = "mixed_xtd/xtd_training.csv",
  bucket = "nfl-pff-data-lucas"
)


cbind(xtd_blitz_df, query_blitz %>% select(team_name, week, season)) %>%
  group_by(def_ssn_Good) %>%
  dplyr::summarise(n_team = n_distinct(team_name),
                   n_ssn = n_distinct(season)) %>%
  filter(n_ssn > 1)

xtd_blitz_df <- cbind(xtd_blitz_df, query_blitz %>% select(team_name, week, season))

left_join(xtd_blitz_df, 
          xtd_training %>% select(posteam:season, mixed_xtd), 
          by = c("team_name" = "posteam", "week" = "week", "season" = "season")) %>%
  filter(is.na(mixed_xtd))
# FUCK. 2025, BUT ONLY SOME TEAMS AND WEIRD ONES 
# WILL NEED TO LOOK AT IT, BUT FOR NOW - WE WON'T NEED IT NEED IT. TRAIN WITHOUT. 
# IT'S SOMETHING WITH THE XTD_TRAINING STEP. THE SHIT ... IDK, BUT PBP_XTD HAS THOSE WEEKS BUT NOT PBP

xtd_training %>% filter(season == 2025 & posteam %in% c("DEN", "IND", "LAC", "MIA", "NE"))


xtd_blitz_df_train <- 
  left_join(xtd_blitz_df, 
            xtd_training %>% select(posteam:season, mixed_xtd), 
            by = c("team_name" = "posteam", "week" = "week", "season" = "season")) %>%
  filter(!is.na(mixed_xtd))


####
####
####


# REGRESSION VERSION
prepare_task <- function(df, task_id, target_col = "mixed_xtd") {
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

train_final_model <- function(df, best_params, target_col = "mixed_xtd") {
  # NO conversion to factor/integer - keep as numeric
  
  set.seed(42)
  split <- sample.split(Y = df[[target_col]], SplitRatio = 0.8)
  train <- subset(df, split == TRUE) %>% 
    filter(!is.na(!!sym(target_col)))
  test <- subset(df, split == FALSE) %>% 
    filter(!is.na(!!sym(target_col)))
  
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
    objective = "reg:squarederror",  # Regression
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


# ============================================================================
# DATA PREPARATION
# ============================================================================


xtd_blitz_df_train_2 <- xtd_blitz_df_train %>%
  select(where(is.numeric)) %>%
  select(-c(week, season))


###
###


task_before_xtd_no_temp <- prepare_task(xtd_blitz_df_train_2, "blitz_characteristics")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xtd_blitz_df_train_2, tuning_before_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "blitz Characteristics"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)


####
####
####


df_blitz_def_scaled_z <- xtd_blitz_df %>%
  select(-c(team_name, week, season)) %>%
  group_by(def_ssn_Good) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rename(def_ssn = def_ssn_Good)

# Get feature importance from your trained model
importance_matrix_blitz_def <- importance_no_temp

# Comparison function
comparison_blitz_def_func <- function(def_ssn2, threshold, year = NULL) {
  
  # Extract relevant features from importance matrix
  relevant_features <- importance_matrix_blitz_def$Feature
  
  # Filter to features that exist in our data
  relevant_features <- relevant_features[relevant_features %in% colnames(df_blitz_def_scaled_z)]
  
  # Get the target QB's features
  relevant_team <- df_blitz_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%
    select(all_of(relevant_features))
  
  # Get all other QBs
  all_others <- df_blitz_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%
    select(all_of(relevant_features))
  
  # Preserve QB identifiers
  all_others_qbs <- df_blitz_def_scaled_z %>% 
    filter(def_ssn != def_ssn2) %>% 
    pull(def_ssn)
  
  # Calculate absolute difference for each QB
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Get weights from importance matrix (only for features we're using)
  weights <- importance_matrix_blitz_def %>%
    filter(Feature %in% relevant_features) %>%
    arrange(match(Feature, relevant_features)) %>%
    pull(Gain)
  
  # Calculate weighted differences
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each QB
  result_qb <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_qb <- data.frame(QB = all_others_qbs, Score = result_qb)
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_qb <- result_qb %>%
      filter(substr(QB, nchar(QB) - 3, nchar(QB)) == as.character(year))
  }
  
  # Apply threshold filter
  result_qb <- result_qb %>% 
    filter(Score <= threshold) %>% 
    arrange(Score)
  
  return(result_qb)
}

# Save dependencies
list_dependencies_blitz_def <- list(
  df_blitz_def_scaled_z = df_blitz_def_scaled_z,
  importance_matrix_blitz_def = importance_matrix_blitz_def
)

comparison_blitz_def_func("LA2025", .9)


save(list_dependencies_blitz_def, file = "list_dependencies_blitz_def.RData")

# Upload dependencies
put_object(
  file = "list_dependencies_blitz_def.RData",
  object = "def_functions/list_dependencies_blitz_def.RData",
  bucket = "nfl-pff-data-lucas"
)


save(comparison_blitz_def_func, file = "comparison_blitz_def_func.rds")

# Upload function
put_object(
  file = "comparison_blitz_def_func.rds",
  object = "def_functions/comparison_blitz_def_func.rds",
  bucket = "nfl-pff-data-lucas"
)



