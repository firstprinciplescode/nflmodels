# ============================================================================
# PRESSURE OFFENSE BUILD - FAST VERSION
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
    evals = list(train = dtrain, test = dtest),
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

qbgrp_list <- dbGetQuery(con, "
    SELECT qbgrp_ssn, COUNT(*) as game_count
    FROM nfl_data.combined_grade_epa_summary
    GROUP BY qbgrp_ssn
    HAVING COUNT(*) >= 10
")

query_pressure <- dbGetQuery(con, "
      SELECT  
          epa.qbgrp_ssn,
          epa.good_off_ind,
          depth.*
      FROM nfl_data.vw_combined_pressure depth
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON depth.team_name = epa.posteam
          AND depth.week = epa.week 
          AND depth.season = epa.season 
  ")

query_pressure <- as_tibble(query_pressure) %>%
  filter(qbgrp_ssn %in% qbgrp_list$qbgrp_ssn)

exclude_cols <- c("qbgrp_ssn", "good_off_ind", "team_name", "week", "season")
numeric_cols <- setdiff(names(query_pressure)[sapply(query_pressure, is.numeric)], exclude_cols)
cat("Processing", length(numeric_cols), "numeric features\n\n")

query_pressure <- query_pressure %>% arrange(qbgrp_ssn, week, season)

# ============================================================================
# CHANGE START: FAST LEAVE-ONE-OUT (replaces the slow for loop)
# ============================================================================

cat("Precomputing statistics...\n")

good_stats <- query_pressure %>%
  filter(good_off_ind == 'Good') %>%
  group_by(qbgrp_ssn) %>%
  summarise(
    n_good = n(),
    across(all_of(numeric_cols), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

bad_stats <- query_pressure %>%
  filter(good_off_ind == 'Bad') %>%
  group_by(qbgrp_ssn) %>%
  summarise(
    n_bad = n(),
    across(all_of(numeric_cols), list(sum = ~sum(., na.rm = TRUE))),
    .groups = 'drop'
  )

# Precompute overall means for ALL QBs
all_good_means <- query_pressure %>%
  filter(good_off_ind == 'Good') %>%
  group_by(qbgrp_ssn) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')

all_bad_means <- query_pressure %>%
  filter(good_off_ind == 'Bad') %>%
  group_by(qbgrp_ssn) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE), .groups = 'drop')

cat("Processing", nrow(query_pressure), "games with fast LOO...\n")

xtd_depth_df <- map_dfr(1:nrow(query_pressure), function(i) {
  
  if(i %% 500 == 0) cat("Processing game", i, "of", nrow(query_pressure), "\n")
  
  row <- query_pressure[i, ]
  rel_ssn <- row$qbgrp_ssn
  is_good <- row$good_off_ind == 'Good'
  
  # Start with all QBs' means
  df_good <- all_good_means
  df_bad <- all_bad_means
  
  # Get this QB's stats
  this_good_stats <- good_stats %>% filter(qbgrp_ssn == rel_ssn)
  this_bad_stats <- bad_stats %>% filter(qbgrp_ssn == rel_ssn)
  
  # Skip if QB doesn't have both good and bad games
  if(nrow(this_good_stats) == 0 || nrow(this_bad_stats) == 0) return(NULL)
  
  # Adjust means for THIS QB only (remove current game's contribution)
  # Formula: new_mean = (sum - removed_value) / (n - 1)
  if(is_good) {
    n <- this_good_stats$n_good
    if(n <= 1) return(NULL)  # Can't leave one out if only 1 game
    
    for(col in numeric_cols) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_good_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_good[df_good$qbgrp_ssn == rel_ssn, col] <- new_mean
    }
  } else {
    n <- this_bad_stats$n_bad
    if(n <= 1) return(NULL)
    
    for(col in numeric_cols) {
      sum_col <- paste0(col, "_sum")
      old_sum <- this_bad_stats[[sum_col]]
      removed_val <- row[[col]]
      new_mean <- (old_sum - removed_val) / (n - 1)
      df_bad[df_bad$qbgrp_ssn == rel_ssn, col] <- new_mean
    }
  }
  
  # Calculate differences (align QBs that exist in both)
  df_good_ordered <- df_good %>% arrange(qbgrp_ssn)
  df_bad_ordered <- df_bad %>% 
    filter(qbgrp_ssn %in% df_good_ordered$qbgrp_ssn) %>% 
    arrange(qbgrp_ssn)
  df_good_ordered <- df_good_ordered %>% filter(qbgrp_ssn %in% df_bad_ordered$qbgrp_ssn)
  
  df_diff <- df_good_ordered[, -1] - df_bad_ordered[, -1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  
  qbgrp_ssn_col <- df_good_ordered[, 1, drop = FALSE]
  
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
  
  # Add QB identifier back
  df_good2 <- cbind(qbgrp_ssn_col, df_good2)
  df_bad2 <- cbind(qbgrp_ssn_col, df_bad2)
  df_diff2 <- cbind(qbgrp_ssn_col, df_diff2)
  
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  
  # Extract row for the held-out game's QB
  qb_good_row <- df_good2 %>% filter(qbgrp_ssn_Good == rel_ssn)
  qb_bad_row <- df_bad2 %>% filter(qbgrp_ssn_Bad == rel_ssn)
  qb_diff_row <- df_diff2 %>% filter(qbgrp_ssn == rel_ssn)
  
  if(nrow(qb_good_row) == 0 || nrow(qb_bad_row) == 0 || nrow(qb_diff_row) == 0) return(NULL)
  
  cbind(qb_good_row, qb_bad_row, qb_diff_row) %>% 
    select(-c(qbgrp_ssn_Bad, qbgrp_ssn))
  
})

cat("\n=== SUMMARY ===\n")
cat("Total games processed:", nrow(query_pressure), "\n")
cat("Successful feature rows:", nrow(xtd_depth_df), "\n")
cat("Failed rows:", nrow(query_pressure) - nrow(xtd_depth_df), "\n")

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

xtd_pressure_df <- cbind(xtd_depth_df, query_pressure %>% select(team_name, week, season))

xtd_pressure_df_train <- left_join(
  xtd_pressure_df, 
  xtd_training %>% select(posteam:season, mixed_xtd), 
  by = c("team_name" = "posteam", "week" = "week", "season" = "season")
) %>% filter(!is.na(mixed_xtd))

xtd_pressure_df_train_2 <- xtd_pressure_df_train %>%
  select(where(is.numeric)) %>%
  select(-c(week, season))

# ============================================================================
# MODEL TRAINING
# ============================================================================

task_before_xtd_no_temp <- prepare_task(xtd_pressure_df_train_2, "pressure_characteristics")
tuning_before_xtd_no_temp <- tune_xgb_model(task_before_xtd_no_temp, n_evals = 125)

cat("\nBest hyperparameters found:\n")
print(tuning_before_xtd_no_temp$best_params)
cat("\nBest CV RMSE:", round(tuning_before_xtd_no_temp$best_score, 4), "\n\n")

final_model_no_temp <- train_final_model(xtd_pressure_df_train_2, tuning_before_xtd_no_temp$best_params)

eval_no_temp <- evaluate_model(
  final_model_no_temp$model,
  final_model_no_temp$dtest,
  final_model_no_temp$y_test,
  "Pressure Characteristics"
)

importance_no_temp <- xgb.importance(final_model_no_temp$feature_names, model = final_model_no_temp$model)
cat("\nFeature Importance:\n")
print(importance_no_temp)

# ============================================================================
# COMPARISON FUNCTION & SAVE
# ============================================================================

df_pressure_scaled_z <- xtd_pressure_df %>%
  select(-c(team_name, week, season)) %>%
  group_by(qbgrp_ssn_Good) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  rename(qbgrp_ssn = qbgrp_ssn_Good)

importance_matrix_pressure <- importance_no_temp

comparison_pressure_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  relevant_features <- importance_matrix_pressure$Feature
  relevant_features <- relevant_features[relevant_features %in% colnames(df_pressure_scaled_z)]
  
  relevant_team <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%
    select(all_of(relevant_features))
  
  all_others_qbs <- df_pressure_scaled_z %>% 
    filter(qbgrp_ssn != qbgrp_ssn2) %>% 
    pull(qbgrp_ssn)
  
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  weights <- importance_matrix_pressure %>%
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
comparison_pressure_func("SEADarnold-2025", .95)

# Save dependencies
list_dependencies_pressure <- list(
  df_pressure_scaled_z = df_pressure_scaled_z,
  importance_matrix_pressure = importance_matrix_pressure
)

save(list_dependencies_pressure, file = "list_dependencies_pressure.RData")
put_object(
  file = "list_dependencies_pressure.RData",
  object = "qbgrp_def_functions/list_dependencies_pressure.RData",
  bucket = "nfl-pff-data-lucas"
)

save(comparison_pressure_func, file = "comparison_pressure_func.rds")
put_object(
  file = "comparison_pressure_func.rds",
  object = "qbgrp_def_functions/comparison_pressure_func.rds",
  bucket = "nfl-pff-data-lucas"
)

save.image("~/df_pressure_off_build_workspace_AWS.RData")