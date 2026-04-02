library(aws.s3)

# Read pbp_xtd
pbp_xtd <- s3read_using(
  read.csv,
  object = "pbp_xtd/pbp_xtd.csv",
  bucket = "nfl-pff-data-lucas"
)

colnames(pbp_xtd)[c(5:8)] <- paste0("pbp_", colnames(pbp_xtd)[c(5:8)])

# Read part_xtd
part_xtd <- s3read_using(
  read.csv,
  object = "part_xtd/part_xtd.csv",
  bucket = "nfl-pff-data-lucas"
)

colnames(part_xtd)[c(5:8)] <- paste0("part_", colnames(part_xtd)[c(5:8)])


xtd_training <- full_join(pbp_xtd %>% select(-actual_fg), 
          part_xtd %>% select(-c(actual_td)), 
          by = c("posteam", "defteam", "week", "season")) %>%
  filter(season <= 2024)


# Define predictors
features <- c("pbp_before_old_xtd", "pbp_before_new_xtd", "pbp_after_old_xtd", "pbp_after_new_xtd",
              "part_before_old_xtd", "part_before_new_xtd", "part_after_old_xtd", "part_after_new_xtd")

# Prep data
model_data <- xtd_training %>%
  ungroup() %>%
  select(all_of(features), actual_td) %>%
  na.omit()

# Train/test split
set.seed(42)
train_idx <- sample(1:nrow(model_data), 0.8 * nrow(model_data))
train <- model_data[train_idx, ]
test <- model_data[-train_idx, ]

# ============================================
# 1. LINEAR REGRESSION (simplest)
# ============================================
lm_model <- lm(actual_td ~ ., data = train)
summary(lm_model)

lm_preds <- predict(lm_model, test)
lm_rmse <- sqrt(mean((test$actual_td - lm_preds)^2))
lm_mae <- mean(abs(test$actual_td - lm_preds))
cat("Linear Regression - RMSE:", lm_rmse, "MAE:", lm_mae, "\n")          
          
#### SO EVERYTHING EVEN, NOT DOING XGBOOST


# Fit full model first
# Backward elimination with p < 0.125
backward_select <- function(data, target, threshold = 0.125) {
  current_vars <- setdiff(names(data), target)
  
  while(TRUE) {
    formula <- as.formula(paste(target, "~", paste(current_vars, collapse = " + ")))
    model <- lm(formula, data = data)
    
    pvals <- summary(model)$coefficients[-1, "Pr(>|t|)"]  # Exclude intercept
    max_p <- max(pvals)
    
    if (max_p >= threshold) {
      drop_var <- names(which.max(pvals))
      cat("Dropping", drop_var, "- p =", round(max_p, 4), "\n")
      current_vars <- setdiff(current_vars, drop_var)
    } else {
      break
    }
  }
  
  return(model)
}

lm_selected <- backward_select(train, "actual_td", threshold = 0.125)
summary(lm_selected) 

# Predict on test set
lm_preds <- predict(lm_selected, test)

# Calculate metrics
lm_rmse <- sqrt(mean((test$actual_td - lm_preds)^2))
lm_mae <- mean(abs(test$actual_td - lm_preds))

cat("RMSE:", lm_rmse, "\nMAE:", lm_mae, "\n")



### NOW FOR 2025 SHIT 


# Full model
lm_full <- lm(actual_td ~ pbp_before_old_xtd + pbp_before_new_xtd + pbp_after_old_xtd + pbp_after_new_xtd, data = train)

# Stepwise (backward)
lm_step <- step(lm_full, direction = "backward")
summary(lm_step)

# Evaluate
preds <- predict(lm_step, test)
rmse <- sqrt(mean((test$actual_td - preds)^2))
mae <- mean(abs(test$actual_td - preds))

cat("RMSE:", rmse, "\nMAE:", mae, "\n")


# Create folder
dir.create("xtd_normalization", showWarnings = FALSE)

# Save models
saveRDS(lm_selected, "xtd_normalization/pbp_part_model.rds")
saveRDS(lm_step, "xtd_normalization/pbp_only_model.rds")


put_object(file = "xtd_normalization/pbp_part_model.rds", 
           object = "models/xtd_normalization/pbp_part_model.rds", 
           bucket = "nfl-pff-data-lucas")

put_object(file = "xtd_normalization/pbp_only_model.rds", 
           object = "models/xtd_normalization/pbp_only_model.rds", 
           bucket = "nfl-pff-data-lucas")


xtd_USE_THIS <- 
  full_join(pbp_xtd %>% select(-actual_fg), 
            part_xtd %>% select(-c(actual_td)), 
            by = c("posteam", "defteam", "week", "season"))


xtd_USE_THIS <- xtd_USE_THIS %>%
  mutate(
    mixed_xtd = case_when(
      season <= 2024 ~ predict(lm_selected, .),
      season == 2025 ~ predict(lm_step, .),
      TRUE ~ NA_real_
    )
  )


s3write_using(xtd_USE_THIS, write.csv, row.names = FALSE,
              object = "mixed_xtd/xtd_training.csv",
              bucket = "nfl-pff-data-lucas")
