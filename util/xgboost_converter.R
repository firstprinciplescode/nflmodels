# Skip tuning - use known best params from earlier run
library(xgboost)

# Load data
s3load(object = "training_data/final_df.RData", bucket = bucket)

final_df_clean <- final_df %>% select(PPP, TOV_perc, ORB_perc, FTR, STL_perc, 
                                      `3P%`, `2Pperc`, poss, BLK_perc, `3PRate`,
                                      off_mean_def_home_perc_PPP:away_b2b_ind)
colnames(final_df_clean) <- gsub('%', '', colnames(final_df_clean))

METRIC <- "poss"
model_df <- final_df_clean %>% select(all_of(METRIC), off_mean_def_home_perc_PPP:away_b2b_ind)

set.seed(42)
split <- sample.split(model_df[[METRIC]], SplitRatio = 0.85)
train <- subset(model_df, split == TRUE)
test <- subset(model_df, split == FALSE)

X_train <- train %>% select(-all_of(METRIC)) %>% as.matrix()
X_test <- test %>% select(-all_of(METRIC)) %>% as.matrix()
y_train <- train[[METRIC]]
y_test <- test[[METRIC]]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Known best params from your earlier run
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05743589,
  gamma = 4.392237,
  max_depth = 5,
  min_child_weight = 2.248039,
  alpha = 0.5653827,
  lambda = 0.8236206,
  colsample_bynode = 0.9737402,
  colsample_bylevel = 0.8993507,
  colsample_bytree = 0.9672737,
  nthread = 4
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 230,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50
)

# Save in NEW format
xgb.save(model, "poss_model.ubj")

# Upload to S3
put_object(file = "poss_model.ubj", object = "models/poss_model.ubj", bucket = bucket)

cat("Done! Saved poss_model.ubj to S3\n")