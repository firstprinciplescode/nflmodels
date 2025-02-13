#### 
####
####

# I HAVE A PBP, PART XTD - I'M GOING TO HAVE TO WEIGHT THE TWO TO FIND OUT 60% PART, 40% PART, ETC. 

library(nflreadr)
library(nflfastR)
library(sqldf)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(flexclust)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)


setwd("C:/Users/vflre/Downloads/NFL Models")

pbp_df <- read.csv('pbp_xtd.csv') %>% select(-X)

part_df <- read.csv('part_xtd.csv') %>% select(-X)

colnames(pbp_df) <- paste0("pbp_", colnames(pbp_df))
colnames(part_df) <- paste0("part_", colnames(part_df))

###
###

combined_xtd_df <- left_join(pbp_df, part_df %>% select(part_posteam:part_after_new_xtd), by = c("pbp_posteam" = "part_posteam", "pbp_week" = "part_week", "pbp_season" = "part_season")) 


combined_xtd_reg_df <- combined_xtd_df %>% select(pbp_before_old_xtd:pbp_after_new_xtd, part_before_old_xtd:part_after_new_xtd, pbp_actual_td) %>% drop_na()


#####
#####

sample_split_xtd_df <- sample.split(Y = combined_xtd_reg_df$pbp_actual_td, SplitRatio = 0.8)
train_set_xtd_df <- subset(x = combined_xtd_reg_df, sample_split_xtd_df == TRUE)
test_set_xtd_df <- subset(x = combined_xtd_reg_df, sample_split_xtd_df == FALSE)


X_train_xtd_df <- train_set_xtd_df %>% select(-pbp_actual_td) %>% as.data.frame()
X_test_xtd_df <- test_set_xtd_df %>% select(-pbp_actual_td) %>% as.data.frame()
y_train_xtd_df <- train_set_xtd_df$pbp_actual_td
y_test_xtd_df <- test_set_xtd_df$pbp_actual_td


dtrain_xtd_df = xgb.DMatrix(data = as.matrix(X_train_xtd_df), label = y_train_xtd_df)
dtest_xtd_df = xgb.DMatrix(data =as.matrix(X_test_xtd_df), label = y_test_xtd_df)

d_xpass_all = combined_xtd_reg_df %>% select(-pbp_actual_td)
d_ypass_all = combined_xtd_reg_df$pbp_actual_td
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_xtd_df = list(train=dtrain_xtd_df, test=dtest_xtd_df)


eta_xtd_df = .005 # .005
gamma_xtd_df = 4.5 # 4.5
max_depth_xtd_df = 5 # 5
min_child_weight_xtd_df = 0 # 0
alpha_xtd_df = 0 # 0
lambda_xtd_df = .1 # .1
colsample_bynode_xtd_df = .7 # .7
colsample_bylevel_xtd_df = .7 # .7
colsample_bytree_xtd_df = .85 # .85

xgb_xtd_df <- xgboost(data = dtrain_xtd_df, 
                             label = y_train_xtd_df, 
                             eta = eta_xtd_df,
                             max_depth = max_depth_xtd_df, 
                             alpha = alpha_xtd_df,
                             lambda = lambda_xtd_df,
                             min_child_weight = min_child_weight_xtd_df,
                             colsample_bynode = colsample_bynode_xtd_df,
                             colsample_bytree = colsample_bytree_xtd_df,
                             colsample_bylevel = colsample_bylevel_xtd_df,
                             nround = 700, # 700
                             objective = "reg:squarederror",
                             nthread = 2,
                             gamma = gamma_xtd_df,
                             early_stopping_rounds = 50
)


test_preds = predict(xgb_xtd_df, newdata = xgb.DMatrix(X_test_xtd_df %>% as.matrix()))


xtd_df_breaks <- seq(0, 6, by = 0.25)

xtd_df_labels <- sprintf("%.3f", head(xtd_df_breaks, -1))

resultant_df_xtd_df = cbind(test_preds, y_test_xtd_df) %>% as.data.frame()
colnames(resultant_df_xtd_df) = c("Preds", "Vals")

resultant_df_xtd_df$buckets <- cut(resultant_df_xtd_df$Preds, xtd_df_breaks, labels = xtd_df_labels, include.lowest = TRUE, right = FALSE)

resultant_df_xtd_df %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_xtd_df)

names_xtd_df = colnames(dtrain_xtd_df)

importance_matrix_xtd_df <- xgb.importance(names_xtd_df, model = xgb_xtd_df)
importance_matrix_xtd_df

predictions <- predict(xgb_xtd_df, newdata = dtest_xtd_df, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)


rmse_value <- rmse(y_test_xtd_df, mean(y_test_xtd_df))
rmse_value # 1.42


rmse_value <- rmse(predictions, y_test_press_off)
rmse_value 



View(resultant_df_xtd_df %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


# 1.202



xgb_xtd_df <- xgboost(data = as.matrix(d_xpass_all), 
                      label = d_ypass_all, 
                      eta = eta_xtd_df,
                      max_depth = max_depth_xtd_df, 
                      alpha = alpha_xtd_df,
                      lambda = lambda_xtd_df,
                      min_child_weight = min_child_weight_xtd_df,
                      colsample_bynode = colsample_bynode_xtd_df,
                      colsample_bytree = colsample_bytree_xtd_df,
                      colsample_bylevel = colsample_bylevel_xtd_df,
                      nround = 700, # 700
                      objective = "reg:squarederror",
                      nthread = 2,
                      gamma = gamma_xtd_df,
                      early_stopping_rounds = 50
)

importance_matrix_xtd_df <- xgb.importance(names_xtd_df, model = xgb_xtd_df)
importance_matrix_xtd_df


combined_xtd_df$mixed_xtd <- ifelse(!is.na(combined_xtd_df$part_after_new_xtd), .2181 * combined_xtd_df$pbp_after_new_xtd + .2559 * combined_xtd_df$part_after_new_xtd + .1336 * combined_xtd_df$part_after_old_xtd + .1058 * combined_xtd_df$pbp_after_old_xtd + .0988 * combined_xtd_df$part_before_new_xtd + .0486 * combined_xtd_df$pbp_before_old_xtd + .0812 * combined_xtd_df$pbp_before_new_xtd + .0579 * combined_xtd_df$part_before_old_xtd, (.2182 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_after_new_xtd + (.1058 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_after_old_xtd + (.0812 / (.2182 + .1058 + .0548 + .0486)) * combined_xtd_df$pbp_before_old_xtd + (.0547 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_before_new_xtd) 

View(combined_xtd_df)

write.csv(combined_xtd_df %>% select(pbp_posteam, pbp_week, pbp_season, mixed_xtd), 'combined_xtd_df.csv')
