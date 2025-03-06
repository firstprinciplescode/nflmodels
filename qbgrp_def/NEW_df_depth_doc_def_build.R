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

options(scipen = 999)

setwd("C:/Users/vflre/Downloads/NFL Models")


df_depth <- read.xlsx("df_depth_all.xlsx")

df_depth_agg <- df_depth %>% group_by(team_name, Week, Season) %>%
  dplyr::summarise(behind_los_snaps = sum(behind_los_passing_snaps, na.rm = T),
                   short_snaps = sum(short_passing_snaps, na.rm = T),
                   behind_los_snaps = sum(behind_los_passing_snaps, na.rm = T),
                   medium_snaps = sum(medium_passing_snaps, na.rm = T),
                   deep_snaps = sum(deep_passing_snaps, na.rm = T),
                   short_snaps = sum(short_passing_snaps, na.rm = T),
                   behind_los_rate = sum(behind_los_passing_snaps, na.rm = T) / (sum(behind_los_passing_snaps, na.rm = T) + sum(short_passing_snaps, na.rm = T) + sum(medium_passing_snaps, na.rm = T) + sum(deep_passing_snaps, na.rm = T)),
                   short_rate = sum(short_passing_snaps, na.rm = T) / (sum(behind_los_passing_snaps, na.rm = T) + sum(short_passing_snaps, na.rm = T) + sum(medium_passing_snaps, na.rm = T) + sum(deep_passing_snaps, na.rm = T)),
                   medium_rate = sum(medium_passing_snaps, na.rm = T) / (sum(behind_los_passing_snaps, na.rm = T) + sum(short_passing_snaps, na.rm = T) + sum(medium_passing_snaps, na.rm = T) + sum(deep_passing_snaps, na.rm = T)),
                   deep_rate = sum(deep_passing_snaps, na.rm = T) / (sum(behind_los_passing_snaps, na.rm = T) + sum(short_passing_snaps, na.rm = T) + sum(medium_passing_snaps, na.rm = T) + sum(deep_passing_snaps, na.rm = T)),
                   behind_los_pressure_rate = sum(behind_los_def_gen_pressures, na.rm = T) / sum(behind_los_passing_snaps, na.rm = T),
                   short_pressure_rate = sum(short_def_gen_pressures, na.rm = T) / sum(short_passing_snaps, na.rm = T),
                   behind_los_grade = sum(behind_los_passing_snaps * behind_los_grades_pass, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_grade = sum(short_passing_snaps * short_grades_pass, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_time_to_throw = sum(behind_los_passing_snaps * behind_los_avg_time_to_throw, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_time_to_throw = sum(short_passing_snaps * short_avg_time_to_throw, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_ypa = sum(behind_los_passing_snaps * behind_los_ypa, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_ypa = sum(short_passing_snaps * short_ypa, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_acc_pct = sum(behind_los_passing_snaps * behind_los_accuracy_percent, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_acc_pct = sum(short_passing_snaps * short_accuracy_percent, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_qbr = sum(behind_los_passing_snaps * behind_los_qb_rating, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_qbr = sum(short_passing_snaps * short_qb_rating, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_btt_rate = sum(behind_los_passing_snaps * behind_los_btt_rate, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_btt_rate = sum(short_passing_snaps * short_btt_rate, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_twp_rate = sum(behind_los_passing_snaps * behind_los_twp_rate, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_twp_rate = sum(short_passing_snaps * short_twp_rate, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   behind_los_adot = sum(behind_los_passing_snaps * behind_los_avg_depth_of_target, na.rm = TRUE) / sum(behind_los_passing_snaps, na.rm = TRUE),
                   short_adot = sum(short_passing_snaps * short_avg_depth_of_target, na.rm = TRUE) / sum(short_passing_snaps, na.rm = TRUE),
                   medium_pressure_rate = sum(medium_def_gen_pressures, na.rm = T) / sum(medium_passing_snaps, na.rm = T),
                   deep_pressure_rate = sum(deep_def_gen_pressures, na.rm = T) / sum(deep_passing_snaps, na.rm = T),
                   medium_grade = sum(medium_passing_snaps * medium_grades_pass, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_grade = sum(deep_passing_snaps * deep_grades_pass, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_time_to_throw = sum(medium_passing_snaps * medium_avg_time_to_throw, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_time_to_throw = sum(deep_passing_snaps * deep_avg_time_to_throw, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_ypa = sum(medium_passing_snaps * medium_ypa, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_ypa = sum(deep_passing_snaps * deep_ypa, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_acc_pct = sum(medium_passing_snaps * medium_accuracy_percent, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_acc_pct = sum(deep_passing_snaps * deep_accuracy_percent, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_qbr = sum(medium_passing_snaps * medium_qb_rating, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_qbr = sum(deep_passing_snaps * deep_qb_rating, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_btt_rate = sum(medium_passing_snaps * medium_btt_rate, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_btt_rate = sum(deep_passing_snaps * deep_btt_rate, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_twp_rate = sum(medium_passing_snaps * medium_twp_rate, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_twp_rate = sum(deep_passing_snaps * deep_twp_rate, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE),
                   medium_adot = sum(medium_passing_snaps * medium_avg_depth_of_target, na.rm = TRUE) / sum(medium_passing_snaps, na.rm = TRUE),
                   deep_adot = sum(deep_passing_snaps * deep_avg_depth_of_target, na.rm = TRUE) / sum(deep_passing_snaps, na.rm = TRUE)
  )


#### JOIN IN ALLOWED PRESSURE SHIT, GO FROM THERE


df_depth_agg$team_name[which(df_depth_agg$team_name == "SD")] = "LAC"
df_depth_agg$team_name[which(df_depth_agg$team_name == "OAK")] = "LV"

xtd_df <- read.csv('combined_xtd_df.csv') %>% select(-X)

df_depth_agg <- left_join(df_depth_agg, xtd_df, by = c("team_name" = "pbp_posteam", "Week" = "pbp_week", "Season" = "pbp_season"))

qbgrp_df <- read.csv('nfl_weeks_good.csv') %>% select(-X)

defgrp_df <- read.csv('nfl_def_weeks_good.csv')


df_depth_agg <- left_join(df_depth_agg, qbgrp_df %>% select(team_name:season, qbgrp_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_depth_agg)[50] <- "OffPerformance"

df_depth_agg <- left_join(df_depth_agg, defgrp_df %>% select(team_name:season, def_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_depth_agg)[52] <- "DefPerformance"

# AGAIN NOTE - GOOD IS ALWAYS GOOD FOR OFFENSE.


df_depth_agg$ds_grade_difference = df_depth_agg$deep_grade - df_depth_agg$short_grade
df_depth_agg$ms_grade_difference = df_depth_agg$medium_grade - df_depth_agg$short_grade
df_depth_agg$ttt_difference = df_depth_agg$medium_time_to_throw - df_depth_agg$short_time_to_throw
df_depth_agg$ypa_difference = df_depth_agg$medium_ypa - df_depth_agg$short_ypa
df_depth_agg$ds_acc_pct_difference = df_depth_agg$deep_acc_pct - df_depth_agg$short_acc_pct
df_depth_agg$ms_acc_pct_difference = df_depth_agg$medium_acc_pct - df_depth_agg$short_acc_pct
df_depth_agg$ds_qbr_difference = df_depth_agg$deep_qbr - df_depth_agg$short_qbr
df_depth_agg$ms_qbr_difference = df_depth_agg$medium_qbr - df_depth_agg$short_qbr
df_depth_agg$ds_twp_difference = df_depth_agg$deep_twp_rate - df_depth_agg$short_twp_rate
df_depth_agg$ms_twp_difference = df_depth_agg$medium_twp_rate - df_depth_agg$short_twp_rate
df_depth_agg$pressure_rate_difference = df_depth_agg$deep_pressure_rate - df_depth_agg$medium_pressure_rate


# Create the new data frame with the filter
df_depth_agg_reg <- df_depth_agg %>%
  group_by(def_ssn) %>% filter(!is.na(def_ssn)) %>%
  filter(n() >= 4) %>%
  ungroup()


#####
#####
#####


df_depth_agg_reg  <- df_depth_agg_reg %>% arrange(def_ssn, team_name, Season, Week)

# xtd_depth_df <- data.frame()
for(i in 1:nrow(df_depth_agg_reg)){
  rel_ssn = df_depth_agg_reg[i,]$def_ssn
  df_press = df_depth_agg_reg[-i,]
  df_good <- df_press %>% as.data.frame() %>% filter(DefPerformance == "Good") %>% select(-c(team_name, Week, Season, short_snaps, medium_snaps, deep_snaps, behind_los_snaps, OffPerformance, DefPerformance, mixed_xtd, qbgrp_ssn)) %>% ungroup() %>% group_by(def_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T))
  df_bad <- df_press %>% as.data.frame() %>% filter(DefPerformance == "Bad") %>% select(-c(team_name, Week, Season, short_snaps, medium_snaps, deep_snaps, behind_los_snaps, OffPerformance, DefPerformance, mixed_xtd, qbgrp_ssn)) %>% ungroup() %>% group_by(def_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T))
  df_diff <- df_good[,-1] - df_bad[,-1]
  colnames(df_diff) <- paste0(colnames(df_diff), "_diff")
  qb_ssn <- df_good[,1]
  df_good2 <- df_good[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_bad2 <- df_bad[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_diff2 <- df_diff[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
  df_good2 <- cbind(qb_ssn, df_good2)
  df_bad2 <- cbind(qb_ssn, df_bad2)
  df_diff2 <- cbind(qb_ssn, df_diff2)
  colnames(df_good2) <- paste0(colnames(df_good2), "_Good")
  colnames(df_bad2) <- paste0(colnames(df_bad2), "_Bad")
  xtd_depth_df <- rbind(xtd_depth_df, cbind(df_good2 %>% filter(def_ssn_Good == rel_ssn), df_bad2 %>% filter(def_ssn_Bad == rel_ssn), df_diff2 %>% filter(def_ssn == rel_ssn)) %>% select(-c(def_ssn_Bad, def_ssn)))
  print(paste(i, rel_ssn))
}


#####
#####
#####


xtd_depth_df2 <- xtd_depth_df %>% select(-c(def_ssn_Good, behind_los_btt_rate_Good, behind_los_btt_rate_Bad, behind_los_btt_rate_diff))
# xtd_depth_df2 <- xtd_depth_df2 %>% select(-qbgrp_ssn_Good)

xtd_depth_df2$mixed_xtd <- df_depth_agg_reg$mixed_xtd

sample_split_press_off <- sample.split(Y = xtd_depth_df2$mixed_xtd, SplitRatio = 0.85)
train_set_press_off <- subset(x = xtd_depth_df2, sample_split_press_off == TRUE)
test_set_press_off <- subset(x = xtd_depth_df2, sample_split_press_off == FALSE)


X_train_press_off <- train_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
X_test_press_off <- test_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
y_train_press_off <- train_set_press_off$mixed_xtd
y_test_press_off <- test_set_press_off$mixed_xtd


dtrain_press_off = xgb.DMatrix(data = as.matrix(X_train_press_off), label = y_train_press_off)
dtest_press_off = xgb.DMatrix(data =as.matrix(X_test_press_off), label = y_test_press_off)

d_xpass_all = xtd_depth_df2 %>% select(-mixed_xtd)
d_ypass_all = xtd_depth_df2$mixed_xtd
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_press_off = list(train=dtrain_press_off, test=dtest_press_off)


eta_press_off = .017 # .017
gamma_press_off = 0 # 0
max_depth_press_off = 4 # 4
min_child_weight_press_off = 0 # 0
alpha_press_off = 0 # .01
lambda_press_off = 1 # 1
colsample_bynode_press_off = 0 # 1
colsample_bylevel_press_off = 0 # .725
colsample_bytree_press_off = 0 # .55

xgb_press_off_route <- xgboost(data = dtrain_press_off,
                               label = y_train_press_off,
                               eta = eta_press_off,
                               max_depth = max_depth_press_off,
                               alpha = alpha_press_off,
                               lambda = lambda_press_off,
                               min_child_weight = min_child_weight_press_off,
                               colsample_bynode = colsample_bynode_press_off,
                               colsample_bytree = colsample_bytree_press_off,
                               colsample_bylevel = colsample_bylevel_press_off,
                               nround = 295, # 295
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)


test_preds = predict(xgb_press_off_route, newdata = xgb.DMatrix(X_test_press_off %>% as.matrix()))

resultant_df_press_off = cbind(test_preds, y_test_press_off) %>% as.data.frame()
colnames(resultant_df_press_off) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(0, 4.25, by = 0.25)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_press_off$buckets <- cut(resultant_df_press_off$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_press_off = colnames(dtrain_press_off)

importance_matrix_press_off <- xgb.importance(names_press_off, model = xgb_press_off_route)
importance_matrix_press_off

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

rmse(y_test_press_off, mean(y_test_press_off)) # 1.029


rmse_value <- rmse(predictions, y_test_press_off)
rmse_value

# 1.02

plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))
summary(lm(y_test_press_off ~ predictions))

# 4


xgb_press_off_route <- xgboost(data = as.matrix(d_xpass_all),
                               label = d_ypass_all,
                               eta = eta_press_off,
                               max_depth = max_depth_press_off,
                               alpha = alpha_press_off,
                               lambda = lambda_press_off,
                               min_child_weight = min_child_weight_press_off,
                               colsample_bynode = colsample_bynode_press_off,
                               colsample_bytree = colsample_bytree_press_off,
                               colsample_bylevel = colsample_bylevel_press_off,
                               nround = 295, # 295
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))

importance_matrix_press_off <- xgb.importance(names_press_off, model = xgb_press_off_route)
importance_matrix_press_off


write.csv(importance_matrix_press_off, 'imp_depth_def.csv')


#####
#####
#####


df_depth_good <- df_depth_agg_reg %>% as.data.frame() %>% filter(DefPerformance == "Good") %>% select(-c(team_name, Week, Season, behind_los_snaps, short_snaps, medium_snaps, deep_snaps, OffPerformance, DefPerformance, mixed_xtd, qbgrp_ssn)) %>% ungroup() %>% group_by(def_ssn) %>% summarize(across(everything(), mean, na.rm = T))
df_depth_bad <- df_depth_agg_reg %>% as.data.frame() %>% filter(DefPerformance == "Bad") %>% select(-c(team_name, Week, Season, behind_los_snaps, short_snaps, medium_snaps, deep_snaps, OffPerformance, DefPerformance, mixed_xtd, qbgrp_ssn)) %>% ungroup() %>% group_by(def_ssn) %>% summarize(across(everything(), mean, na.rm = T))
df_depth_diff <- df_depth_good[,-1] - df_depth_bad[,-1]
colnames(df_depth_diff) <- paste0(colnames(df_depth_diff), "_diff")
df_depth_good2 <- df_depth_good[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_depth_bad2 <- df_depth_bad[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_depth_diff2 <- df_depth_diff %>% as.data.frame() %>% mutate(across(everything(), scale))
colnames(df_depth_good2) <- paste0(colnames(df_depth_good2), "_Good")
colnames(df_depth_bad2) <- paste0(colnames(df_depth_bad2), "_Bad")

df_depth_def_scaled_z <- cbind(df_depth_good$def_ssn, df_depth_good2, df_depth_bad2, df_depth_diff2)
colnames(df_depth_def_scaled_z)[1] <- "def_ssn"


# Function to compare defenses across seasons and teams
comparison_depth_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_off$Feature

  # Filter the relevant columns in df_depth_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_depth_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance

  all_others <- df_depth_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison

  # Preserve the team identifiers
  all_others_tms <- df_depth_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)

  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))

  # Convert Gain to numeric weights
  weights <- importance_matrix_press_off$Gain

  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")

  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)

  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)


  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }

  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)

  return(result_def)
}

list_dependencies_depth <- list(
  df_depth_def_scaled_z = df_depth_def_scaled_z,
  importance_matrix_depth_def <- importance_matrix_press_off
)

save(list_dependencies_depth, file = 'dependencies_depth_def.RData')

# Example usage
comparison_depth_def_func("MIN2024", .925)

saveRDS(comparison_depth_def_func, file = 'comparison_depth_def_func.rds')
