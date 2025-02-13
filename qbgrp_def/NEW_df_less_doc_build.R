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

setwd("C:/Users/AndLi/Downloads/NFL Models")


df_tip <- read.xlsx("df_time_in_pocket_all.xlsx")

df_less_agg <- df_tip %>% group_by(team_name, Week, Season) %>% 
  dplyr::summarise(less_snaps = sum(less_passing_snaps, na.rm = T), 
                   more_snaps = sum(more_passing_snaps, na.rm = T),
                   less_rate = sum(less_passing_snaps, na.rm = T) / (sum(less_passing_snaps, na.rm = T) + sum(more_passing_snaps, na.rm = T)),
                   less_pressure_rate = sum(less_def_gen_pressures, na.rm = T) / sum(less_passing_snaps, na.rm = T),
                   more_pressure_rate = sum(more_def_gen_pressures, na.rm = T) / sum(more_passing_snaps, na.rm = T),
                   less_grade = sum(less_passing_snaps * less_grades_pass, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_grade = sum(more_passing_snaps * more_grades_pass, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_time_to_throw = sum(less_passing_snaps * less_avg_time_to_throw, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_time_to_throw = sum(more_passing_snaps * more_avg_time_to_throw, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_ypa = sum(less_passing_snaps * less_ypa, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_ypa = sum(more_passing_snaps * more_ypa, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_acc_pct = sum(less_passing_snaps * less_accuracy_percent, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_acc_pct = sum(more_passing_snaps * more_accuracy_percent, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_qbr = sum(less_passing_snaps * less_qb_rating, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_qbr = sum(more_passing_snaps * more_qb_rating, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_btt_rate = sum(less_passing_snaps * less_btt_rate, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_btt_rate = sum(more_passing_snaps * more_btt_rate, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),                  
                   less_twp_rate = sum(less_passing_snaps * less_twp_rate, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_twp_rate = sum(more_passing_snaps * more_twp_rate, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_adot = sum(less_passing_snaps * less_avg_depth_of_target, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_adot = sum(more_passing_snaps * more_avg_depth_of_target, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),
                   less_scr_rate = sum(less_scrambles, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE),
                   more_scr_rate = sum(more_scrambles, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE),                   
                   less_sack_pct = sum(less_sacks, na.rm = T) / sum(less_passing_snaps, na.rm = T),
                   more_sack_pct = sum(more_sacks, na.rm = T) / sum(more_passing_snaps, na.rm = T))


#### JOIN IN ALLOWED PRESSURE SHIT, GO FROM THERE


df_less_agg$team_name[which(df_less_agg$team_name == "SD")] = "LAC"
df_less_agg$team_name[which(df_less_agg$team_name == "OAK")] = "LV"

xtd_df <- read.csv('combined_xtd_df.csv') %>% select(-X)

df_less_agg <- left_join(df_less_agg, xtd_df, by = c("team_name" = "pbp_posteam", "Week" = "pbp_week", "Season" = "pbp_season"))

qbgrp_df <- read.csv('nfl_weeks_good.csv') %>% select(-X)

defgrp_df <- read.csv('nfl_def_weeks_good.csv')


df_less_agg <- left_join(df_less_agg, qbgrp_df %>% select(team_name:season, qbgrp_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_less_agg)[31] <- "OffPerformance"

df_less_agg <- left_join(df_less_agg, defgrp_df %>% select(team_name:season, def_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_less_agg)[33] <- "DefPerformance"

# AGAIN NOTE - GOOD IS ALWAYS GOOD FOR OFFENSE. 


df_less_agg$grade_difference = df_less_agg$less_grade - df_less_agg$more_grade
df_less_agg$ttt_difference = df_less_agg$less_time_to_throw - df_less_agg$more_time_to_throw
df_less_agg$ypa_difference = df_less_agg$less_ypa - df_less_agg$more_ypa
df_less_agg$acc_pct_difference = df_less_agg$less_acc_pct - df_less_agg$more_acc_pct
df_less_agg$qbr_difference = df_less_agg$less_qbr - df_less_agg$more_qbr
df_less_agg$btt_difference = df_less_agg$less_btt_rate - df_less_agg$more_btt_rate
df_less_agg$twp_difference = df_less_agg$less_twp_rate - df_less_agg$more_twp_rate
df_less_agg$adot_difference = df_less_agg$less_adot - df_less_agg$more_adot
df_less_agg$scr_rate_difference = df_less_agg$less_scr_rate - df_less_agg$more_scr_rate
df_less_agg$pressure_rate_difference = df_less_agg$less_pressure_rate - df_less_agg$more_pressure_rate
df_less_agg$sack_rate_difference = df_less_agg$less_sack_pct - df_less_agg$more_sack_pct


# Create the new data frame with the filter
df_less_agg_reg <- df_less_agg %>%
  group_by(qbgrp_ssn) %>%
  filter(n() >= 4) %>%
  ungroup()


#####
#####
#####


df_less_agg_reg  <- df_less_agg_reg %>% arrange(qbgrp_ssn, team_name, Season, Week)

xtd_less_df <- data.frame()
for(i in 1:nrow(df_less_agg_reg)){
  rel_ssn = df_less_agg_reg[i,]$qbgrp_ssn
  df_press = df_less_agg_reg[-i,]
  df_good <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, more_snaps, less_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
  df_bad <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, more_snaps, less_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
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
  xtd_less_df <- rbind(xtd_less_df, cbind(df_good2 %>% filter(qbgrp_ssn_Good == rel_ssn), df_bad2 %>% filter(qbgrp_ssn_Bad == rel_ssn), df_diff2 %>% filter(qbgrp_ssn == rel_ssn)) %>% select(-c(qbgrp_ssn_Bad, qbgrp_ssn)))
  print(paste(i, rel_ssn))
}

#xtd_less_df <- xtd_less_df[1:4145,]


#####
#####
#####


xtd_less_df2 <- xtd_less_df %>% select(-c(qbgrp_ssn_Good))
# xtd_less_df2 <- xtd_less_df2 %>% select(-qbgrp_ssn_Good)

xtd_less_df2$mixed_xtd <- df_less_agg_reg$mixed_xtd

sample_split_press_off <- sample.split(Y = xtd_less_df2$mixed_xtd, SplitRatio = 0.85)
train_set_press_off <- subset(x = xtd_less_df2, sample_split_press_off == TRUE)
test_set_press_off <- subset(x = xtd_less_df2, sample_split_press_off == FALSE)


X_train_press_off <- train_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
X_test_press_off <- test_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
y_train_press_off <- train_set_press_off$mixed_xtd
y_test_press_off <- test_set_press_off$mixed_xtd


dtrain_press_off = xgb.DMatrix(data = as.matrix(X_train_press_off), label = y_train_press_off)
dtest_press_off = xgb.DMatrix(data =as.matrix(X_test_press_off), label = y_test_press_off)

d_xpass_all = xtd_less_df2 %>% select(-mixed_xtd)
d_ypass_all = xtd_less_df2$mixed_xtd
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_press_off = list(train=dtrain_press_off, test=dtest_press_off)


eta_press_off = .037 # .037
gamma_press_off = 12.5 # 12.5
max_depth_press_off = 5 # 5
min_child_weight_press_off = 0 # 0
alpha_press_off = 1 # 1
lambda_press_off = .35 # .35
colsample_bynode_press_off = .3 # .3
colsample_bylevel_press_off = .8 # .8
colsample_bytree_press_off = 1 # 1



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
                               nround = 525, # 525
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

rmse(y_test_press_off, mean(y_test_press_off)) # 1.017

rmse_value <- rmse(predictions, y_test_press_off)
rmse_value 

# .986

plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))
summary(lm(y_test_press_off ~ predictions))

# 6.69


xgb_press_off_route <- xgboost(data = as.matrix(d_xpass_all), 
                               label = d_ypass_all, 
                               eta = eta_press_off,
                               max_less = max_depth_press_off, 
                               alpha = alpha_press_off,
                               lambda = lambda_press_off,
                               min_child_weight = min_child_weight_press_off,
                               colsample_bynode = colsample_bynode_press_off,
                               colsample_bytree = colsample_bytree_press_off,
                               colsample_bylevel = colsample_bylevel_press_off,
                               nround = 525, # 525
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))


write.csv(importance_matrix_press_off, 'imp_less_off.csv')


#####
#####
#####


df_less_good <- df_less_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, more_snaps, less_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
df_less_bad <- df_less_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, more_snaps, less_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
df_less_diff <- df_less_good[,-1] - df_less_bad[,-1]
colnames(df_less_diff) <- paste0(colnames(df_less_diff), "_diff")
df_less_good2 <- df_less_good[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_less_bad2 <- df_less_bad[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_less_diff2 <- df_less_diff %>% as.data.frame() %>% mutate(across(everything(), scale))
colnames(df_less_good2) <- paste0(colnames(df_less_good2), "_Good")
colnames(df_less_bad2) <- paste0(colnames(df_less_bad2), "_Bad")

df_less_scaled_z <- cbind(df_less_good$qbgrp_ssn, df_less_good2, df_less_bad2, df_less_diff2)
colnames(df_less_scaled_z)[1] <- "qbgrp_ssn"


# Function to compare defenses across seasons and teams
comparison_less_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_off$Feature
  
  # Filter the relevant columns in df_less_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_less_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_less_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_less_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
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

list_dependencies_less <- list(
  importance_matrix_less_off <- importance_matrix_press_off,
  df_less_scaled_z <- df_less_scaled_z
)

save(list_dependencies_less, file = 'list_dependencies_less.RData')

# Example usage
comparison_less_func("KCMahomes-2024", .675)

saveRDS(comparison_less_func, file = "comparison_less_func.rds")
