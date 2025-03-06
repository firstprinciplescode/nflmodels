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


df_press <- read.xlsx("df_press_all.xlsx")


df_blitz_agg <- df_press %>% group_by(team_name, Week, Season) %>% 
  dplyr::summarise(blitz_snaps = sum(blitz_passing_snaps, na.rm = T), 
                   no_blitz_snaps = sum(no_blitz_passing_snaps, na.rm = T),
                   blitz_rate = sum(blitz_passing_snaps, na.rm = T) / (sum(blitz_passing_snaps, na.rm = T) + sum(no_blitz_passing_snaps, na.rm = T)),
                   blitz_pressure_rate = sum(blitz_def_gen_pressures, na.rm = T) / sum(blitz_passing_snaps, na.rm = T),
                   no_blitz_pressure_rate = sum(no_blitz_def_gen_pressures, na.rm = T) / sum(no_blitz_passing_snaps, na.rm = T),
                   blitz_grade = sum(blitz_passing_snaps * blitz_grades_pass, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_grade = sum(no_blitz_passing_snaps * no_blitz_grades_pass, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_time_to_throw = sum(blitz_passing_snaps * blitz_avg_time_to_throw, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_time_to_throw = sum(no_blitz_passing_snaps * no_blitz_avg_time_to_throw, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_ypa = sum(blitz_passing_snaps * blitz_ypa, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_ypa = sum(no_blitz_passing_snaps * no_blitz_ypa, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_acc_pct = sum(blitz_passing_snaps * blitz_accuracy_percent, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_acc_pct = sum(no_blitz_passing_snaps * no_blitz_accuracy_percent, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_qbr = sum(blitz_passing_snaps * blitz_qb_rating, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_qbr = sum(no_blitz_passing_snaps * no_blitz_qb_rating, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_btt_rate = sum(blitz_passing_snaps * blitz_btt_rate, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_btt_rate = sum(no_blitz_passing_snaps * no_blitz_btt_rate, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),                  
                   blitz_twp_rate = sum(blitz_passing_snaps * blitz_twp_rate, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_twp_rate = sum(no_blitz_passing_snaps * no_blitz_twp_rate, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_adot = sum(blitz_passing_snaps * blitz_avg_depth_of_target, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_adot = sum(no_blitz_passing_snaps * no_blitz_avg_depth_of_target, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),
                   blitz_scr_rate = sum(blitz_scrambles, na.rm = TRUE) / sum(blitz_passing_snaps, na.rm = TRUE),
                   no_blitz_scr_rate = sum(no_blitz_scrambles, na.rm = TRUE) / sum(no_blitz_passing_snaps, na.rm = TRUE),                   
                   blitz_sack_pct = sum(blitz_sacks, na.rm = T) / sum(blitz_passing_snaps, na.rm = T),
                   no_blitz_sack_pct = sum(no_blitz_sacks, na.rm = T) / sum(no_blitz_passing_snaps, na.rm = T))


#### JOIN IN ALLOWED PRESSURE SHIT, GO FROM THERE


df_blitz_agg$team_name[which(df_blitz_agg$team_name == "SD")] = "LAC"
df_blitz_agg$team_name[which(df_blitz_agg$team_name == "OAK")] = "LV"

xtd_df <- read.csv('combined_xtd_df.csv') %>% select(-X)

df_blitz_agg <- left_join(df_blitz_agg, xtd_df, by = c("team_name" = "pbp_posteam", "Week" = "pbp_week", "Season" = "pbp_season"))

qbgrp_df <- read.csv('nfl_weeks_good.csv') %>% select(-X)

defgrp_df <- read.csv('nfl_def_weeks_good.csv')


df_blitz_agg <- left_join(df_blitz_agg, qbgrp_df %>% select(team_name:season, qbgrp_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_blitz_agg)[31] <- "OffPerformance"

df_blitz_agg <- left_join(df_blitz_agg, defgrp_df %>% select(team_name:season, def_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(df_blitz_agg)[33] <- "DefPerformance"

# AGAIN NOTE - GOOD IS ALWAYS GOOD FOR OFFENSE. 


df_blitz_agg$grade_difference = df_blitz_agg$blitz_grade - df_blitz_agg$no_blitz_grade
df_blitz_agg$ttt_difference = df_blitz_agg$blitz_time_to_throw - df_blitz_agg$no_blitz_time_to_throw
df_blitz_agg$ypa_difference = df_blitz_agg$blitz_ypa - df_blitz_agg$no_blitz_ypa
df_blitz_agg$acc_pct_difference = df_blitz_agg$blitz_acc_pct - df_blitz_agg$no_blitz_acc_pct
df_blitz_agg$qbr_difference = df_blitz_agg$blitz_qbr - df_blitz_agg$no_blitz_qbr
df_blitz_agg$btt_difference = df_blitz_agg$blitz_btt_rate - df_blitz_agg$no_blitz_btt_rate
df_blitz_agg$twp_difference = df_blitz_agg$blitz_twp_rate - df_blitz_agg$no_blitz_twp_rate
df_blitz_agg$adot_difference = df_blitz_agg$blitz_adot - df_blitz_agg$no_blitz_adot
df_blitz_agg$scr_rate_difference = df_blitz_agg$blitz_scr_rate - df_blitz_agg$no_blitz_scr_rate
df_blitz_agg$pressure_rate_difference = df_blitz_agg$blitz_pressure_rate - df_blitz_agg$no_blitz_pressure_rate
df_blitz_agg$sack_rate_difference = df_blitz_agg$blitz_sack_pct - df_blitz_agg$no_blitz_sack_pct


# Create the new data frame with the filter
df_blitz_agg_reg <- df_blitz_agg %>%
  group_by(qbgrp_ssn) %>%
  filter(n() >= 4) %>%
  ungroup()


#####
#####
#####


df_blitz_agg_reg  <- df_blitz_agg_reg %>% arrange(qbgrp_ssn, team_name, Season, Week)

# xtd_blitz_df <- data.frame()
for(i in 1:nrow(df_blitz_agg_reg)){
  rel_ssn = df_blitz_agg_reg[i,]$qbgrp_ssn
  df_press = df_blitz_agg_reg[-i,]
  df_good <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, no_blitz_snaps, blitz_snaps, OffPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T)) 
  df_bad <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, no_blitz_snaps, blitz_snaps, OffPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T)) 
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
  xtd_blitz_df <- rbind(xtd_blitz_df, cbind(df_good2 %>% filter(qbgrp_ssn_Good == rel_ssn), df_bad2 %>% filter(qbgrp_ssn_Bad == rel_ssn), df_diff2 %>% filter(qbgrp_ssn == rel_ssn)) %>% select(-c(qbgrp_ssn_Bad, qbgrp_ssn)))
  print(paste(i, rel_ssn))
}

#xtd_blitz_df <- xtd_blitz_df[1:4145,]


#####
#####
#####


xtd_blitz_df2 <- xtd_blitz_df %>% select(-c(qbgrp_ssn_Good, DefPerformance_Good, DefPerformance_diff, DefPerformance_Bad))
xtd_blitz_df2 <- xtd_blitz_df2 %>% scale() %>% as.data.frame() 
# xtd_blitz_df2 <- xtd_blitz_df2 %>% select(-qbgrp_ssn_Good)

xtd_blitz_df2$mixed_xtd <- df_blitz_agg_reg$mixed_xtd

sample_split_press_off <- sample.split(Y = xtd_blitz_df2$mixed_xtd, SplitRatio = 0.85)
train_set_press_off <- subset(x = xtd_blitz_df2, sample_split_press_off == TRUE)
test_set_press_off <- subset(x = xtd_blitz_df2, sample_split_press_off == FALSE)


X_train_press_off <- train_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
X_test_press_off <- test_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
y_train_press_off <- train_set_press_off$mixed_xtd
y_test_press_off <- test_set_press_off$mixed_xtd


dtrain_press_off = xgb.DMatrix(data = as.matrix(X_train_press_off), label = y_train_press_off)
dtest_press_off = xgb.DMatrix(data =as.matrix(X_test_press_off), label = y_test_press_off)

d_xpass_all = xtd_blitz_df2 %>% select(-mixed_xtd)
d_ypass_all = xtd_blitz_df2$mixed_xtd
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_press_off = list(train=dtrain_press_off, test=dtest_press_off)


eta_press_off = .075 # .075
gamma_press_off = 5 # 5
max_depth_press_off = 5 # 5
min_child_weight_press_off = 2.5 # 2.5
alpha_press_off = .95 # .95
lambda_press_off = .2 # .2
colsample_bynode_press_off = 0 # 0
colsample_bylevel_press_off = .1 # .1
colsample_bytree_press_off = .4 # .4

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
                               nround = 620, # 620
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

rmse(y_test_press_off, mean(y_test_press_off)) # .98

rmse_value <- rmse(predictions, y_test_press_off)
rmse_value 

# .95


plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))
summary(lm(y_test_press_off ~ predictions))

# 6.8


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
                               nround = 620, # 620
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

plot(y_test_press_off, predictions)
abline(lm(predictions ~ y_test_press_off))

importance_matrix_press_off <- xgb.importance(names_press_off, model = xgb_press_off_route)
importance_matrix_press_off


write.csv(importance_matrix_press_off, 'imp_blitz_off.csv')
 

#####
#####
#####


df_blitz_good <- df_blitz_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, no_blitz_snaps, blitz_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T)) 
df_blitz_bad <- df_blitz_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, no_blitz_snaps, blitz_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% dplyr::summarize(across(everything(), mean, na.rm = T)) 
df_blitz_diff <- df_blitz_good[,-1] - df_blitz_bad[,-1]
colnames(df_blitz_diff) <- paste0(colnames(df_blitz_diff), "_diff")
df_blitz_good2 <- df_blitz_good[,-1] %>% scale() %>% as.data.frame()
df_blitz_bad2 <- df_blitz_bad[,-1] %>% scale() %>% as.data.frame()
df_blitz_diff2 <- df_blitz_diff %>% scale() %>% as.data.frame()
colnames(df_blitz_good2) <- paste0(colnames(df_blitz_good2), "_Good")
colnames(df_blitz_bad2) <- paste0(colnames(df_blitz_bad2), "_Bad")

df_blitz_scaled_z <- cbind(df_blitz_good$qbgrp_ssn, df_blitz_good2, df_blitz_bad2, df_blitz_diff2)
colnames(df_blitz_scaled_z)[1] <- "qbgrp_ssn"


# Function to compare defenses across seasons and teams
comparison_blitz_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_off$Feature
  
  # Filter the relevant columns in df_blitz_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_blitz_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_blitz_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_blitz_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
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

# Example usage
comparison_blitz_func("KCMahomes-2024", .675)


dependencies_blitz <- list(
  importance_matrix_blitz_off = importance_matrix_press_off,
  df_blitz_scaled_z = df_blitz_scaled_z
)

save(dependencies_blitz, file = 'dependencies_blitz.RData')

saveRDS(comparison_blitz_func, file = 'comparison_blitz_func.rds')
