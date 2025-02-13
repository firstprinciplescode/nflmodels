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


df_press <- read.xlsx("df_press_all.xlsx")

df_all_press <- read.xlsx("df_allpress_all.xlsx")


df_press_agg <- df_press %>% group_by(team_name, Week, Season) %>% 
  dplyr::summarise(pressure_snaps = sum(pressure_passing_snaps, na.rm = T), 
                   no_pressure_snaps = sum(no_pressure_passing_snaps, na.rm = T),
                   pressure_rate = sum(pressure_passing_snaps, na.rm = T) / (sum(pressure_passing_snaps, na.rm = T) + sum(no_pressure_passing_snaps, na.rm = T)),
                   pressure_grade = sum(pressure_passing_snaps * pressure_grades_pass, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_grade = sum(no_pressure_passing_snaps * no_pressure_grades_pass, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_time_to_throw = sum(pressure_passing_snaps * pressure_avg_time_to_throw, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_time_to_throw = sum(no_pressure_passing_snaps * no_pressure_avg_time_to_throw, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_ypa = sum(pressure_passing_snaps * pressure_ypa, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_ypa = sum(no_pressure_passing_snaps * no_pressure_ypa, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_acc_pct = sum(pressure_passing_snaps * pressure_accuracy_percent, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_acc_pct = sum(no_pressure_passing_snaps * no_pressure_accuracy_percent, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_qbr = sum(pressure_passing_snaps * pressure_qb_rating, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_qbr = sum(no_pressure_passing_snaps * no_pressure_qb_rating, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_btt_rate = sum(pressure_passing_snaps * pressure_btt_rate, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_btt_rate = sum(no_pressure_passing_snaps * no_pressure_btt_rate, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),                  
                   pressure_twp_rate = sum(pressure_passing_snaps * pressure_twp_rate, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_twp_rate = sum(no_pressure_passing_snaps * no_pressure_twp_rate, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_adot = sum(pressure_passing_snaps * pressure_avg_depth_of_target, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_adot = sum(no_pressure_passing_snaps * no_pressure_avg_depth_of_target, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),
                   pressure_scr_rate = sum(pressure_scrambles, na.rm = TRUE) / sum(pressure_passing_snaps, na.rm = TRUE),
                   no_pressure_scr_rate = sum(no_pressure_scrambles, na.rm = TRUE) / sum(no_pressure_passing_snaps, na.rm = TRUE),                   
                   sack_pct = sum(pressure_sacks, na.rm = T) / (sum(pressure_passing_snaps, na.rm = T) + sum(no_pressure_passing_snaps, na.rm = T)))


#### JOIN IN ALLOWED PRESSURE SHIT, GO FROM THERE

df_all_press_agg <- df_all_press %>% group_by(team_name, Week, Season) %>% 
                      dplyr::summarise(pressures_total = sum(pressures_off),
                                       pressures_qb = sum(pressures_self),
                                       pressures_oth = pressures_total - pressures_qb)


combined_press_agg <- left_join(df_press_agg, df_all_press_agg %>% select(-pressures_total), by = c("team_name" = "team_name", "Week" = "Week", "Season" = "Season"))

combined_press_agg$team_name[which(combined_press_agg$team_name == "SD")] = "LAC"
combined_press_agg$team_name[which(combined_press_agg$team_name == "OAK")] = "LV"

xtd_df <- read.csv('combined_xtd_df.csv') %>% select(-X)

combined_press_agg <- left_join(combined_press_agg, xtd_df, by = c("team_name" = "pbp_posteam", "Week" = "pbp_week", "Season" = "pbp_season"))

qbgrp_df <- read.csv('nfl_weeks_good.csv') %>% select(-X)

defgrp_df <- read.csv('nfl_def_weeks_good.csv')


combined_press_agg <- left_join(combined_press_agg, qbgrp_df %>% select(team_name:season, qbgrp_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(combined_press_agg)[30] <- "OffPerformance"

combined_press_agg <- left_join(combined_press_agg, defgrp_df %>% select(team_name:season, def_ssn, performance), by = c("team_name" = "team_name", "Week" = "week", "Season" = "season"))
colnames(combined_press_agg)[32] <- "DefPerformance"

# AGAIN NOTE - GOOD IS ALWAYS GOOD FOR OFFENSE. 


combined_press_agg$grade_difference = combined_press_agg$pressure_grade - combined_press_agg$no_pressure_grade
combined_press_agg$ttt_difference = combined_press_agg$pressure_time_to_throw - combined_press_agg$no_pressure_time_to_throw
combined_press_agg$ypa_difference = combined_press_agg$pressure_ypa - combined_press_agg$no_pressure_ypa
combined_press_agg$acc_pct_difference = combined_press_agg$pressure_acc_pct - combined_press_agg$no_pressure_acc_pct
combined_press_agg$qbr_difference = combined_press_agg$pressure_qbr - combined_press_agg$no_pressure_qbr
combined_press_agg$btt_difference = combined_press_agg$pressure_btt_rate - combined_press_agg$no_pressure_btt_rate
combined_press_agg$twp_difference = combined_press_agg$pressure_twp_rate - combined_press_agg$no_pressure_twp_rate
combined_press_agg$adot_difference = combined_press_agg$pressure_adot - combined_press_agg$no_pressure_adot
combined_press_agg$scr_rate_difference = combined_press_agg$pressure_scr_rate - combined_press_agg$no_pressure_scr_rate



# Create the new data frame with the filter
combined_press_agg_reg <- combined_press_agg %>%
  group_by(qbgrp_ssn) %>%
  filter(n() >= 4) %>%
  ungroup()


#####
#####
#####

combined_press_agg_reg <- combined_press_agg_reg %>% arrange(qbgrp_ssn, team_name, Season, Week)

#xtd_pressure_df <- data.frame()
for(i in 1:nrow(combined_press_agg_reg)){
  rel_ssn = combined_press_agg_reg[i,]$qbgrp_ssn
  df_press = combined_press_agg_reg[-i,]
  df_good <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, no_pressure_snaps, pressure_snaps, pressures_qb, pressures_oth, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
  df_bad <- df_press %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, no_pressure_snaps, pressure_snaps, pressures_qb, pressures_oth, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
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
  xtd_pressure_df <- rbind(xtd_pressure_df, cbind(df_good2 %>% filter(qbgrp_ssn_Good == rel_ssn), df_bad2 %>% filter(qbgrp_ssn_Bad == rel_ssn), df_diff2 %>% filter(qbgrp_ssn == rel_ssn)) %>% select(-c(qbgrp_ssn_Bad, qbgrp_ssn)))
  print(paste(i, rel_ssn))
}


#####
#####
#####


xtd_pressure_df <- xtd_pressure_df %>% select(-c(OffPerformance_Bad, OffPerformance_diff, OffPerformance_Good, DefPerformance_Good, DefPerformance_Bad, DefPerformance_diff))
xtd_pressure_df <- xtd_pressure_df %>% select(-qbgrp_ssn_Good)

xtd_pressure_df$mixed_xtd <- combined_press_agg_reg$mixed_xtd

sample_split_press_off <- sample.split(Y = xtd_pressure_df$mixed_xtd, SplitRatio = 0.8)
train_set_press_off <- subset(x = xtd_pressure_df, sample_split_press_off == TRUE)
test_set_press_off <- subset(x = xtd_pressure_df, sample_split_press_off == FALSE)


X_train_press_off <- train_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
X_test_press_off <- test_set_press_off %>% select(-mixed_xtd) %>% as.data.frame()
y_train_press_off <- train_set_press_off$mixed_xtd
y_test_press_off <- test_set_press_off$mixed_xtd


dtrain_press_off = xgb.DMatrix(data = as.matrix(X_train_press_off), label = y_train_press_off)
dtest_press_off = xgb.DMatrix(data =as.matrix(X_test_press_off), label = y_test_press_off)

d_xpass_all = xtd_pressure_df %>% select(-mixed_xtd)
d_ypass_all = xtd_pressure_df$mixed_xtd
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_press_off = list(train=dtrain_press_off, test=dtest_press_off)


eta_press_off = .008 # .008
gamma_press_off = 6.5 # 6.5
max_depth_press_off = 5 # 5.75
min_child_weight_press_off = 3.25 # 3.25
alpha_press_off = .55 # .55
lambda_press_off = .45 # .45
colsample_bynode_press_off = .45 # .425, .475
colsample_bylevel_press_off = .425 # .425
colsample_bytree_press_off = .425 # .425

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
                                   nround = 535, # 535
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

rmse(y_test_press_off, mean(y_test_press_off)) # 1.006
 
rmse_value <- rmse(predictions, y_test_press_off)
rmse_value 

# .987


plot(y_test_press_off, predictions)
abline(lm(predictions ~ y_test_press_off))
summary(lm(predictions ~ y_test_press_off))

# 8.948


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
                               nround = 535, # 535
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

plot(y_test_press_off, predictions)
abline(lm(predictions ~ y_test_press_off))


write.csv(importance_matrix_press_off, 'imp_press_off.csv')


#####
#####
#####


df_pressure_good <- combined_press_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Good") %>% select(-c(team_name, Week, Season, no_pressure_snaps, pressure_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
df_pressure_bad <- combined_press_agg_reg %>% as.data.frame() %>% filter(OffPerformance == "Bad") %>% select(-c(team_name, Week, Season, no_pressure_snaps, pressure_snaps, OffPerformance, DefPerformance, mixed_xtd, def_ssn)) %>% ungroup() %>% group_by(qbgrp_ssn) %>% summarize(across(everything(), mean, na.rm = T)) 
df_pressure_diff <- df_pressure_good[,-1] - df_pressure_bad[,-1]
colnames(df_pressure_diff) <- paste0(colnames(df_pressure_diff), "_diff")
df_pressure_good2 <- df_pressure_good[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_pressure_bad2 <- df_pressure_bad[,-1] %>% as.data.frame() %>% mutate(across(everything(), scale))
df_pressure_diff2 <- df_pressure_diff %>% as.data.frame() %>% mutate(across(everything(), scale))
colnames(df_pressure_good2) <- paste0(colnames(df_pressure_good2), "_Good")
colnames(df_pressure_bad2) <- paste0(colnames(df_pressure_bad2), "_Bad")

df_pressure_scaled_z <- cbind(df_pressure_good$qbgrp_ssn, df_pressure_good2, df_pressure_bad2, df_pressure_diff2)
colnames(df_pressure_scaled_z)[1] <- "qbgrp_ssn"


# Function to compare defenses across seasons and teams
comparison_pressure_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_off$Feature
  
  # Filter the relevant columns in df_pressure_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_pressure_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
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
comparison_pressure_func("PHIHurts-2024", .9)


dependencies_pressure <- list(
  importance_matrix_press_off = importance_matrix_press_off,
  df_pressure_scaled_z = df_pressure_scaled_z
)

save(dependencies_pressure, file = "dependencies_pressure.RData")

saveRDS(comparison_pressure_func, file = "comparison_pressure_func.rds")
  