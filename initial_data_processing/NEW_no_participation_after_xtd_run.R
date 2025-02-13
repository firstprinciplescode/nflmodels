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

setwd("C:/Users/vflre/Downloads/NFL Models")

pbp_nfl <- load_pbp(c(2016:2024))

'%ni%' <- Negate('%in%')

colnames(pbp_nfl)

# LET'S KEEP THINGS SEPARATE UNTIL THE END TO AVOID CROSS-CONTAMINATION OF INFO

pbp_nfl <- pbp_nfl %>% mutate(mod_ydstogo = ifelse(goal_to_go == 1, yardline_100, ydstogo))
pbp_nfl <- pbp_nfl %>% mutate(lead_timeout = lag(timeout_team, 1), last_timeout_ind = case_when(lead_timeout == posteam ~ 1, is.na(lead_timeout) ~ 0, (lead_timeout != posteam & !is.na(lead_timeout)) ~ -1))
pbp_nfl$td_side = ifelse((!is.na(pbp_nfl$td_team) & pbp_nfl$posteam == pbp_nfl$td_team), 1, 0)
pbp_nfl <- pbp_nfl %>% mutate(posteam_ind = ifelse(home_team == posteam, 1, 0))

pbp_nfl$week <- ifelse(pbp_nfl$week == 18 & pbp_nfl$season <= 2020, 28, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 19 & pbp_nfl$season <= 2020, 29, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 19 & pbp_nfl$season > 2020, 28, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 20 & pbp_nfl$season <= 2020, 30, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 20 & pbp_nfl$season > 2020, 29, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 21 & pbp_nfl$season <= 2020, 32, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 21 & pbp_nfl$season > 2020, 30, pbp_nfl$week)
pbp_nfl$week <- ifelse(pbp_nfl$week == 22, 32, pbp_nfl$week)

pbp_nfl$posteam[which(pbp_nfl$posteam == "ARI")] = "ARZ"
pbp_nfl$posteam[which(pbp_nfl$posteam == "BAL")] = "BLT"
pbp_nfl$posteam[which(pbp_nfl$posteam == "CLE")] = "CLV"
pbp_nfl$posteam[which(pbp_nfl$posteam == "HOU")] = "HST"
pbp_nfl$posteam[which(pbp_nfl$posteam == "SD")] = "LAC"
pbp_nfl$posteam[which(pbp_nfl$posteam == "OAK")] = "LV"

pbp_nfl$defteam[which(pbp_nfl$defteam == "ARI")] = "ARZ"
pbp_nfl$defteam[which(pbp_nfl$defteam == "BAL")] = "BLT"
pbp_nfl$defteam[which(pbp_nfl$defteam == "CLE")] = "CLV"
pbp_nfl$defteam[which(pbp_nfl$defteam == "HOU")] = "HST"
pbp_nfl$defteam[which(pbp_nfl$defteam == "SD")] = "LAC"
pbp_nfl$defteam[which(pbp_nfl$defteam == "OAK")] = "LV"

pbp_nfl$td_team[which(pbp_nfl$td_team == "ARI")] = "ARZ"
pbp_nfl$td_team[which(pbp_nfl$td_team == "BAL")] = "BLT"
pbp_nfl$td_team[which(pbp_nfl$td_team == "CLE")] = "CLV"
pbp_nfl$td_team[which(pbp_nfl$td_team == "HOU")] = "HST"
pbp_nfl$td_team[which(pbp_nfl$td_team == "SD")] = "LAC"
pbp_nfl$td_team[which(pbp_nfl$td_team == "OAK")] = "LV"

pbp_nfl$posteam[which(pbp_nfl$posteam == "ARI")] = "ARZ"
pbp_nfl$posteam[which(pbp_nfl$posteam == "BAL")] = "BLT"
pbp_nfl$posteam[which(pbp_nfl$posteam == "CLE")] = "CLV"
pbp_nfl$posteam[which(pbp_nfl$posteam == "HOU")] = "HST"
pbp_nfl$posteam[which(pbp_nfl$posteam == "SD")] = "LAC"
pbp_nfl$posteam[which(pbp_nfl$posteam == "OAK")] = "LV"

pbp_nfl$air_yards[which(pbp_nfl$air_yards <= -54)] = 0

pbp_nfl$down_one_ind = ifelse(pbp_nfl$down == 1, 1, 0)
pbp_nfl$down_two_ind = ifelse(pbp_nfl$down == 2, 1, 0)
pbp_nfl$down_three_ind = ifelse(pbp_nfl$down == 3, 1, 0)
pbp_nfl$down_four_ind = ifelse(pbp_nfl$down == 4, 1, 0)

pbp_nfl$distance_to_sticks = pbp_nfl$mod_ydstogo - pbp_nfl$air_yards

pbp_nfl$season_type <- ifelse(pbp_nfl$season_type == "REG", 0, 1)
pbp_nfl$roof <- ifelse(pbp_nfl$roof %in% c('dome', 'closed'), 1, 0)
pbp_nfl$surface <- ifelse(pbp_nfl$surface == "grass", 0, 1)

pbp_nfl <- pbp_nfl %>% filter(play_type %in% c("pass", "run")) %>% filter(special_teams_play == 0 & play_type %ni% c('no_play', 'qb_kneel', 'qb_spike', 'punt', 'field_goal')) %>% filter(!is.na(play_type)) %>% filter(!is.na(posteam)) %>% filter(is.na(two_point_conv_result))

pbp_nfl <- pbp_nfl %>% filter(!grepl("TWO-POINT CONVERSION", desc))

pbp_nfl$season_type <- ifelse(pbp_nfl$season_type == "REG", 0, 1)
pbp_nfl$roof <- ifelse(pbp_nfl$roof %in% c('dome', 'closed'), 1, 0)
pbp_nfl$surface <- ifelse(pbp_nfl$surface == "grass", 0, 1)
pbp_nfl$run_location <- case_when(is.na(pbp_nfl$run_location) ~ 2, pbp_nfl$run_location %in% c("left", "right") ~ 1, pbp_nfl$run_location == "middle" ~ 0)
pbp_nfl$run_gap <- case_when(is.na(pbp_nfl$run_gap) ~ 0, pbp_nfl$run_gap == "guard" ~ 1, pbp_nfl$run_gap == "tackle" ~ 2, pbp_nfl$run_gap == "end" ~ 3)

pbp_nfl$guard_ind = ifelse(pbp_nfl$run_gap == 1, 1, 0)
pbp_nfl$tackle_ind = ifelse(pbp_nfl$run_gap == 2, 1, 0)
pbp_nfl$end_ind = ifelse(pbp_nfl$run_gap == 3, 1, 0)



pbp_nfl <- pbp_nfl %>% filter(qb_spike == 0 & qb_kneel == 0)
pbp_nfl <- pbp_nfl %>% filter(play_type == "run") %>% filter(!is.na(run_gap)) %>% filter(!is.na(run_location))

pbp_nfl_run <- pbp_nfl %>% select(td_side, season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining) %>% as.data.frame()

pbp_nfl_run_weather <- pbp_nfl %>% filter(!is.na(wind)) %>% select(td_side, season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, posteam_ind, run_location, guard_ind, tackle_ind, end_ind, posteam_timeouts_remaining, defteam_timeouts_remaining, wind, temp) %>% as.data.frame()


###
# XGBOOST
###


sample_split_pbp <- sample.split(Y = pbp_nfl_run$td_side, SplitRatio = 0.8)
train_set_pbp <- subset(x = pbp_nfl_run, sample_split_pbp == TRUE)
test_set_pbp <- subset(x = pbp_nfl_run, sample_split_pbp == FALSE)

train_set_pbp <- train_set_pbp %>% filter(!is.na(td_side))
test_set_pbp <- test_set_pbp %>% filter(!is.na(td_side))

X_train_pbp <- train_set_pbp %>% select(-td_side) %>% as.data.frame()
X_test_pbp <- test_set_pbp %>% select(-td_side) %>% as.data.frame()
y_train_pbp <- train_set_pbp$td_side
y_test_pbp <- test_set_pbp$td_side


dtrain_pbp = xgb.DMatrix(data = as.matrix(X_train_pbp), label = y_train_pbp)
dtest_pbp = xgb.DMatrix(data =as.matrix(X_test_pbp), label = y_test_pbp)

d_xpbp_all = pbp_nfl_run %>% select(-td_side)
d_ypbp_all = pbp_nfl_run$td_side
d_all_pbp_all = xgb.DMatrix(data = as.matrix(d_xpbp_all), label = d_ypbp_all)

watchlist_pbp = list(train=dtrain_pbp, test=dtest_pbp)


eta_pbp_pbp = .09 # .09
gamma_pbp_pbp = 2 # 2
max_depth_pbp_pbp = 1 # 1
min_child_weight_pbp_pbp = 4 # 4
alpha_pbp_pbp = .25 # .25
lambda_pbp_pbp = .45 # .45
colsample_bynode_pbp_pbp = .725 # .725
colsample_bylevel_pbp_pbp = 1 # 1
colsample_bytree_pbp_pbp = 1 # 1

xgb_pbp_pbp <- xgboost(data = dtrain_pbp, 
                        label = y_train_pbp, 
                        eta = eta_pbp_pbp,
                        max_depth = max_depth_pbp_pbp, 
                        alpha = alpha_pbp_pbp,
                        lambda = lambda_pbp_pbp,
                        min_child_weight = min_child_weight_pbp_pbp,
                        colsample_bynode = colsample_bynode_pbp_pbp,
                        colsample_bytree = colsample_bytree_pbp_pbp,
                        colsample_bylevel = colsample_bylevel_pbp_pbp,
                        nround = 325, # 325
                        objective = "binary:logistic",
                        nthread = 2,
                        gamma = gamma_pbp_pbp,
                        early_stopping_rounds = 50
)

breaks <- seq(0, 1, by = 0.04)

labels <- sprintf("%.3f", head(breaks, -1))

test_preds = predict(xgb_pbp_pbp, newdata = dtest_pbp)

resultant_df_pbp_pbp = cbind(test_preds, y_test_pbp) %>% as.data.frame()
colnames(resultant_df_pbp_pbp) = c("Preds", "Vals")

resultant_df_pbp_pbp$buckets <- cut(resultant_df_pbp_pbp$Preds, breaks, labels = labels, include.lowest = TRUE, right = FALSE)

resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))


summary(xgb_pbp_pbp)

names_pbp_pbp = colnames(dtrain_pbp)

importance_matrix_before_all <- xgb.importance(names_pbp_pbp, model = xgb_pbp_pbp)
importance_matrix_before_all

predictions <- predict(xgb_pbp_pbp, newdata = dtest_pbp, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate confusion matrix and performance metrics
confusionMatrix(factor(predicted_classes), factor(y_test_pbp))

# .9696


View(resultant_df_pbp_pbp %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


xgb_pbp_pbp <- xgboost(data = as.matrix(d_xpbp_all), 
                       label = d_ypbp_all, 
                       eta = eta_pbp_pbp,
                       max_depth = max_depth_pbp_pbp, 
                       alpha = alpha_pbp_pbp,
                       lambda = lambda_pbp_pbp,
                       min_child_weight = min_child_weight_pbp_pbp,
                       colsample_bynode = colsample_bynode_pbp_pbp,
                       colsample_bytree = colsample_bytree_pbp_pbp,
                       colsample_bylevel = colsample_bylevel_pbp_pbp,
                       nround = 325, # 350
                       objective = "binary:logistic",
                       nthread = 2,
                       gamma = gamma_pbp_pbp,
                       early_stopping_rounds = 50
)

xgb.save(xgb_pbp_pbp, 'xgb_pbp_after_td.model')


# WEATHER DOESN'T REALLY MATTER
