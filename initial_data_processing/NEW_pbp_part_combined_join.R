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

'%ni%' <- Negate('%in%')



attach("NEW_participation_combined_workspace.RData")

part_nfl <- part_nfl

detach("file:NEW_participation_combined_workspace.RData")



attach("NEW_pbp_combined_workspace.RData")

pbp_nfl <- pbp_nfl

detach("file:NEW_pbp_combined_workspace.RData")



qbgrp_df <- read.csv('nfl_weeks_good.csv') %>% select(-X)

defgrp_df <- read.csv('nfl_def_weeks_good.csv')



######
###### PART_NFL$NFLVERSE_GAME_ID = PBP_NFL$GAME_ID, PLAY_ID = PLAY_ID
######


part_nfl_join <- part_nfl %>% select(nflverse_game_id, play_id, offense_formation, offense_personnel, defenders_in_box, defense_personnel, ngs_air_yards, time_to_throw, was_pressure, predicted_defenders_in_box, predicted_pass_rushers, predicted_time_to_throw_snap, predicted_time_to_throw, before_xtd, before_xtd_new, after_pass_xtd, after_run_xtd, after_xtd, after_xtd_new, predicted_pressure, predicted_sack, predicted_xpass, predicted_cp, predicted_ypc, predicted_ypa, predicted_yac)

colnames(part_nfl_join)[c(10:26)] <- paste0("part_", colnames(part_nfl_join)[c(10:26)])


combined_info <- left_join(pbp_nfl, part_nfl_join, by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))



######
######
######


combined_info <- combined_info %>%
  left_join(
    qbgrp_df %>%
      mutate(OffPerformance = performance) %>%
      select(team_name, week, season, qbgrp_ssn, OffPerformance),
    by = c("posteam" = "team_name", "week" = "week", "season" = "season")
  ) %>%
  left_join(
    defgrp_df %>%
      mutate(DefPerformance = performance) %>%
      select(opponent, week, season, def_ssn, DefPerformance),
    by = c("defteam" = "opponent", "week" = "week", "season" = "season")
  )


#####
#####
#####


combined_info$receiver_player_name[which(combined_info$receiver_player_name == "N.Dell")] <- "T.Dell"
combined_info$rusher_player_name[which(combined_info$rusher_player_name == "N.Dell")] <- "T.Dell"
          

#####
#####
#####


pbp_xtd <- pbp_nfl %>% group_by(posteam, week, season) %>% 
  dplyr::summarise(before_old_xtd = sum(predicted_before_xtd),
                   before_new_xtd = sum(before_xtd_new),
                   after_old_xtd = sum(after_td),
                   after_new_xtd = sum(after_xtd_new),
                   actual_td = sum(touchdown),
                   actual_fg = sum(field_goal_attempt))


part_xtd <- part_nfl %>% group_by(posteam, defteam, week, season) %>% 
  dplyr::summarise(before_old_xtd = sum(before_xtd),
                   before_new_xtd = sum(before_xtd_new),
                   after_old_xtd = sum(after_xtd),
                   after_new_xtd = sum(after_xtd_new),
                   actual_td = sum(touchdown),
                   actual_fg = sum(field_goal_attempt))


combined_info$mixed_xtd <- ifelse(!is.na(combined_info$part_after_xtd_new), .2181 * combined_info$after_xtd_new + .2559 * combined_info$part_after_xtd_new + .1336 * combined_info$part_after_xtd + .1058 * combined_info$after_xtd_new + .0988 * combined_info$part_before_xtd_new + .0486 * combined_info$predicted_before_xtd + .0812 * combined_info$before_xtd_new + .0579 * combined_info$part_before_xtd, (.2181 / (.2181 + .1058 + .0486 + .0812)) * combined_info$after_xtd_new + (.1058 / (.2181 + .1058 + .0486 + .0812)) * combined_info$after_td + (.0486 / (.2181 + .1058 + .0486 + .0812)) * combined_info$predicted_before_xtd + (.0812 / (.2181 + .1058 + .0486 + .0812)) * combined_info$before_xtd_new) 

combined_xtd_df$mixed_xtd <- ifelse(!is.na(combined_xtd_df$part_after_new_xtd), .2181 * combined_xtd_df$pbp_after_new_xtd + .2559 * combined_xtd_df$part_after_new_xtd + .1336 * combined_xtd_df$part_after_old_xtd + .1058 * combined_xtd_df$pbp_after_old_xtd + .0988 * combined_xtd_df$part_before_new_xtd + .0486 * combined_xtd_df$pbp_before_old_xtd + .0812 * combined_xtd_df$pbp_before_new_xtd + .0579 * combined_xtd_df$part_before_old_xtd, (.2182 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_after_new_xtd + (.1058 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_after_old_xtd + (.0812 / (.2182 + .1058 + .0548 + .0486)) * combined_xtd_df$pbp_before_old_xtd + (.0547 / (.2182 + .1058 + .0812 + .0486)) * combined_xtd_df$pbp_before_new_xtd) 
