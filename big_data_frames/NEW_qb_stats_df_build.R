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


pbp_info <- load_pbp(c(2016:2024))

pbp_info$posteam[which(pbp_info$posteam == "ARI")] = "ARZ"
pbp_info$posteam[which(pbp_info$posteam == "BAL")] = "BLT"
pbp_info$posteam[which(pbp_info$posteam == "CLE")] = "CLV"
pbp_info$posteam[which(pbp_info$posteam == "HOU")] = "HST"
pbp_info$posteam[which(pbp_info$posteam == "SD")] = "LAC"
pbp_info$posteam[which(pbp_info$posteam == "OAK")] = "LV"

pbp_info$td_side = ifelse((!is.na(pbp_info$td_team) & pbp_info$posteam == pbp_info$td_team), 1, 0)

pbp_info$week <- ifelse(pbp_info$week == 18 & pbp_info$season <= 2020, 28, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 19 & pbp_info$season <= 2020, 29, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 19 & pbp_info$season > 2020, 28, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 20 & pbp_info$season <= 2020, 30, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 20 & pbp_info$season > 2020, 29, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 21 & pbp_info$season <= 2020, 32, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 21 & pbp_info$season > 2020, 30, pbp_info$week)
pbp_info$week <- ifelse(pbp_info$week == 22, 32, pbp_info$week)


real_scores <- pbp_info %>% group_by(posteam, week, season) %>% 
  dplyr::summarise(tds = sum(touchdown, na.rm = T), 
                   fgs = sum(field_goal_attempt, na.rm = T))

rm(pbp_info)


attach("NEW_pbp_part_combined_join_workspace.RData")

combined_pbp_info <- combined_info

detach("file:NEW_pbp_part_combined_join_workspace.RData")


qb_stats_df_base <- combined_pbp_info %>% 
  group_by(qbgrp_ssn, def_ssn, posteam, defteam, week, season) %>% 
  dplyr::summarise(
    pass_rate = sum(pass_attempt, na.rm = T) / sum(play, na.rm = T),
    fastr_xpass_rate = sum(xpass, na.rm = T) / sum(play, na.rm = T),
    pbp_xpass_rate = sum(predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    part_xpass_rate = sum(part_predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    scr_rate = (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))) / sum(qb_dropback, na.rm = TRUE),
    scr_ypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), yards_gained, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    pbp_scr_xypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), predicted_ypc, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    part_scr_xypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), part_predicted_ypc, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    acc_rate = sum(complete_pass, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    fastr_cp = sum(cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_cp = sum(predicted_cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    part_cp = sum(part_predicted_cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    sack_rate = sum(sack, na.rm = TRUE) / sum(qb_dropback, na.rm = TRUE),
    pbp_sack_rate = sum(predicted_sack, na.rm = T) / sum(qb_dropback, na.rm = TRUE),
    part_sack_rate = sum(part_predicted_sack, na.rm = T) / sum(qb_dropback, na.rm = TRUE),
    ypa = sum(passing_yards, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_xypa = sum(predicted_ypa, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    part_xypa = sum(part_predicted_ypa, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    yac = sum(yards_after_catch, na.rm = TRUE) / sum(complete_pass, na.rm = T),
    pbp_yac = sum(predicted_yac, na.rm = T) / sum(complete_pass, na.rm = T),
    part_yac = sum(part_predicted_yac, na.rm = T) / sum(complete_pass, na.rm = T),
    adot = sum(air_yards, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    plays = sum(play, na.rm = TRUE),
    no_huddle = sum(no_huddle, na.rm = TRUE) / sum(play, na.rm = TRUE),
    pbp_xtds = sum(after_xtd_new, na.rm = T),
    part_xtds = sum(part_after_xtd_new, na.rm = T)
  ) 

qb_stats_df_base <- left_join(qb_stats_df_base, real_scores, by = c("posteam", "week", "season"))


# FOR SOME OF THESE THINGS --> 0 SHOULD BE NAs HERE. ALSO --> THIS WILL PROBABLY HAVE TO BE BY USING A PART_XPASS_RATE, SINCE SOMETIMES THEORETICALLY PART_SACK_RATE WILL BE 0.


qb_stats_df_base$part_sack_rate[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA
# GOTTA START WITH PART_SACK_RATE
qb_stats_df_base$part_scr_xypc[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA

qb_stats_df_base$part_cp[which(qb_stats_df_base$part_cp == 0)] <- NA
qb_stats_df_base$part_xypa[which(qb_stats_df_base$part_xypa == 0)] <- NA
qb_stats_df_base$part_yac[which(qb_stats_df_base$part_yac == 0)] <- NA
qb_stats_df_base$part_xtds[which(qb_stats_df_base$part_xtds == 0)] <- NA

qb_stats_df_base$part_xpass_rate[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA


attach("NEW_blitz_doc_build_workspace.RData")

df_blitz_agg_reg <- df_blitz_agg

detach("file:NEW_blitz_doc_build_workspace.RData")


attach("NEW_depth_doc_build_workspace.RData")

df_depth_agg_reg <- df_depth_agg

detach("file:NEW_depth_doc_build_workspace.RData")


attach("NEW_less_doc_build_workspace.RData")

df_less_agg_reg <- df_less_agg

detach("file:NEW_less_doc_build_workspace.RData")


attach("NEW_pa_doc_build_workspace.RData")

df_pa_agg_reg <- df_pa_agg

detach("file:NEW_pa_doc_build_workspace.RData")


attach("NEW_pressure_doc_build_workspace.RData")

df_pressure_agg_reg <- combined_press_agg

detach("file:NEW_pressure_doc_build_workspace.RData")


attach("NEW_df_twp_build_workspace.RData")

df_twp_agg_stats <- df_twp_agg_stats 

detach("file:NEW_df_twp_build_workspace.RData")


qb_stats_df_base <- left_join(qb_stats_df_base, df_blitz_agg_reg %>% select(team_name:Season, blitz_rate, blitz_grade, no_blitz_grade, blitz_qbr, no_blitz_qbr), by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))
qb_stats_df_base <- left_join(qb_stats_df_base, df_depth_agg_reg %>% select(team_name:Season, behind_los_rate:deep_rate, behind_los_grade, short_grade, medium_grade, deep_grade, behind_los_qbr, short_qbr, medium_qbr, deep_qbr), by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))
qb_stats_df_base <- left_join(qb_stats_df_base, df_less_agg_reg %>% select(team_name:Season, less_rate, less_grade, more_grade, less_qbr, more_qbr), by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))
qb_stats_df_base <- left_join(qb_stats_df_base, df_pa_agg_reg %>% select(team_name:Season, pa_rate, npa_grade, pa_grade, npa_qbr, pa_qbr), by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))
qb_stats_df_base <- left_join(qb_stats_df_base, df_pressure_agg_reg %>% select(team_name:Season, pressure_rate, no_pressure_grade, pressure_grade, no_pressure_qbr, pressure_qbr), by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))
qb_stats_df_base <- left_join(qb_stats_df_base, df_twp_agg_stats, by = c("posteam" = "team_name", "week" = "Week", "season" = "Season"))


qb_stats_df_base <- qb_stats_df_base %>% ungroup()


qb_stats_df_final <- qb_stats_df_base %>%
  group_by(qbgrp_ssn) %>%
  mutate(
    pass_rate_rank = (rank(pass_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_xpass_rate_rank = (rank(fastr_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    pbp_xpass_rate_rank = (rank(pbp_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    part_xpass_rate_rank = (rank(part_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    scr_rate_rank = (rank(scr_rate, ties.method = "average") - 1) / (n() - 1),
    scr_ypc_rank = (rank(scr_ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_scr_xypc_rank = (rank(pbp_scr_xypc, ties.method = "average") - 1) / (n() - 1),
    part_scr_xypc_rank = (rank(part_scr_xypc, ties.method = "average") - 1) / (n() - 1),
    acc_rate_rank = (rank(acc_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_cp_rank = (rank(fastr_cp, ties.method = "average") - 1) / (n() - 1),
    pbp_cp_rank = (rank(pbp_cp, ties.method = "average") - 1) / (n() - 1),
    part_cp_rank = (rank(part_cp, ties.method = "average") - 1) / (n() - 1),
    sack_rate_rank = (n() - rank(sack_rate, ties.method = "average")) / (n() - 1),
    pbp_sack_rate_rank = (n() - rank(pbp_sack_rate, ties.method = "average")) / (n() - 1),
    part_sack_rate_rank = (n() - rank(part_sack_rate, ties.method = "average")) / (n() - 1),
    ypa_rank = (rank(ypa, ties.method = "average") - 1) / (n() - 1),
    pbp_xypa_rank = (rank(pbp_xypa, ties.method = "average") - 1) / (n() - 1),
    part_xypa_rank = (rank(part_xypa, ties.method = "average") - 1) / (n() - 1),
    yac_rank = (rank(yac, ties.method = "average") - 1) / (n() - 1),
    pbp_yac_rank = (rank(pbp_yac, ties.method = "average") - 1) / (n() - 1),
    part_yac_rank = (rank(part_yac, ties.method = "average") - 1) / (n() - 1),
    adot_rank = (rank(adot, ties.method = "average") - 1) / (n() - 1),
    plays_rank = (rank(plays, ties.method = "average") - 1) / (n() - 1),
    no_huddle_rank = (rank(no_huddle, ties.method = "average") - 1) / (n() - 1),
    pbp_xtds_rank = (rank(pbp_xtds, ties.method = "average") - 1) / (n() - 1),
    part_xtds_rank = (rank(part_xtds, ties.method = "average") - 1) / (n() - 1),
    
    blitz_rate_rank = (rank(blitz_rate, ties.method = "average") - 1) / (n() - 1),
    no_blitz_gr_rank = (rank(no_blitz_grade, ties.method = "average") - 1) / (n() - 1),
    blitz_gr_rank = (rank(blitz_grade, ties.method = "average") - 1) / (n() - 1),
    no_blitz_qbr_rank = (rank(no_blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    blitz_qbr_rank = (rank(blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    
    behind_los_rate_rank = (rank(behind_los_rate, ties.method = "average") - 1) / (n() - 1),
    short_rate_rank = (rank(short_rate, ties.method = "average") - 1) / (n() - 1),
    medium_rate_rank = (rank(medium_rate, ties.method = "average") - 1) / (n() - 1),
    deep_rate_rank = (rank(deep_rate, ties.method = "average") - 1) / (n() - 1),    
    
    behind_los_gr_rank = (rank(behind_los_grade, ties.method = "average") - 1) / (n() - 1),
    short_gr_rank = (rank(short_grade, ties.method = "average") - 1) / (n() - 1),
    medium_gr_rank = (rank(medium_grade, ties.method = "average") - 1) / (n() - 1),
    deep_gr_rank = (rank(deep_grade, ties.method = "average") - 1) / (n() - 1),
    behind_los_qbr_rank = (rank(behind_los_qbr, ties.method = "average") - 1) / (n() - 1),
    short_qbr_rank = (rank(short_qbr, ties.method = "average") - 1) / (n() - 1),
    medium_qbr_rank = (rank(medium_qbr, ties.method = "average") - 1) / (n() - 1),
    deep_qbr_rank = (rank(deep_qbr, ties.method = "average") - 1) / (n() - 1),
    
    less_rate_rank = (rank(less_rate, ties.method = "average")) / (n() - 1),
    less_gr_rank = (rank(less_grade, ties.method = "average") - 1) / (n() - 1),
    more_gr_rank = (rank(more_grade, ties.method = "average") - 1) / (n() - 1),
    less_qbr_rank = (rank(less_qbr, ties.method = "average") - 1) / (n() - 1),
    more_qbr_rank = (rank(more_qbr, ties.method = "average") - 1) / (n() - 1),    
    
    pa_rate_rank = (rank(pa_rate, ties.method = "average") - 1) / (n() - 1),
    npa_gr_rank = (rank(npa_grade, ties.method = "average") - 1) / (n() - 1),
    pa_gr_rank = (rank(pa_grade, ties.method = "average") - 1) / (n() - 1),
    npa_qbr_rank = (rank(npa_qbr, ties.method = "average") - 1) / (n() - 1),
    pa_qbr_rank = (rank(pa_qbr, ties.method = "average") - 1) / (n() - 1),     
    
    pressure_rate_rank = (n() - rank(pressure_rate, ties.method = "average")) / (n() - 1),
    no_pressure_gr_rank = (rank(no_pressure_grade, ties.method = "average") - 1) / (n() - 1),
    pressure_gr_rank = (rank(pressure_grade, ties.method = "average") - 1) / (n() - 1),
    no_pressure_qbr_rank = (rank(no_pressure_qbr, ties.method = "average") - 1) / (n() - 1),
    pressure_qbr_rank = (rank(pressure_qbr, ties.method = "average") - 1) / (n() - 1), 
    
    twp_rate_rank = (n() - rank(twp_rate, ties.method = "average")) / (n() - 1),
    int_rate_rank = (n() - rank(int_rate, ties.method = "average")) / (n() - 1)
  ) %>%
  ungroup()

qb_stats_df_final <- qb_stats_df_final %>%
  group_by(def_ssn) %>%
  mutate(
    pass_rate_rank_def = (rank(pass_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_xpass_rate_rank_def = (rank(fastr_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    pbp_xpass_rate_rank_def = (rank(pbp_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    part_xpass_rate_rank_def = (rank(part_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    scr_rate_rank_def = (rank(scr_rate, ties.method = "average") - 1) / (n() - 1),
    scr_ypc_rank_def = (rank(scr_ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_scr_xypc_rank_def = (rank(pbp_scr_xypc, ties.method = "average") - 1) / (n() - 1),
    part_scr_xypc_rank_def = (rank(part_scr_xypc, ties.method = "average") - 1) / (n() - 1),
    acc_rate_rank_def = (rank(acc_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_cp_rank_def = (rank(fastr_cp, ties.method = "average") - 1) / (n() - 1),
    pbp_cp_rank_def = (rank(pbp_cp, ties.method = "average") - 1) / (n() - 1),
    part_cp_rank_def = (rank(part_cp, ties.method = "average") - 1) / (n() - 1),
    sack_rate_rank_def = (n() - rank(sack_rate, ties.method = "average")) / (n() - 1),
    pbp_sack_rate_rank_def = (n() - rank(pbp_sack_rate, ties.method = "average")) / (n() - 1),
    part_sack_rate_rank_def = (n() - rank(part_sack_rate, ties.method = "average")) / (n() - 1),
    ypa_rank_def = (rank(ypa, ties.method = "average") - 1) / (n() - 1),
    pbp_xypa_rank_def = (rank(pbp_xypa, ties.method = "average") - 1) / (n() - 1),
    part_xypa_rank_def = (rank(part_xypa, ties.method = "average") - 1) / (n() - 1),
    yac_rank_def = (rank(yac, ties.method = "average") - 1) / (n() - 1),
    pbp_yac_rank_def = (rank(pbp_yac, ties.method = "average") - 1) / (n() - 1),
    part_yac_rank_def = (rank(part_yac, ties.method = "average") - 1) / (n() - 1),
    adot_rank_def = (rank(adot, ties.method = "average") - 1) / (n() - 1),
    plays_rank_def = (rank(plays, ties.method = "average") - 1) / (n() - 1),
    no_huddle_rank_def = (rank(no_huddle, ties.method = "average") - 1) / (n() - 1),
    pbp_xtds_rank_def = (rank(pbp_xtds, ties.method = "average") - 1) / (n() - 1),
    part_xtds_rank_def = (rank(part_xtds, ties.method = "average") - 1) / (n() - 1),
    
    blitz_rate_rank_def = (rank(blitz_rate, ties.method = "average") - 1) / (n() - 1),
    no_blitz_gr_rank_def = (rank(no_blitz_grade, ties.method = "average") - 1) / (n() - 1),
    blitz_gr_rank_def = (rank(blitz_grade, ties.method = "average") - 1) / (n() - 1),
    no_blitz_qbr_rank_def = (rank(no_blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    blitz_qbr_rank_def = (rank(blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    
    behind_los_rate_rank_def = (rank(behind_los_rate, ties.method = "average") - 1) / (n() - 1),
    short_rate_rank_def = (rank(short_rate, ties.method = "average") - 1) / (n() - 1),
    medium_rate_rank_def = (rank(medium_rate, ties.method = "average") - 1) / (n() - 1),
    deep_rate_rank_def = (rank(deep_rate, ties.method = "average") - 1) / (n() - 1),  
    
    behind_los_gr_rank_def = (rank(behind_los_grade, ties.method = "average") - 1) / (n() - 1),
    short_gr_rank_def = (rank(short_grade, ties.method = "average") - 1) / (n() - 1),
    medium_gr_rank_def = (rank(medium_grade, ties.method = "average") - 1) / (n() - 1),
    deep_gr_rank_def = (rank(deep_grade, ties.method = "average") - 1) / (n() - 1),
    behind_los_qbr_rank_def = (rank(behind_los_qbr, ties.method = "average") - 1) / (n() - 1),
    short_qbr_rank_def = (rank(short_qbr, ties.method = "average") - 1) / (n() - 1),
    medium_qbr_rank_def = (rank(medium_qbr, ties.method = "average") - 1) / (n() - 1),
    deep_qbr_rank_def = (rank(deep_qbr, ties.method = "average") - 1) / (n() - 1),
    
    less_rate_rank_def = (rank(less_rate, ties.method = "average")) / (n() - 1),
    less_gr_rank_def = (rank(less_grade, ties.method = "average") - 1) / (n() - 1),
    more_gr_rank_def = (rank(more_grade, ties.method = "average") - 1) / (n() - 1),
    less_qbr_rank_def = (rank(less_qbr, ties.method = "average") - 1) / (n() - 1),
    more_qbr_rank_def = (rank(more_qbr, ties.method = "average") - 1) / (n() - 1),    
    
    pa_rate_rank_def = (rank(pa_rate, ties.method = "average") - 1) / (n() - 1),
    npa_gr_rank_def = (rank(npa_grade, ties.method = "average") - 1) / (n() - 1),
    pa_gr_rank_def = (rank(pa_grade, ties.method = "average") - 1) / (n() - 1),
    npa_qbr_rank_def = (rank(npa_qbr, ties.method = "average") - 1) / (n() - 1),
    pa_qbr_rank_def = (rank(pa_qbr, ties.method = "average") - 1) / (n() - 1),     
    
    pressure_rate_rank_def = (n() - rank(pressure_rate, ties.method = "average")) / (n() - 1),
    no_pressure_gr_rank_def = (rank(no_pressure_grade, ties.method = "average") - 1) / (n() - 1),
    pressure_gr_rank_def = (rank(pressure_grade, ties.method = "average") - 1) / (n() - 1),
    no_pressure_qbr_rank_def = (rank(no_pressure_qbr, ties.method = "average") - 1) / (n() - 1),
    pressure_qbr_rank_def = (rank(pressure_qbr, ties.method = "average") - 1) / (n() - 1), 
    
    twp_rate_rank_def = (n() - rank(twp_rate, ties.method = "average")) / (n() - 1),
    int_rate_rank_def = (n() - rank(int_rate, ties.method = "average")) / (n() - 1)
  ) %>%
  ungroup()


qb_stats_df_final$part_xpass_rate_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_scr_xypc_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_cp_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_sack_rate_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_xypa_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_yac_rank[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_xtds_rank[which(qb_stats_df_final$season == 2024)] <- NA

qb_stats_df_final$part_xpass_rate_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_scr_xypc_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_cp_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_sack_rate_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_xypa_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_yac_rank_def[which(qb_stats_df_final$season == 2024)] <- NA
qb_stats_df_final$part_xtds_rank_def[which(qb_stats_df_final$season == 2024)] <- NA


colnames(qb_stats_df_final)

qb_stats_df_final %>% filter(season == 2024) %>% group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T))


qb_stats_df_final %>% filter(season == 2024) %>% group_by(def_ssn) %>%
  dplyr::summarise(mn_xtd_rank = mean(pbp_xtds_rank, na.rm = T))
