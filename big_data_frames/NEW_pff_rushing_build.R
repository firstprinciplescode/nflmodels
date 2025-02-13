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



rushing_stats <- read.xlsx("df_rushing_stats.xlsx") 



attach("NEW_pbp_part_combined_join_workspace.RData")

combined_pbp_info <- combined_info

detach("file:NEW_pbp_part_combined_join_workspace.RData")


attach("NEW_pff_ids_build_workspace.RData")

combined_ids_final <- combined_ids_final

detach("file:NEW_pff_ids_build_workspace.RData")


snaps_df <- read.xlsx("nfl_plays.xlsx")

game_id_df <- read.xlsx("nfl_game_ids.xlsx") %>% distinct()

away_id_df <- game_id_df %>% select(away_franchise_id, away_abbreviation)
home_id_df <- game_id_df %>% select(home_franchise_id, home_abbreviation)

colnames(away_id_df) <- c("franchise_id", "team")
colnames(home_id_df) <- c("franchise_id", "team")


team_id_df <- rbind(away_id_df, home_id_df) %>% distinct() %>% arrange(franchise_id)
team_id_df$team[which(team_id_df$franchise_id == 27)] <- 'LAC'
team_id_df$team[which(team_id_df$franchise_id == 23)] <- 'LV'

team_id_df <- team_id_df %>% distinct()


snaps_df <- left_join(snaps_df, team_id_df, by = c("franchise_id"))

snaps_df <- left_join(snaps_df, game_id_df %>% select(id, season, week), by = c("game_id" = "id"))


combined_pbp_info <- combined_pbp_info %>% 
  mutate(mixed_xpass = case_when(!is.na(part_predicted_xpass) ~ part_predicted_xpass,
                                 !is.na(predicted_xpass) & is.na(xpass) ~ predicted_xpass,
                                 !is.na(xpass) & is.na(predicted_xpass) ~ xpass,
                                 !is.na(xpass) & !is.na(predicted_xpass) ~ -.03 + .778 * predicted_xpass + .258 * xpass,
                                 TRUE ~ NA_real_))


rush_snaps_df <- snaps_df %>% filter(snap_counts_run > 0)

rush_snaps_df <- rush_snaps_df %>% mutate(position_group = ifelse(position %in% c("HB", "QB", "FB"), "HB", "NonHB"))


qb_scramble_counts <- combined_pbp_info |> filter(qb_kneel == 0, qb_scramble == 1) |>
  group_by(rusher_player_name, rusher_player_id, posteam, week, season) |>
  summarize(
    scrambles = sum(qb_scramble, na.rm = TRUE)
  )

qb_scramble_counts <- left_join(qb_scramble_counts, combined_ids_final %>% select(player_id, team, Week, Season, player_name_join, gsis_id_final) %>% distinct(), by = c("rusher_player_id" = "gsis_id_final", "week" = "Week", "season" = "Season", "posteam" = "team"))


rush_snaps_df <- left_join(rush_snaps_df, qb_scramble_counts %>% ungroup() %>% select(player_id, posteam, week, season, scrambles), by = c("player_id" = "player_id", "week" = "week", "season" = "season", "team" = "posteam"))

rush_snaps_df <- left_join(rush_snaps_df, combined_ids_final %>% ungroup() %>% select(player_id, team, Week, Season, player_name_join, gsis_id_final) %>% distinct(), by = c("player_id" = "player_id", "week" = "Week", "season" = "Season", "team" = "team"))


rush_snaps_df <- rush_snaps_df %>%
  group_by(player_id) %>%
  arrange(player_id) %>%
  fill(gsis_id_final, .direction = "downup") %>%
  ungroup()



# FOR PLAYERS WITH MISSING GSIS_ID_FINAL ... ALMOST CERTAINLY THEY'RE MISSING THEIR PEOPLE IN COMBINED_IDS_FINAL
# I'M GOING TO HAVE TO FUCKING INTERPOLATE. NOT BEFORE I ACTUALLY ONE-HOT ENCODE THOUGH ON THE SHIT THAT PAYS THE BILLS. 
# LIKELY MEANS I'M GOING TO HAVE TO FIX SHIT IN THE RECEIVING ONE TOO. FML. 


rush_snaps_df$scrambles[which(is.na(rush_snaps_df$scrambles))] <- 0

rush_snaps_df$status[which(is.na(rush_snaps_df$status))] <- "P"

# ACTUALLY ... MAY WANT TO INTRODUCE THESE RANKS AFTER WE SUBTRACT 
rush_snaps_df <- sqldf("
    SELECT *,
           ROW_NUMBER() OVER (
             PARTITION BY team, position_group, game_id
             ORDER BY snap_counts_run DESC, snap_counts_total_run DESC, snap_counts_total_run DESC, snap_counts_total DESC
           ) AS pos_rank,
           ROW_NUMBER() OVER (
             PARTITION BY team, game_id
             ORDER BY snap_counts_run DESC, snap_counts_total_run DESC, snap_counts_total_run DESC, snap_counts_total DESC
           ) AS team_rank
    FROM rush_snaps_df
")

rush_snaps_df <- rush_snaps_df %>% ungroup() %>% group_by(team, week, season) %>% 
  mutate(total_rushes = sum(snap_counts_run, na.rm = T)) %>% ungroup() %>%
  mutate(rush_proportion = snap_counts_run / total_rushes)


rush_order_df <- rush_snaps_df %>% select(player, player_id, position_group, team, week, season, status, pos_rank, team_rank, rush_proportion, gsis_id_final)


rush_order_df %>%
  filter(team_rank == 2) %>%  # Filter for rushers with most rushes in their team
  pull(rush_proportion) %>%   # Extract the rush_proportion column
  quantile(probs = seq(0.1, 0.9, by = 0.1))  # Compute 10th through 90th percentiles


rush_order_df <- rush_order_df %>%
  mutate(rank_grp = case_when(
    rush_proportion >= 0.475 ~ "A",
    rush_proportion >= .15 & rush_proportion < 0.475 ~ "B",
    TRUE ~ "C"
  ))

rush_order_df <- rush_order_df %>%
  group_by(player_id) %>%
  arrange(player_id) %>%
  fill(gsis_id_final, .direction = "downup") %>%
  ungroup()


pbp_rush_info <- left_join(combined_pbp_info %>% filter(rush == 1, qb_scramble == 0, qb_kneel == 0), rush_order_df %>% select(gsis_id_final, position_group:rush_proportion, rank_grp) %>% distinct(), by = c("rusher_player_id" = "gsis_id_final", "posteam" = "team", "week" = "week", "season" = "season"))

pbp_rush_info <- pbp_rush_info %>% mutate(ydstogo_group = case_when(
    down == 1 & ydstogo < 10 ~ "1st9",
    down == 1 & ydstogo >= 10 ~ "1st10+",
    down == 2 & ydstogo <= 1 ~ "2nd1",
    down == 2 & ydstogo <= 4 & ydstogo >= 2 ~ "2nd2-4",
    down == 2 & ydstogo <= 7 & ydstogo >= 5 ~ "2nd5-7",
    down == 2 & ydstogo <= 10 & ydstogo >= 8 ~ "2nd8-10",    
    down == 2 & ydstogo >= 11 ~ "2ndLong", 
    down == 3 & ydstogo <= 1 ~ "3rd1",
    down == 3 & ydstogo >= 2 & ydstogo <= 3 ~ "3rd2-3",
    down == 3 & ydstogo >= 4 ~ "3rdRest",
    down == 4 & ydstogo <= 1 ~ "4th1",
    down == 4 & ydstogo > 1 ~ "4thLong",
    TRUE ~ NA_character_))


pbp_rush_info <- pbp_rush_info %>% group_by(game_id, posteam) %>% 
  mutate(med_run_xpass = mean(mixed_xpass, na.rm = T),
         run_xpass_diff = mixed_xpass - med_run_xpass)

pbp_rush_info$rank_grp[which(is.na(pbp_rush_info$rank_grp))] <- "C"

rusher_xpass_diff_df <- pbp_rush_info %>%
  # filter(season == 2022, posteam == "BLT", !is.na(rusher_player_name), qb_kneel == 0) %>%
  group_by(rusher_player_name, rusher_player_id, rank_grp, posteam, season) %>%
  summarize(
    q20_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.8, na.rm = TRUE),
    q20_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.8, na.rm = TRUE),    
    n = n()
  ) %>% filter(n > 5)
  
