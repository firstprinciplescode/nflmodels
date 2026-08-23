
colnames(pbp_rush)
colnames(pbp_rush)[511] <- "qbgrp_ssn"
colnames(pbp_rush)[512] <- "def_ssn"

pbp_rush <- pbp_rush %>% select(-c(qbgrp_ssn.y, def_ssn.y))

pbp_rush_statistics <- 
  pbp_rush %>%
    group_by(rusher_player_id, week, season, qbgrp_ssn, def_ssn) %>%
    dplyr::summarise(pbp_rushes = n(),
                     ypc = mean(yards_gained, na.rm = T),
                     pbp_xypc = mean(pbp_predicted_ypc, na.rm = T),
                     part_xypc = mean(part_predicted_ypc, na.rm = T),
                     pbp_xtd = sum(pbp_after_new_xtd, na.rm = T),
                     part_xtd = sum(part_after_new_xtd, na.rm = T))

contact_statistics <- 
  rushing_summary_rank %>%
    mutate(yards_before_contact = yards - yards_after_contact) %>%
    group_by(player_id, gsis_id, player, position_group, team, week, season, qbgrp_ssn, rank_grp, pos_rank, team_rank) %>%
    dplyr::summarise(attempts = sum(attempts, na.rm = T) - sum(scrambles, na.rm = T),
                      ybc = sum(yards_before_contact, na.rm = T) / sum(attempts, na.rm = T),
                      yac = sum(yards_after_contact, na.rm = T) / sum(attempts, na.rm = T),
                      rush_proportion = mean(rush_proportion, na.rm = T))

situation_cluster_df %>%
  dplyr::select(rusher_player_id:rank_grp, cluster)

gap_cluster_df %>%
  dplyr::select(rusher_player_id:rank_grp, cluster)

player_zone_gap_zscore %>%
  dplyr::select(player:season, gap_z)


rush_stats_draft_one <- left_join(pbp_rush_statistics,
                                  contact_statistics,
                                  by = c("rusher_player_id" = "gsis_id", "week" = "week", "season" = "season", "qbgrp_ssn" = "qbgrp_ssn"))

rush_stats_draft_two <- left_join(rush_stats_draft_one, 
          situation_cluster_df %>%
            dplyr::select(rusher_player_id:rank_grp, cluster),
          by = c("rusher_player_id" = "rusher_player_id", "team" = "posteam", "season" = "season", "rank_grp" = "rank_grp"))
colnames(rush_stats_draft_two)[23] <- "situation_cluster"

rush_stats_draft_three <- left_join(rush_stats_draft_two, 
          gap_cluster_df %>%
            dplyr::select(rusher_player_id:rank_grp, cluster),
          by = c("rusher_player_id" = "rusher_player_id", "team" = "posteam", "season" = "season", "rank_grp" = "rank_grp"))
colnames(rush_stats_draft_three)[24] <- "gap_cluster"


### JUMP TO THE EP ONE
### YAY

rush_stats_draft_four <- left_join(rush_stats_draft_three,
                                   rusher_xtd_final %>% select(rusher_player_id:season, xtd_percentile), 
                                   by = c("rusher_player_id" = "rusher_player_id", "rank_grp" = "rank_grp", "situation_cluster" = "sit_cluster", "gap_cluster" = "gap_cluster", "team" = "posteam", "season" = "season")
          )

rush_stats_final <- left_join(rush_stats_draft_four,
          player_zone_gap_zscore %>%
            dplyr::select(player_id:season, gap_z),
          by = c("player_id", "rank_grp", "season"))


rush_stats_final$part_xtd[which(rush_stats_final$season == 2025)] <- NA

rush_stats_final <- rush_stats_final %>%
  group_by(qbgrp_ssn, def_ssn, week, season) %>%
  mutate(
    rush_share = attempts / sum(attempts, na.rm = TRUE),
    pbp_xtd_share = pbp_xtd / sum(pbp_xtd, na.rm = TRUE),
    part_xtd_share = part_xtd / sum(part_xtd, na.rm = TRUE)
  ) %>%
  ungroup()

# KNOW THAT FOR THE GAP_Z STUFF --> GONNA HAVE TO TAKE NA AS AN INPUT TOO


#rm(combined_ids)
rm(combined_pbp)
#rm(pbp_rush)

rm(first_down)
rm(second_down)
rm(third_down)
rm(fourth_down)


