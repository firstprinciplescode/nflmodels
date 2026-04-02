
situation_cluster_df

gap_cluster_df

conflict_prefer_all("dplyr", quiet = TRUE)


####
####


combined_grade_epa_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.combined_grade_epa_summary
")


###
###


rushing_summary <- left_join(
    rushing_summary,
    combined_grade_epa_summary %>%
      dplyr::select(posteam:season, qbgrp_ssn),
    by = c("team" = "posteam", "week" = "week", "season" = "season")
  )

rushing_summary <- 
  rushing_summary %>% filter(!(scrambles >= attempts))

# rush_order_df
# WE NEED TO JOIN THE ORDER 

rushing_summary_rank <- 
  left_join(rushing_summary,
          rush_order_df %>% dplyr::select(-c(player, status)), 
          by = c("player_id" = "player_id", "team" = "abbreviation", "week" = "week", "season" = "season")) %>%
  filter(!is.na(grades_run)) %>%
  filter(!is.na(rank_grp))

# GOTTA FIGURE OUT WHY THERE ARE NAs

player_zone_gap <- rushing_summary_rank %>%
  group_by(player, player_id, team, season, qbgrp_ssn, rank_grp) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(zone_attempts + gap_attempts >= 3) %>%
  mutate(zone_perc = zone_attempts / (zone_attempts + gap_attempts))
# WEEK IS IN THERE BECAUSE OF NEED TO ADD IN SCRAMBLES 
# LET'S DO 3. FROM THERE --> IF ONLY 2 ATTEMPTS - LITERALLY ONLY
# 2 / 0, 1 / 1, 0 / 2 --> AND WE'LL JUST PUT IN SOME FAKE Z SCORE

player_zone_gap <- left_join(player_zone_gap, 
          combined_ids %>% ungroup() %>% dplyr::select(player_id, team, season, gsis_id) %>% distinct(),
          by = c("player_id", "team", "season")
          )

player_zone_gap_draft <- left_join(player_zone_gap,
          situation_cluster_df %>% dplyr::select(rusher_player_id:rank_grp, cluster),
          by = c("gsis_id" = "rusher_player_id", "team" = "posteam", "season" = "season", "rank_grp" = "rank_grp"))


# Check player_zone_gap duplicates
player_zone_gap %>%
  group_by(gsis_id, team, season) %>%
  filter(n() > 1) %>%
  arrange(gsis_id, season)

# Check situation_cluster_df duplicates  
situation_cluster_df %>%
  group_by(rusher_player_id, posteam, season, rank_grp) %>%
  filter(n() > 1) %>%
  arrange(rusher_player_id, season)
# SO WITH THE ABOVE ... SHIT. MIGHT HAVE TO GO "BACK TO THE PAST" TYPE SHIT


player_zone_gap_draft <- player_zone_gap_draft %>%
  mutate(attempts_calc = gap_attempts + zone_attempts)


player_zone_gap_draft %>%
  group_by(player, player_id, rank_grp, season) %>%
  dplyr::summarise(attempts = sum(gap_attempts, na.rm = T) + sum(zone_attempts, na.rm = T),
                    gap_perc = sum(gap_attempts, na.rm = T) / (sum(gap_attempts, na.rm = T) + sum(zone_attempts, na.rm = T))) %>%
  ungroup() %>%
  group_by(rank_grp) %>%
  dplyr::summarise(mn_gap = mean(gap_perc, na.rm = T),
                   sd_gap = sd(gap_perc, na.rm = T))
  

player_zone_gap_zscore <- player_zone_gap_draft %>%
  group_by(player, player_id, rank_grp, season) %>%
  summarise(
    attempts = sum(gap_attempts, na.rm = T) + sum(zone_attempts, na.rm = T),
    gap_attempts = sum(gap_attempts, na.rm = T),
    zone_attempts = sum(zone_attempts, na.rm = T),
    gap_perc = sum(gap_attempts, na.rm = T) / (sum(gap_attempts, na.rm = T) + sum(zone_attempts, na.rm = T)),
    .groups = "drop"
  ) %>%
  group_by(rank_grp) %>%
  mutate(
    mn_gap = mean(gap_perc, na.rm = T),
    sd_gap = sd(gap_perc, na.rm = T),
    gap_z = (gap_perc - mn_gap) / sd_gap
  ) %>%
  ungroup()

