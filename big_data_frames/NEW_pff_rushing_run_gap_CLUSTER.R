pbp_rush_info$guard_ind

pbp_rush_info$tackle_ind

pbp_rush_info$end_ind

pbp_rush_info$outside_ind

pbp_rush_info$middle_ind


# 0 --> middle
# 1 --> guard
# 2 --> tackle
# 3 --> end


pbp_rush_info %>% 
  filter(rusher_player_name == "L.Jackson", posteam == "BLT", season == 2020) %>%
  select(rank_grp) %>% distinct()


sqldf("
  SELECT rusher_player_name, posteam, season,
         COUNT(DISTINCT CASE WHEN rank_grp = 'A' THEN game_id END) AS primary_games,
         COUNT(DISTINCT CASE WHEN rank_grp != 'A' THEN game_id END) AS non_primary_games,
         SUM(CASE WHEN rank_grp = 'A' THEN 1 ELSE 0 END) AS primary_total_rushes,
         SUM(CASE WHEN rank_grp != 'A' THEN 1 ELSE 0 END) AS non_primary_total_rushes,
         100.0 * SUM(CASE WHEN rank_grp = 'A' AND run_gap = 0 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp = 'A' THEN 1 ELSE 0 END) AS primary_center_perc,
         100.0 * SUM(CASE WHEN rank_grp = 'A' AND run_gap = 1 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp = 'A' THEN 1 ELSE 0 END) AS primary_guard_perc,
         100.0 * SUM(CASE WHEN rank_grp = 'A' AND run_gap = 2 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp = 'A' THEN 1 ELSE 0 END) AS primary_tackle_perc,
         100.0 * SUM(CASE WHEN rank_grp = 'A' AND run_gap = 3 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp = 'A' THEN 1 ELSE 0 END) AS primary_end_perc,
         100.0 * SUM(CASE WHEN rank_grp != 'A' AND run_gap = 0 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp != 'A' THEN 1 ELSE 0 END) AS non_primary_center_perc,
         100.0 * SUM(CASE WHEN rank_grp != 'A' AND run_gap = 1 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp != 'A' THEN 1 ELSE 0 END) AS non_primary_guard_perc,
         100.0 * SUM(CASE WHEN rank_grp != 'A' AND run_gap = 2 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp != 'A' THEN 1 ELSE 0 END) AS non_primary_tackle_perc,
         100.0 * SUM(CASE WHEN rank_grp != 'A' AND run_gap = 3 THEN 1 ELSE 0 END) / SUM(CASE WHEN rank_grp != 'A' THEN 1 ELSE 0 END) AS non_primary_end_perc
  FROM pbp_rush_info
  GROUP BY rusher_player_name, posteam, season
  HAVING primary_games >= 5 AND non_primary_games >= 5
")

# THANK GOD --> RANK_GRP DOESN'T MATTER


pbp_rush_info %>% group_by(rusher_player_name, rusher_player_id, posteam, season) %>%
  dplyr::summarise(n = n()) %>% ungroup() %>%
  summarise(percentile_15 = quantile(n, probs = 0.15, na.rm = TRUE)
            ,percentile_25 = quantile(n, probs = 0.25, na.rm = TRUE)
            ,percentile_35 = quantile(n, probs = 0.35, na.rm = TRUE)
            ,percentile_45 = quantile(n, probs = 0.45, na.rm = TRUE)
            ,percentile_55 = quantile(n, probs = 0.55, na.rm = TRUE)
            ,percentile_65 = quantile(n, probs = 0.65, na.rm = TRUE)
            ,percentile_75 = quantile(n, probs = 0.75, na.rm = TRUE)
            ,percentile_85 = quantile(n, probs = 0.85, na.rm = TRUE))   
# 6 --> 45TH PERCENTILE AND UP


base_run_gap_cluster <- pbp_rush_info %>%
  group_by(rusher_player_name, rusher_player_id, posteam, season) %>%
  summarise(
    total_rushes = n(),  # Total rushes for this combination
    center_perc = sum(run_gap == 0, na.rm = TRUE) / n(),
    guard_perc = sum(run_gap == 1, na.rm = TRUE) / n(),
    tackle_perc = sum(run_gap == 2, na.rm = TRUE) / n(),
    end_perc = sum(run_gap == 3, na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  filter(total_rushes >= 6)


base_run_gap_cluster_data <- base_run_gap_cluster %>% ungroup() %>%
  select(center_perc:end_perc)


k <- 9
kmeans_run_gap <- kmeans(base_run_gap_cluster_data, centers = k, nstart = 25)

kmeans_run_gap


# 2 - .444 - .444
# 3 - .648 - .204
# 4 - .717 - .069
# 5 - .766 - .049
# 6 - .8 - .034
# 7 - .821 - .021
# 8 - .835 - .014
# 9 - .848 - .013
# 10 - .86 - .012
# 11 - .87 - .01


kmeans_run_gap$cluster


post_base_run_gap_cluster <- cbind(base_run_gap_cluster, kmeans_run_gap$cluster)
colnames(post_base_run_gap_cluster)[10] <- "cluster"

post_base_run_gap_cluster %>% ungroup() %>% group_by(cluster) %>%
  dplyr::summarise(rushers = n(),
                   med_rushes = median(total_rushes),
                   center_perc = mean(center_perc),
                   guard_perc = mean(guard_perc),
                   tackle_perc = mean(tackle_perc),
                   end_perc = mean(end_perc)
  )


# 1 - B-SIDE END ONLY
# 2 - B-SIDE CENTER LEAN
# 3 - A-SIDE EVEN
# 4 - B-SIDE CENTER ONLY
# 5 - A-SIDE INSIDE LEAN
# 6 - A-SIDE GUARD / TACKLE
# 7 - B-SIDE CENTER GUARD
# 8 - A-SIDE OUTSIDE LEAN
# 9 - B-SIDE END LEAN

# A-SIDE IS PRIMARY / LIKE OBVIOUSLY THE PRIMARY. B-SIDE IS EVERYTHING ELSE


# SAVE TO WORKSPACE HERE