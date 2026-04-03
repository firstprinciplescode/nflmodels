# Helper function to do the aggregation and ranking
create_rush_stats <- function(data, suffix = "") {
  
  # Aggregate to game level
  rush_game <- data %>%
    mutate(
      off_ssn = paste0(team, season),
      # Convert averages back to totals
      total_ybc = ybc * attempts,
      total_yac = yac * attempts
    ) %>%
    group_by(off_ssn, def_ssn, team, week, season) %>%
    summarise(
      attempts = sum(attempts, na.rm = TRUE),
      total_yards = sum(ypc * attempts, na.rm = TRUE),
      total_ybc = sum(total_ybc, na.rm = TRUE),
      total_yac = sum(total_yac, na.rm = TRUE),
      total_pbp_xypc = sum(pbp_xypc * attempts, na.rm = TRUE),
      total_part_xypc = sum(part_xypc * attempts, na.rm = TRUE),
      pbp_xtd = sum(pbp_xtd, na.rm = TRUE),
      part_xtd = sum(part_xtd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ypc = total_yards / attempts,
      ybc = total_ybc / attempts,
      yac = total_yac / attempts,
      pbp_xypc = total_pbp_xypc / attempts,
      part_xypc = ifelse(season == 2025, NA_real_, total_part_xypc / attempts),
      part_xtd = ifelse(season == 2025, NA_real_, part_xtd)
    ) %>%
    select(-starts_with("total_"))
  
  # Offensive ranks
  rush_game <- rush_game %>%
    group_by(off_ssn) %>%
    mutate(
      ypc_rank = (rank(ypc, ties.method = "average") - 1) / (n() - 1),
      ybc_rank = (rank(ybc, ties.method = "average") - 1) / (n() - 1),
      yac_rank = (rank(yac, ties.method = "average") - 1) / (n() - 1),
      pbp_xypc_rank = (rank(pbp_xypc, ties.method = "average") - 1) / (n() - 1),
      part_xypc_rank = (rank(part_xypc, ties.method = "average", na.last = "keep") - 1) / (sum(!is.na(part_xypc)) - 1),
      pbp_xtd_rank = (rank(pbp_xtd, ties.method = "average") - 1) / (n() - 1),
      part_xtd_rank = (rank(part_xtd, ties.method = "average", na.last = "keep") - 1) / (sum(!is.na(part_xtd)) - 1),
      attempts_rank = (rank(attempts, ties.method = "average") - 1) / (n() - 1)
    ) %>%
    ungroup() %>%
    # Defensive ranks
    group_by(def_ssn) %>%
    mutate(
      ypc_rank_def = (rank(ypc, ties.method = "average") - 1) / (n() - 1),
      ybc_rank_def = (rank(ybc, ties.method = "average") - 1) / (n() - 1),
      yac_rank_def = (rank(yac, ties.method = "average") - 1) / (n() - 1),
      pbp_xypc_rank_def = (rank(pbp_xypc, ties.method = "average") - 1) / (n() - 1),
      part_xypc_rank_def = (rank(part_xypc, ties.method = "average", na.last = "keep") - 1) / (sum(!is.na(part_xypc)) - 1),
      pbp_xtd_rank_def = (rank(pbp_xtd, ties.method = "average") - 1) / (n() - 1),
      part_xtd_rank_def = (rank(part_xtd, ties.method = "average", na.last = "keep") - 1) / (sum(!is.na(part_xtd)) - 1),
      attempts_rank_def = (rank(attempts, ties.method = "average") - 1) / (n() - 1)
    ) %>%
    ungroup()
  
  return(rush_game)
}

# High usage (rush_share >= 0.4)
rush_stats_high <- rush_stats_final %>%
  filter(rush_share >= 0.4) %>%
  create_rush_stats()

# Low usage (rush_share < 0.4)  
rush_stats_low <- rush_stats_final %>%
  filter(rush_share < 0.4, position_group == "HB") %>%
  create_rush_stats()

# Receivers
rush_stats_rec <- rush_stats_final %>%
  filter(position_group == "REC") %>%
  create_rush_stats()

View(rush_stats_high %>% 
       mutate(lane_ind = ifelse(week %in% c(1,2,4,5,6,7,8,10), 1, 0)) %>% 
       filter(off_ssn == "PHI2025") %>%
       group_by(lane_ind) %>%
       summarise(ypc_rank = mean(ypc_rank_def),
                ybc_rank = mean(ybc_rank_def),
                yac_rank = mean(yac_rank_def)))

View(rush_stats_high %>% filter(off_ssn == "NE2025"))
View(rush_stats_rec %>% filter(off_ssn == "NE2025"))

View(rush_stats_high %>% filter(def_ssn == "SEA2025"))
View(rush_stats_rec %>% filter(def_ssn == "SEA2025"))
