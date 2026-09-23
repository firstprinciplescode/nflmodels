

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate, dplyr::count)

'%ni%' <- Negate('%in%')

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  # Poll for completion
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  if (status != "SUCCEEDED") {
    # Get error message
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  # Get results location
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


coverage_scheme <- run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_scheme
")

coverage_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_summary
")

coverage_summary_by_game <- run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_summary_by_game
")

slot_coverage <- run_athena_query("
    SELECT  *
    FROM    nfl_data.slot_coverage
")

receiving_coverage_versus_raw <- run_athena_query("
    SELECT  *
    FROM    nfl_data.receiving_coverage_versus
")


coverage_intermediary_df <- 
  full_join(coverage_summary %>% select(-scraped_at), 
            coverage_scheme %>% select(-scraped_at), 
            by = c("team_name", "team", "position", "franchise_id", "player", "player_id", "week", "season", "draft_season", "jersey_number", "player_game_count", "eligible_season"))

colnames(coverage_intermediary_df)[76] <- 'declined_penalties_scheme'
colnames(coverage_intermediary_df)[72] <- 'penalties_cov_scheme'
colnames(coverage_intermediary_df)[27] <- 'declined_penalties'
colnames(coverage_intermediary_df)[22] <- 'penalties'


final_coverage_df <- full_join(coverage_intermediary_df, 
                               slot_coverage %>% select(-scraped_at), 
                               by = c("draft_season", "eligible_season", "franchise_id", "player", "player_id", "team", "team_name", "week", "season"))


# 1. drop “.x” from any name that ends with .x
colnames(final_coverage_df) <- sub("\\.x$", "", colnames(final_coverage_df))

# 2. replace “.y” at end with “_slot_cov”
colnames(final_coverage_df) <- sub("\\.y$", "_slot_cov", colnames(final_coverage_df))

colnames(final_coverage_df)[106] <- "coverage_snaps_slot"


colnames(coverage_summary_by_game)[6] <- "adv_position"
final_coverage_df <- left_join(final_coverage_df, 
                               coverage_summary_by_game %>% select(player_id, franchise_id, week, season, adv_position, scraped_at), 
                               by = c("player_id" = "player_id", "franchise_id" = "franchise_id", "week" = "week", "season" = "season"))


final_coverage_df %>% 
  #filter(snap_counts_pass_block >= 10, true_pass_set_non_spike_pass_block >= 5) %>%
  summarise(
    cov_snap = list(quantile(snap_counts_coverage, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    man_cov_snap = list(quantile(man_snap_counts_coverage, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    zone_cov_snap = list(quantile(zone_snap_counts_coverage, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    slot_cov_snap = list(quantile(coverage_snaps_slot, probs = seq(0, 1, 0.1), na.rm = TRUE))    
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))
# 20+ - ALL
# 7+ - MAN
# 12+ - ZONE
# 8+ - SLOT

final_coverage_df <- 
  final_coverage_df %>% filter(snap_counts_coverage >= 23 | man_snap_counts_coverage >= 8 | zone_snap_counts_coverage >= 13 | coverage_snaps_slot >= 8)

## POSITION WALL (2026-09-22): every season must get its position from coverage_summary_by_game.
## When Athena lost that table's 2023 partitions, adv_position came back NA for ALL of 2023, and
## old frames labeled the whole season "DE" (the cached frames still carried it on 2026-09-22).
## Normal seasons miss a position on ~0% of rows, so more than 2% of a season, or more than half
## of any one week (a lost playoff week is under 2% of a season), means the table or that week's
## scrape is missing. Stop here instead of building percentiles on weeks with no positions.
pos_wall_cov <- final_coverage_df %>%
  group_by(season, week) %>%
  summarise(rows = n(), no_position = sum(is.na(adv_position)), .groups = "drop") %>%
  group_by(season) %>%
  mutate(season_share = sum(no_position) / sum(rows)) %>%
  ungroup() %>%
  filter(season_share > 0.02 | no_position / rows > 0.5)
if (nrow(pos_wall_cov) > 0) {
  print(as.data.frame(pos_wall_cov %>% filter(no_position > 0)))
  stop("POSITION WALL: coverage_summary_by_game gave no position for the season-weeks above. ",
       "Athena probably can't see those partitions: run  MSCK REPAIR TABLE nfl_data.coverage_summary_by_game;  ",
       "in the Athena console (or run the dbt build), then source this file again. ",
       "If only the newest week is listed, that week's coverage-by-game scrape is missing.")
} else {
  cat("POSITION WALL OK: every season-week has its position from coverage_summary_by_game\n")
}


final_coverage_df <- final_coverage_df %>%
  mutate(
    final_position = case_when(
      adv_position == "MLB"                     ~ "MLB",
      str_detect(adv_position, "LB$")           ~ "LB",        # ends in “LB”
      adv_position %in% c("FS", "SS")           ~ "S",
      adv_position == "SCB"                     ~ "SCB",
      str_detect(adv_position, "CB")            ~ "CB",        # anywhere “CB”
      adv_position %in% c("DE","ED","DI","DT","NT","DLE","DRE","DLT","DRT") ~ "DL",
      TRUE                                       ~ NA         # everything else
    )
  )

final_coverage_df$avg_yac = final_coverage_df$yards_after_catch / final_coverage_df$receptions

final_coverage_df$man_qb_rating_against[which(final_coverage_df$man_targets == 0)] <- NA
final_coverage_df$zone_qb_rating_against[which(final_coverage_df$zone_targets == 0)] <- NA
final_coverage_df$man_yards_after_catch[which(final_coverage_df$man_targets == 0)] <- NA
final_coverage_df$man_avg_yac = final_coverage_df$man_yards_after_catch / final_coverage_df$man_receptions
final_coverage_df$man_yards_per_coverage_snap[which(final_coverage_df$man_targets == 0)] <- 0
final_coverage_df$zone_yards_after_catch[which(final_coverage_df$zone_targets == 0)] <- NA
final_coverage_df$zone_avg_yac = final_coverage_df$zone_yards_after_catch / final_coverage_df$zone_receptions
final_coverage_df$zone_yards_per_coverage_snap[which(final_coverage_df$zone_targets == 0)] <- 0

final_coverage_df$pass_break_up_rate = final_coverage_df$pass_break_ups / final_coverage_df$targets
final_coverage_df$man_pass_break_up_rate = final_coverage_df$man_pass_break_ups / final_coverage_df$man_targets
final_coverage_df$zone_pass_break_up_rate = final_coverage_df$zone_pass_break_ups / final_coverage_df$zone_targets

final_coverage_df$qb_rating_against_slot_cov[which(final_coverage_df$man_targets == 0)] <- NA
final_coverage_df$yards_after_catch_slot_cov[which(final_coverage_df$man_targets == 0)] <- NA
final_coverage_df$slot_avg_yac = final_coverage_df$yards_after_catch_slot_cov / final_coverage_df$receptions_slot_cov
final_coverage_df$yards_per_coverage_snap_slot_cov[which(final_coverage_df$targets_slot_cov == 0)] <- 0


final_coverage_df$team[which(final_coverage_df$team == "ARI")] = "ARZ"
final_coverage_df$team[which(final_coverage_df$team == "BAL")] = "BLT"
final_coverage_df$team[which(final_coverage_df$team == "CLE")] = "CLV"
final_coverage_df$team[which(final_coverage_df$team == "HOU")] = "HST"
final_coverage_df$team[which(final_coverage_df$team == "LAC" & final_coverage_df$season == 2016)] = "SD"
final_coverage_df$team[which(final_coverage_df$team == "LV" & final_coverage_df$season <= 2019)] = "OAK"

final_coverage_df$team_name[which(final_coverage_df$team_name == "ARI")] = "ARZ"
final_coverage_df$team_name[which(final_coverage_df$team_name == "BAL")] = "BLT"
final_coverage_df$team_name[which(final_coverage_df$team_name == "CLE")] = "CLV"
final_coverage_df$team_name[which(final_coverage_df$team_name == "HOU")] = "HST"
final_coverage_df$team_name[which(final_coverage_df$team_name == "LAC" & final_coverage_df$season == 2016)] = "SD"
final_coverage_df$team_name[which(final_coverage_df$team_name == "LV" & final_coverage_df$season <= 2019)] = "OAK"


final_coverage_df_qbgrp <- 
  left_join(final_coverage_df %>% select(-scraped_at),
            combined_grade_epa_summary %>% select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc, def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "opp", 
                   "week" = "week", 
                   "season" = "season"))


final_coverage_df_qbgrp <- left_join(final_coverage_df_qbgrp,
                                       play_counts %>% select(player_id, week, season, game_id, position),
                                       by = c("player_id", "week", "season"))


final_coverage_df_qbgrp <- final_coverage_df_qbgrp %>%
  mutate(pass_break_up_rate      = ifelse(targets      > 0, pass_break_ups      / targets,      NA_real_),
         man_pass_break_up_rate  = ifelse(man_targets  > 0, man_pass_break_ups  / man_targets,  NA_real_),
         zone_pass_break_up_rate = ifelse(zone_targets > 0, zone_pass_break_ups / zone_targets, NA_real_))

colnames(final_coverage_df_qbgrp)

final_coverage_df_qbgrp %>%
  filter(snap_counts_coverage >= 23) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 6 - FOR THE GENERAL STATS

final_coverage_df_qbgrp %>%
  filter(man_snap_counts_coverage >= 8) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 5 FOR THE MAN STATS

final_coverage_df_qbgrp %>%
  filter(zone_snap_counts_coverage >= 13) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 5+ 

final_coverage_df_qbgrp %>%
  filter(coverage_snaps_slot >= 8) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 4+ 

# SNAPS / GAMES
# COMBINED: 23 / 6
# MAN: 8 / 5
# ZONE: 13 / 6
# SLOT: 8 / 4


# ============================================
# 1. COMBINED COVERAGE
# ============================================
coverage_combined_player_agg <- final_coverage_df_qbgrp %>%
  filter(snap_counts_coverage >= 23) %>%
  group_by(player, player_id, final_position, qbgrp_ssn, season) %>%
  mutate(
    # Higher is better
    player_grade_cov_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_coverage_defense)),
    player_cov_snaps_per_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_target)),
    player_cov_snaps_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_reception)),
    player_pass_break_up_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pass_break_up_rate)),
    # Lower is better (negate)
    player_catch_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-catch_rate)),
    player_yards_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_reception)),
    player_yards_per_cov_snap_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_coverage_snap)),
    player_avg_yac_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_yac)),
    player_qb_rating_against_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-qb_rating_against)),
    player_missed_tackle_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-missed_tackle_rate)),
    player_avg_depth_of_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_depth_of_target))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, final_position) %>%
  mutate(
    # Higher is better
    grade_cov_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_coverage_defense)),
    cov_snaps_per_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_target)),
    cov_snaps_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_reception)),
    pass_break_up_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pass_break_up_rate)),
    # Lower is better (negate)
    catch_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-catch_rate)),
    yards_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_reception)),
    yards_per_cov_snap_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_coverage_snap)),
    avg_yac_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_yac)),
    qb_rating_against_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-qb_rating_against)),
    missed_tackle_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-missed_tackle_rate)),
    avg_depth_of_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_depth_of_target)),
    n_combined = n()
  ) %>%
  filter(n_combined >= 6) %>%
  ungroup()

valid_qbgrp_cov <- coverage_combined_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

coverage_combined_opp_percentile <- coverage_combined_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_cov) %>%
  group_by(qbgrp_ssn, final_position) %>%
  summarise(
    grade_cov_perc = mean(grade_cov_def_ssn_perc, na.rm = TRUE),
    cov_snaps_per_target_perc = mean(cov_snaps_per_target_def_ssn_perc, na.rm = TRUE),
    cov_snaps_per_rec_perc = mean(cov_snaps_per_rec_def_ssn_perc, na.rm = TRUE),
    pass_break_up_rate_perc = mean(pass_break_up_rate_def_ssn_perc, na.rm = TRUE),
    catch_rate_perc = mean(catch_rate_def_ssn_perc, na.rm = TRUE),
    yards_per_rec_perc = mean(yards_per_rec_def_ssn_perc, na.rm = TRUE),
    yards_per_cov_snap_perc = mean(yards_per_cov_snap_def_ssn_perc, na.rm = TRUE),
    avg_yac_perc = mean(avg_yac_def_ssn_perc, na.rm = TRUE),
    qb_rating_against_perc = mean(qb_rating_against_def_ssn_perc, na.rm = TRUE),
    missed_tackle_rate_perc = mean(missed_tackle_rate_def_ssn_perc, na.rm = TRUE),
    avg_depth_of_target_perc = mean(avg_depth_of_target_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = final_position,
    values_from = c(grade_cov_perc, cov_snaps_per_target_perc, cov_snaps_per_rec_perc, 
                    pass_break_up_rate_perc, catch_rate_perc, yards_per_rec_perc,
                    yards_per_cov_snap_perc, avg_yac_perc, qb_rating_against_perc,
                    missed_tackle_rate_perc, avg_depth_of_target_perc)
  )

# Player season summary
coverage_combined_player_season_summary <- final_coverage_df_qbgrp %>%
  filter(snap_counts_coverage >= 23) %>%
  group_by(player, player_id, final_position, def_ssn, season) %>%
  summarise(
    player_grade_cov = mean(grades_coverage_defense, na.rm = TRUE),
    player_cov_snaps_per_target = mean(coverage_snaps_per_target, na.rm = TRUE),
    player_cov_snaps_per_rec = mean(coverage_snaps_per_reception, na.rm = TRUE),
    player_pass_break_up_rate = mean(pass_break_up_rate, na.rm = TRUE),
    player_catch_rate = mean(catch_rate, na.rm = TRUE),
    player_yards_per_rec = mean(yards_per_reception, na.rm = TRUE),
    player_yards_per_cov_snap = mean(yards_per_coverage_snap, na.rm = TRUE),
    player_avg_yac = mean(avg_yac, na.rm = TRUE),
    player_qb_rating_against = mean(qb_rating_against, na.rm = TRUE),
    player_missed_tackle_rate = mean(missed_tackle_rate, na.rm = TRUE),
    player_avg_depth_of_target = mean(avg_depth_of_target, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%
  group_by(final_position, season) %>%
  mutate(
    grade_cov_season_pctl = percent_rank(player_grade_cov),
    cov_snaps_per_target_season_pctl = percent_rank(player_cov_snaps_per_target),
    cov_snaps_per_rec_season_pctl = percent_rank(player_cov_snaps_per_rec),
    pass_break_up_rate_season_pctl = percent_rank(player_pass_break_up_rate),
    catch_rate_season_pctl = percent_rank(-player_catch_rate),
    yards_per_rec_season_pctl = percent_rank(-player_yards_per_rec),
    yards_per_cov_snap_season_pctl = percent_rank(-player_yards_per_cov_snap),
    avg_yac_season_pctl = percent_rank(-player_avg_yac),
    qb_rating_against_season_pctl = percent_rank(-player_qb_rating_against),
    missed_tackle_rate_season_pctl = percent_rank(-player_missed_tackle_rate),
    avg_depth_of_target_season_pctl = percent_rank(-player_avg_depth_of_target)
  ) %>%
  ungroup()

# ============================================
# 2. MAN COVERAGE
# ============================================
coverage_man_player_agg <- final_coverage_df_qbgrp %>%
  filter(man_snap_counts_coverage >= 7) %>%
  group_by(player, player_id, final_position, qbgrp_ssn, season) %>%
  mutate(
    player_man_grade_cov_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_grades_coverage_defense)),
    player_man_cov_snaps_per_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_coverage_snaps_per_target)),
    player_man_cov_snaps_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_coverage_snaps_per_reception)),
    player_man_pass_break_up_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_pass_break_up_rate)),
    player_man_catch_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_catch_rate)),
    player_man_yards_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_yards_per_reception)),
    player_man_yards_per_cov_snap_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_yards_per_coverage_snap)),
    player_man_avg_yac_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_avg_yac)),
    player_man_qb_rating_against_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_qb_rating_against)),
    player_man_missed_tackle_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_missed_tackle_rate)),
    player_man_avg_depth_of_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_avg_depth_of_target))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, final_position) %>%
  mutate(
    man_grade_cov_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_grades_coverage_defense)),
    man_cov_snaps_per_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_coverage_snaps_per_target)),
    man_cov_snaps_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_coverage_snaps_per_reception)),
    man_pass_break_up_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(man_pass_break_up_rate)),
    man_catch_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_catch_rate)),
    man_yards_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_yards_per_reception)),
    man_yards_per_cov_snap_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_yards_per_coverage_snap)),
    man_avg_yac_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_avg_yac)),
    man_qb_rating_against_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_qb_rating_against)),
    man_missed_tackle_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_missed_tackle_rate)),
    man_avg_depth_of_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-man_avg_depth_of_target)),
    n_man = n()
  ) %>%
  filter(n_man >= 6) %>%
  ungroup()

valid_qbgrp_man <- coverage_man_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

coverage_man_opp_percentile <- coverage_man_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_man) %>%
  group_by(qbgrp_ssn, final_position) %>%
  summarise(
    man_grade_cov_perc = mean(man_grade_cov_def_ssn_perc, na.rm = TRUE),
    man_cov_snaps_per_target_perc = mean(man_cov_snaps_per_target_def_ssn_perc, na.rm = TRUE),
    man_cov_snaps_per_rec_perc = mean(man_cov_snaps_per_rec_def_ssn_perc, na.rm = TRUE),
    man_pass_break_up_rate_perc = mean(man_pass_break_up_rate_def_ssn_perc, na.rm = TRUE),
    man_catch_rate_perc = mean(man_catch_rate_def_ssn_perc, na.rm = TRUE),
    man_yards_per_rec_perc = mean(man_yards_per_rec_def_ssn_perc, na.rm = TRUE),
    man_yards_per_cov_snap_perc = mean(man_yards_per_cov_snap_def_ssn_perc, na.rm = TRUE),
    man_avg_yac_perc = mean(man_avg_yac_def_ssn_perc, na.rm = TRUE),
    man_qb_rating_against_perc = mean(man_qb_rating_against_def_ssn_perc, na.rm = TRUE),
    man_missed_tackle_rate_perc = mean(man_missed_tackle_rate_def_ssn_perc, na.rm = TRUE),
    man_avg_depth_of_target_perc = mean(man_avg_depth_of_target_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = final_position,
    values_from = c(man_grade_cov_perc, man_cov_snaps_per_target_perc, man_cov_snaps_per_rec_perc,
                    man_pass_break_up_rate_perc, man_catch_rate_perc, man_yards_per_rec_perc,
                    man_yards_per_cov_snap_perc, man_avg_yac_perc, man_qb_rating_against_perc,
                    man_missed_tackle_rate_perc, man_avg_depth_of_target_perc)
  )

coverage_man_player_season_summary <- final_coverage_df_qbgrp %>%
  filter(man_snap_counts_coverage >= 7) %>%
  group_by(player, player_id, final_position, def_ssn, season) %>%
  summarise(
    player_man_grade_cov = mean(man_grades_coverage_defense, na.rm = TRUE),
    player_man_cov_snaps_per_target = mean(man_coverage_snaps_per_target, na.rm = TRUE),
    player_man_cov_snaps_per_rec = mean(man_coverage_snaps_per_reception, na.rm = TRUE),
    player_man_pass_break_up_rate = mean(man_pass_break_up_rate, na.rm = TRUE),
    player_man_catch_rate = mean(man_catch_rate, na.rm = TRUE),
    player_man_yards_per_rec = mean(man_yards_per_reception, na.rm = TRUE),
    player_man_yards_per_cov_snap = mean(man_yards_per_coverage_snap, na.rm = TRUE),
    player_man_avg_yac = mean(man_avg_yac, na.rm = TRUE),
    player_man_qb_rating_against = mean(man_qb_rating_against, na.rm = TRUE),
    player_man_missed_tackle_rate = mean(man_missed_tackle_rate, na.rm = TRUE),
    player_man_avg_depth_of_target = mean(man_avg_depth_of_target, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  group_by(final_position, season) %>%
  mutate(
    man_grade_cov_season_pctl = percent_rank(player_man_grade_cov),
    man_cov_snaps_per_target_season_pctl = percent_rank(player_man_cov_snaps_per_target),
    man_cov_snaps_per_rec_season_pctl = percent_rank(player_man_cov_snaps_per_rec),
    man_pass_break_up_rate_season_pctl = percent_rank(player_man_pass_break_up_rate),
    man_catch_rate_season_pctl = percent_rank(-player_man_catch_rate),
    man_yards_per_rec_season_pctl = percent_rank(-player_man_yards_per_rec),
    man_yards_per_cov_snap_season_pctl = percent_rank(-player_man_yards_per_cov_snap),
    man_avg_yac_season_pctl = percent_rank(-player_man_avg_yac),
    man_qb_rating_against_season_pctl = percent_rank(-player_man_qb_rating_against),
    man_missed_tackle_rate_season_pctl = percent_rank(-player_man_missed_tackle_rate),
    man_avg_depth_of_target_season_pctl = percent_rank(-player_man_avg_depth_of_target)
  ) %>%
  ungroup()

# ============================================
# 3. ZONE COVERAGE
# ============================================
coverage_zone_player_agg <- final_coverage_df_qbgrp %>%
  filter(zone_snap_counts_coverage >= 13) %>%
  group_by(player, player_id, final_position, qbgrp_ssn, season) %>%
  mutate(
    player_zone_grade_cov_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_grades_coverage_defense)),
    player_zone_cov_snaps_per_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_coverage_snaps_per_target)),
    player_zone_cov_snaps_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_coverage_snaps_per_reception)),
    player_zone_pass_break_up_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_pass_break_up_rate)),
    player_zone_catch_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_catch_rate)),
    player_zone_yards_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_yards_per_reception)),
    player_zone_yards_per_cov_snap_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_yards_per_coverage_snap)),
    player_zone_avg_yac_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_avg_yac)),
    player_zone_qb_rating_against_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_qb_rating_against)),
    player_zone_missed_tackle_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_missed_tackle_rate)),
    player_zone_avg_depth_of_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_avg_depth_of_target))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, final_position) %>%
  mutate(
    zone_grade_cov_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_grades_coverage_defense)),
    zone_cov_snaps_per_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_coverage_snaps_per_target)),
    zone_cov_snaps_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_coverage_snaps_per_reception)),
    zone_pass_break_up_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(zone_pass_break_up_rate)),
    zone_catch_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_catch_rate)),
    zone_yards_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_yards_per_reception)),
    zone_yards_per_cov_snap_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_yards_per_coverage_snap)),
    zone_avg_yac_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_avg_yac)),
    zone_qb_rating_against_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_qb_rating_against)),
    zone_missed_tackle_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_missed_tackle_rate)),
    zone_avg_depth_of_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-zone_avg_depth_of_target)),
    n_zone = n()
  ) %>%
  filter(n_zone >= 6) %>%
  ungroup()

valid_qbgrp_zone <- coverage_zone_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

coverage_zone_opp_percentile <- coverage_zone_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_zone) %>%
  group_by(qbgrp_ssn, final_position) %>%
  summarise(
    zone_grade_cov_perc = mean(zone_grade_cov_def_ssn_perc, na.rm = TRUE),
    zone_cov_snaps_per_target_perc = mean(zone_cov_snaps_per_target_def_ssn_perc, na.rm = TRUE),
    zone_cov_snaps_per_rec_perc = mean(zone_cov_snaps_per_rec_def_ssn_perc, na.rm = TRUE),
    zone_pass_break_up_rate_perc = mean(zone_pass_break_up_rate_def_ssn_perc, na.rm = TRUE),
    zone_catch_rate_perc = mean(zone_catch_rate_def_ssn_perc, na.rm = TRUE),
    zone_yards_per_rec_perc = mean(zone_yards_per_rec_def_ssn_perc, na.rm = TRUE),
    zone_yards_per_cov_snap_perc = mean(zone_yards_per_cov_snap_def_ssn_perc, na.rm = TRUE),
    zone_avg_yac_perc = mean(zone_avg_yac_def_ssn_perc, na.rm = TRUE),
    zone_qb_rating_against_perc = mean(zone_qb_rating_against_def_ssn_perc, na.rm = TRUE),
    zone_missed_tackle_rate_perc = mean(zone_missed_tackle_rate_def_ssn_perc, na.rm = TRUE),
    zone_avg_depth_of_target_perc = mean(zone_avg_depth_of_target_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = final_position,
    values_from = c(zone_grade_cov_perc, zone_cov_snaps_per_target_perc, zone_cov_snaps_per_rec_perc,
                    zone_pass_break_up_rate_perc, zone_catch_rate_perc, zone_yards_per_rec_perc,
                    zone_yards_per_cov_snap_perc, zone_avg_yac_perc, zone_qb_rating_against_perc,
                    zone_missed_tackle_rate_perc, zone_avg_depth_of_target_perc)
  )

coverage_zone_player_season_summary <- final_coverage_df_qbgrp %>%
  filter(zone_snap_counts_coverage >= 13) %>%
  group_by(player, player_id, final_position, def_ssn, season) %>%
  summarise(
    player_zone_grade_cov = mean(zone_grades_coverage_defense, na.rm = TRUE),
    player_zone_cov_snaps_per_target = mean(zone_coverage_snaps_per_target, na.rm = TRUE),
    player_zone_cov_snaps_per_rec = mean(zone_coverage_snaps_per_reception, na.rm = TRUE),
    player_zone_pass_break_up_rate = mean(zone_pass_break_up_rate, na.rm = TRUE),
    player_zone_catch_rate = mean(zone_catch_rate, na.rm = TRUE),
    player_zone_yards_per_rec = mean(zone_yards_per_reception, na.rm = TRUE),
    player_zone_yards_per_cov_snap = mean(zone_yards_per_coverage_snap, na.rm = TRUE),
    player_zone_avg_yac = mean(zone_avg_yac, na.rm = TRUE),
    player_zone_qb_rating_against = mean(zone_qb_rating_against, na.rm = TRUE),
    player_zone_missed_tackle_rate = mean(zone_missed_tackle_rate, na.rm = TRUE),
    player_zone_avg_depth_of_target = mean(zone_avg_depth_of_target, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  group_by(final_position, season) %>%
  mutate(
    zone_grade_cov_season_pctl = percent_rank(player_zone_grade_cov),
    zone_cov_snaps_per_target_season_pctl = percent_rank(player_zone_cov_snaps_per_target),
    zone_cov_snaps_per_rec_season_pctl = percent_rank(player_zone_cov_snaps_per_rec),
    zone_pass_break_up_rate_season_pctl = percent_rank(player_zone_pass_break_up_rate),
    zone_catch_rate_season_pctl = percent_rank(-player_zone_catch_rate),
    zone_yards_per_rec_season_pctl = percent_rank(-player_zone_yards_per_rec),
    zone_yards_per_cov_snap_season_pctl = percent_rank(-player_zone_yards_per_cov_snap),
    zone_avg_yac_season_pctl = percent_rank(-player_zone_avg_yac),
    zone_qb_rating_against_season_pctl = percent_rank(-player_zone_qb_rating_against),
    zone_missed_tackle_rate_season_pctl = percent_rank(-player_zone_missed_tackle_rate),
    zone_avg_depth_of_target_season_pctl = percent_rank(-player_zone_avg_depth_of_target)
  ) %>%
  ungroup()

# ============================================
# 4. SLOT COVERAGE
# ============================================
coverage_slot_player_agg <- final_coverage_df_qbgrp %>%
  filter(coverage_snaps_slot >= 8) %>%
  group_by(player, player_id, final_position, qbgrp_ssn, season) %>%
  mutate(
    player_slot_cov_snaps_per_target_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_target_slot_cov)),
    player_slot_cov_snaps_per_rec_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_reception_slot_cov)),
    player_slot_yards_per_cov_snap_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_coverage_snap_slot_cov)),
    player_slot_avg_yac_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-slot_avg_yac)),
    player_slot_qb_rating_against_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-qb_rating_against_slot_cov))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, final_position) %>%
  mutate(
    slot_cov_snaps_per_target_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_target_slot_cov)),
    slot_cov_snaps_per_rec_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(coverage_snaps_per_reception_slot_cov)),
    slot_yards_per_cov_snap_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-yards_per_coverage_snap_slot_cov)),
    slot_avg_yac_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-slot_avg_yac)),
    slot_qb_rating_against_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-qb_rating_against_slot_cov)),
    n_slot = n()
  ) %>%
  filter(n_slot >= 5) %>%
  ungroup()

valid_qbgrp_slot <- coverage_slot_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

coverage_slot_opp_percentile <- coverage_slot_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_slot) %>%
  group_by(qbgrp_ssn, final_position) %>%
  summarise(
    slot_cov_snaps_per_target_perc = mean(slot_cov_snaps_per_target_def_ssn_perc, na.rm = TRUE),
    slot_cov_snaps_per_rec_perc = mean(slot_cov_snaps_per_rec_def_ssn_perc, na.rm = TRUE),
    slot_yards_per_cov_snap_perc = mean(slot_yards_per_cov_snap_def_ssn_perc, na.rm = TRUE),
    slot_avg_yac_perc = mean(slot_avg_yac_def_ssn_perc, na.rm = TRUE),
    slot_qb_rating_against_perc = mean(slot_qb_rating_against_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = final_position,
    values_from = c(slot_cov_snaps_per_target_perc, slot_cov_snaps_per_rec_perc,
                    slot_yards_per_cov_snap_perc, slot_avg_yac_perc,
                    slot_qb_rating_against_perc)
  )

coverage_slot_player_season_summary <- final_coverage_df_qbgrp %>%
  filter(coverage_snaps_slot >= 8) %>%
  group_by(player, player_id, final_position, def_ssn, season) %>%
  summarise(
    player_slot_cov_snaps_per_target = mean(coverage_snaps_per_target_slot_cov, na.rm = TRUE),
    player_slot_cov_snaps_per_rec = mean(coverage_snaps_per_reception_slot_cov, na.rm = TRUE),
    player_slot_yards_per_cov_snap = mean(yards_per_coverage_snap_slot_cov, na.rm = TRUE),
    player_slot_avg_yac = mean(slot_avg_yac, na.rm = TRUE),
    player_slot_qb_rating_against = mean(qb_rating_against_slot_cov, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  group_by(final_position, season) %>%
  mutate(
    slot_cov_snaps_per_target_season_pctl = percent_rank(player_slot_cov_snaps_per_target),
    slot_cov_snaps_per_rec_season_pctl = percent_rank(player_slot_cov_snaps_per_rec),
    slot_yards_per_cov_snap_season_pctl = percent_rank(-player_slot_yards_per_cov_snap),
    slot_avg_yac_season_pctl = percent_rank(-player_slot_avg_yac),
    slot_qb_rating_against_season_pctl = percent_rank(-player_slot_qb_rating_against),
  ) %>%
  ungroup()


receiving_coverage_defense_helper <- final_coverage_df_qbgrp %>%
  select(player_id, week, season, team_name, def_ssn, qbgrp_ssn,
         final_position, snap_counts_coverage,
         man_snap_counts_coverage, zone_snap_counts_coverage, coverage_snaps_slot) %>%
  distinct()

receiving_coverage_rec_helper <- receiving_func_base %>%
  select(player_id, week, season, posteam, qbgrp_ssn, def_ssn, pos_rank, team_rank, final_position_group, align_cluster_name, rte_cluster_name, tgt_cluster_name, man_zone_grp_cluster, z_score_percentile, xpass_percentile, td_grp_cluster, xtd_percentile) %>%
  distinct()

# joins rebuild from RAW every run -- rerunning this block any number of
# times produces the identical result; stacking is impossible by construction
receiving_coverage_versus <- receiving_coverage_versus_raw %>%
  left_join(receiving_coverage_rec_helper,
            by = c("player_id", "week", "season")) %>%
  left_join(receiving_coverage_defense_helper,
            by = c("coverage_player_id" = "player_id", "week", "season",
                   "qbgrp_ssn", "def_ssn"))
sfx <- grep("\\.x$|\\.y$", names(receiving_coverage_versus), value = TRUE)
if (length(sfx)) { print(sfx); stop("these columns doubled -- collision, not rerun damage") }
stopifnot(!any(endsWith(names(receiving_coverage_versus), ".y")))

add_pctl_buckets <- function(df,
                             cols = c("z_score_percentile",
                                      "xpass_percentile",
                                      "xtd_percentile")) {
  for (col in cols) {
    if (col %in% names(df)) {
      new_col <- sub("_percentile$", "_qrtl", col)
      df[[new_col]] <- cut(df[[col]],
                           breaks         = c(-Inf, 25, 50, 75, Inf),
                           labels         = c("Q1 (≤25)", "Q2 (26-50)",
                                              "Q3 (51-75)", "Q4 (>75)"),
                           include.lowest = TRUE)
    }
  }
  df
}
receiving_coverage_versus <- add_pctl_buckets(receiving_coverage_versus)


View(coverage_man_player_season_summary %>% filter(def_ssn == "DEN2025"))
View(coverage_zone_player_season_summary %>% filter(def_ssn == "DEN2025"))
View(coverage_combined_player_season_summary %>% filter(def_ssn == "NE2025"))

View(coverage_man_player_season_summary %>% filter(player_id == 10698))
View(coverage_zone_player_season_summary %>% filter(player_id == 10698))

# 61853 57662 61853


receiving_func_base %>% 
  filter(def_ssn == 'NE2025')


put_object(
  file = "~/pff_blocking_defense_workspace_AWS.RData",
  object = "temp/pff_blocking_defense_workspace_AWS.RData",
  bucket = "nfl-pff-data-lucas"
)

passer_rating <- function(att, comp, yds, td, int) {
  att_safe <- ifelse(att > 0, att, NA_real_)
  a <- pmin(pmax(((comp / att_safe) - 0.3) * 5,    0), 2.375)
  b <- pmin(pmax(((yds  / att_safe) - 3)   * 0.25, 0), 2.375)
  c <- pmin(pmax((td    / att_safe) * 20,          0), 2.375)
  d <- pmin(pmax(2.375 - ((int / att_safe) * 25),  0), 2.375)
  ((a + b + c + d) / 6) * 100
}

# ─────────────────────────────────────────────────────────────────────────────
# Valid `dim` values for coverage_archetype_profile():
#
#   Receiver archetype clusters (categorical — best for slicing by role/style):
#     rte_cluster_name      — route-running archetype  [DEFAULT]
#                              (e.g. DT, MT, BT, SMT, ML, ST, RB)
#     tgt_cluster_name      — target-profile archetype
#                              (e.g. G, SMT, MT, BT, ML)
#     align_cluster_name    — pre-snap alignment archetype
#                              (e.g. STE, ITE, WWR, RB)
#     man_zone_grp_cluster  — position × depth × man/zone production class
#                              (WR_DEEP / WR_SHORT / WR_LT,
#                               TE_DEEP / TE_SHORT / TE_LT,
#                               HB_DEEP / HB_SHORT / HB_LT, OTH)
#     td_grp_cluster        — TD-production cluster
#
#   Depth-chart rank (numeric — pair with position_group_filter to avoid
#   mixing WR1 / TE1 / RB1 into the same bucket):
#     pos_rank              — depth rank within position group
#                              (1 = top guy at position, 2 = #2, ...)
#     team_rank             — overall team rank across receivers
#
#   Receiver-quality quartile buckets (categorical — derived once via
#   `add_pctl_buckets(receiving_coverage_versus)`. Use `order_by = "arch"`
#   in plot calls so Q1 → Q4 reads in natural order):
#     z_score_qrtl          — man-vs-zone production tendency
#                              (Q1 = zone-heavy, Q4 = man-heavy)
#     xpass_qrtl            — expected pass-game value drawn
#                              (Q1 = low-leverage, Q4 = top targets)
#     xtd_qrtl              — expected-TD profile
#                              (Q1 = low-TD risk, Q4 = red-zone threats)
# ─────────────────────────────────────────────────────────────────────────────

coverage_archetype_profile <- function(defender_id,
                                       dim                   = "rte_cluster_name",
                                       df                    = receiving_coverage_versus,
                                       def_ssn_filter        = NULL,
                                       position_group_filter = "all") {
  
  d <- df %>% filter(!is.na(.data[[dim]]))
  if (!is.null(def_ssn_filter))                d <- d %>% filter(def_ssn == def_ssn_filter)
  if (!identical(position_group_filter, "all")) {
    d <- d %>% filter(final_position_group %in% position_group_filter)
  }
  
  team_totals <- d %>%
    group_by(def_ssn, season, arch = .data[[dim]]) %>%
    summarise(team_targets    = sum(targets,          na.rm = TRUE),
              team_receptions = sum(receptions,       na.rm = TRUE),
              team_yards      = sum(yards,            na.rm = TRUE),
              team_tds        = sum(touchdowns,       na.rm = TRUE),
              team_ints       = sum(interceptions,    na.rm = TRUE),
              .groups = "drop")
  
  focal <- d %>%
    filter(coverage_player_id == defender_id) %>%
    group_by(def_ssn, season, arch = .data[[dim]]) %>%
    summarise(targets    = sum(targets,          na.rm = TRUE),
              receptions = sum(receptions,       na.rm = TRUE),
              yards      = sum(yards,            na.rm = TRUE),
              tds        = sum(touchdowns,       na.rm = TRUE),
              pbu        = sum(broken_up_passes, na.rm = TRUE),
              ints       = sum(interceptions,    na.rm = TRUE),
              .groups = "drop")
  
  focal %>%
    left_join(team_totals, by = c("def_ssn", "season", "arch")) %>%
    mutate(
      rest_targets        = team_targets    - targets,
      rest_receptions     = team_receptions - receptions,
      rest_yards          = team_yards      - yards,
      rest_tds            = team_tds        - tds,
      rest_ints           = team_ints       - ints,
      
      catch_rate                = receptions / targets,
      ypt                       = yards / targets,
      passer_rating_vs          = passer_rating(targets, receptions, yards, tds, ints),
      
      teammate_catch_rate       = ifelse(rest_targets > 0, rest_receptions / rest_targets, NA_real_),
      teammate_ypt              = ifelse(rest_targets > 0, rest_yards      / rest_targets, NA_real_),
      teammate_passer_rating    = ifelse(rest_targets > 0,
                                         passer_rating(rest_targets, rest_receptions,
                                                       rest_yards, rest_tds, rest_ints),
                                         NA_real_),
      
      catch_rate_oe             = catch_rate       - teammate_catch_rate,
      ypt_oe                    = ypt              - teammate_ypt,
      passer_rating_oe          = passer_rating_vs - teammate_passer_rating
    ) %>%
    group_by(def_ssn) %>%
    mutate(tgt_share = targets / sum(targets)) %>%
    ungroup() %>%
    arrange(def_ssn, desc(tgt_share)) %>%
    select(def_ssn, season, arch,
           targets, receptions, yards, tds, pbu, ints,
           tgt_share,
           catch_rate, ypt, passer_rating_vs,
           teammate_catch_rate, teammate_ypt, teammate_passer_rating,
           rest_targets,
           catch_rate_oe, ypt_oe, passer_rating_oe)
}


coverage_archetype_profile(55088, dim = "man_zone_grp_cluster", def_ssn_filter = "SEA2025")
coverage_archetype_profile(55088, dim = "pos_rank", def_ssn_filter = "SEA2025")
coverage_archetype_profile(55088, dim = "rte_cluster_name", def_ssn_filter = "SEA2025")
coverage_archetype_profile(55088, dim = "tgt_cluster_name", def_ssn_filter = "SEA2025")

# receiver baseline: how each receiver does across ALL coverage, per season
receiving_coverage_rec_stats_base <- receiving_coverage_versus %>%
  group_by(player_id, season) %>%
  summarise(rec_targets    = sum(targets, na.rm = TRUE),
            rec_catch_rate = sum(receptions, na.rm = TRUE) / sum(targets, na.rm = TRUE),
            rec_ypt        = sum(yards,      na.rm = TRUE) / sum(targets, na.rm = TRUE),
            .groups = "drop") %>%
  filter(rec_targets >= 15)

# defender effect = actual allowed minus what those receivers normally do
# defender effect = actual allowed minus what those receivers normally do
receiving_coverage_defender_base <- receiving_coverage_versus %>%
  inner_join(receiving_coverage_rec_stats_base, by = c("player_id", "season")) %>%
  # GATE: sub-floor weeks carry NA final_position (helper join whiffed the snap floor) -
  # drop them entirely so their targets never pool. Snap-based gate by design; n_tgt
  # will read lower than PFF site season totals.
  filter(!is.na(final_position)) %>%
  # CONSOLIDATE: minority labels from cleared weeks (WLB/MLB oscillation) resolve to
  # the player-season mode so one player can't fragment into two percentile rows.
  group_by(coverage_player_id, season) %>%
  mutate(final_position = names(which.max(table(final_position)))) %>%
  ungroup() %>%
  group_by(coverage_player_id, final_position, def_ssn, season) %>%
  summarise(
    n_tgt              = sum(targets, na.rm = TRUE),
    catch_rate_actual  = sum(receptions, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    catch_rate_exp     = weighted.mean(rec_catch_rate, targets, na.rm = TRUE),
    ypt_actual         = sum(yards, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    ypt_exp            = weighted.mean(rec_ypt, targets, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(catch_rate_vs_exp = catch_rate_actual - catch_rate_exp,   # neg = held below their norm = good
         ypt_vs_exp         = ypt_actual - ypt_exp) %>%
  filter(n_tgt >= 10) %>%
  group_by(final_position, season) %>%
  mutate(cr_pctl  = percent_rank(-catch_rate_vs_exp),              # lower allowed = higher pctl
         ypt_pctl = percent_rank(-ypt_vs_exp)) %>%
  ungroup()

# tripwire: no player-season may carry two labels (would fragment percentile rows)
stopifnot(
  receiving_coverage_defender_base %>%
    group_by(coverage_player_id, season) %>%
    filter(n_distinct(final_position) > 1) %>%
    nrow() == 0
)

receiving_coverage_defender_final <- receiving_coverage_defender_base %>% 
  filter(!is.na(final_position))

src <- readLines("C:/Users/vflre/Downloads/nflmodels_UPDATE/pff_stats/secondary/pff_pass_coverage_AWS.R")
c(raw_pull    = any(grepl("receiving_coverage_versus_raw <- run_athena_query", src, fixed = TRUE)),
  helper_full = any(grepl("posteam, qbgrp_ssn, def_ssn, pos_rank", src, fixed = TRUE)),
  rebuild     = any(grepl("receiving_coverage_versus <- receiving_coverage_versus_raw", src, fixed = TRUE)),
  print_wall  = any(grepl("these columns doubled", src, fixed = TRUE)),
  canary      = any(grepl("coverage_player_id == 61790", src, fixed = TRUE)))



plot_coverage_card_player <- function(player_id_vec,
                                      seasons = NULL,
                                      family  = c("man", "zone", "slot"),
                                      df_man  = coverage_man_player_season_summary,
                                      df_zone = coverage_zone_player_season_summary,
                                      df_slot = coverage_slot_player_season_summary) {
  
  family <- match.arg(family)
  
  family_spec <- list(
    man = list(
      df      = df_man,
      prefix  = "man_",
      metrics = c("grade_cov", "cov_snaps_per_target", "cov_snaps_per_rec",
                  "pass_break_up_rate", "catch_rate", "yards_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against",
                  "missed_tackle_rate", "avg_depth_of_target"),
      labels  = c("Grade", "Snap/Tgt", "Snap/Rec", "PBU %", "Catch %",
                  "Y/Rec", "Y/CovSnp", "YAC", "QBR", "MT %", "aDOT")
    ),
    zone = list(
      df      = df_zone,
      prefix  = "zone_",
      metrics = c("grade_cov", "cov_snaps_per_target", "cov_snaps_per_rec",
                  "pass_break_up_rate", "catch_rate", "yards_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against",
                  "missed_tackle_rate", "avg_depth_of_target"),
      labels  = c("Grade", "Snap/Tgt", "Snap/Rec", "PBU %", "Catch %",
                  "Y/Rec", "Y/CovSnp", "YAC", "QBR", "MT %", "aDOT")
    ),
    slot = list(
      df      = df_slot,
      prefix  = "slot_",
      metrics = c("cov_snaps_per_target", "cov_snaps_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against"),
      labels  = c("Snap/Tgt", "Snap/Rec", "Y/CovSnp", "YAC", "QBR")
    )
  )
  
  spec <- family_spec[[family]]
  df   <- spec$df %>% filter(player_id %in% player_id_vec)
  if (!is.null(seasons)) df <- df %>% filter(season %in% seasons)
  
  if (nrow(df) == 0) {
    player_name <- NULL
    for (fam in c("man", "zone", "slot")) {
      nm <- family_spec[[fam]]$df %>%
        filter(player_id %in% player_id_vec) %>%
        pull(player) %>% unique()
      if (length(nm) > 0) { player_name <- nm[1]; break }
    }
    
    available <- character(0)
    for (fam in c("man", "zone", "slot")) {
      d_check <- family_spec[[fam]]$df %>% filter(player_id %in% player_id_vec)
      if (!is.null(seasons)) d_check <- d_check %>% filter(season %in% seasons)
      if (nrow(d_check) > 0) available <- c(available, fam)
    }
    
    who <- if (!is.null(player_name)) {
      paste0(player_name, " (", paste(player_id_vec, collapse = ", "), ")")
    } else {
      paste("player_id", paste(player_id_vec, collapse = ", "))
    }
    when <- if (!is.null(seasons)) paste0(" in ", paste(seasons, collapse = ", ")) else ""
    
    msg <- sprintf("No %s coverage rows for %s%s.", family, who, when)
    if (length(available) > 0) {
      msg <- paste0(msg, " Try family = ", paste(shQuote(available), collapse = " or "), ".")
    } else {
      msg <- paste0(msg, " Player isn't in any coverage family at the current snap thresholds.")
    }
    
    message(msg)
    return(invisible(NULL))
  }
  
  metrics       <- spec$metrics
  metric_labels <- spec$labels
  pctl_cols     <- paste0(spec$prefix, metrics, "_season_pctl")
  
  d_full <- df %>%
    arrange(desc(player), season) %>%
    mutate(row_label = paste0(player, " — ", def_ssn, " (", n, " gms)"),
           row_label = factor(row_label, levels = unique(row_label))) %>%
    select(row_label, final_position, n, all_of(pctl_cols)) %>%
    rename_with(~ paste0(metrics, "_season_pctl"), all_of(pctl_cols)) %>%
    pivot_longer(ends_with("_season_pctl"),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric         = sub("_season_pctl$", "", metric),
           metric         = factor(metric, levels = metrics, labels = metric_labels),
           final_position = factor(final_position, levels = c("CB","SCB","S","LB","MLB")))
  
  build_panel <- function(pos, show_x_strip, show_x_axis) {
    d <- d_full %>% filter(final_position == pos)
    if (nrow(d) == 0) return(NULL)
    d <- d %>% mutate(row_label = droplevels(row_label))
    
    ggplot(d, aes(x = pctl, y = row_label, fill = pctl)) +
      geom_col(width = 0.75) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40", linewidth = 0.3) +
      scale_fill_gradient(low = "#deebf7", high = "#08306b",
                          limits = c(0, 1),
                          labels = scales::percent_format(accuracy = 1),
                          name = "Pctl") +
      scale_x_continuous(limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = scales::percent_format(accuracy = 1)) +
      facet_grid(. ~ metric, scales = "free_y") +
      labs(title = pos, x = NULL, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(plot.title         = element_text(face = "bold", size = 14, hjust = 0),
            panel.grid.major.y = element_blank(),
            strip.text.x       = if (show_x_strip) element_text(face = "bold", size = 8) else element_blank(),
            strip.background   = element_rect(fill = "grey88", color = NA),
            axis.text.x        = if (show_x_axis)  element_text(size = 6) else element_blank(),
            plot.margin        = margin(t = 10, b = 5))
  }
  
  pos_order <- intersect(c("CB","SCB","S","LB","MLB"),
                         as.character(unique(d_full$final_position)))
  
  panels  <- list()
  heights <- c()
  for (i in seq_along(pos_order)) {
    p <- pos_order[i]
    panels[[i]] <- build_panel(p,
                               show_x_strip = (i == 1),
                               show_x_axis  = (i == length(pos_order)))
    heights[i]  <- d_full %>% filter(final_position == p) %>% distinct(row_label) %>% nrow()
  }
  panels <- panels[!vapply(panels, is.null, logical(1))]
  
  player_names <- unique(df$player)
  family_label <- switch(family,
                         man  = "Man Coverage",
                         zone = "Zone Coverage",
                         slot = "Slot Coverage")
  title_str <- if (length(player_names) == 1) {
    paste0(player_names, " — ", family_label, " Scouting Card")
  } else {
    paste0(family_label, " Scouting Card")
  }
  
  library(patchwork)
  result <- if (length(panels) == 1) {
    panels[[1]]
  } else {
    Reduce(`/`, panels) + plot_layout(heights = heights, guides = "collect")
  }
  
  result + plot_annotation(
    title    = title_str,
    subtitle = "Each row = one player-season  |  Dark = elite season percentile",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10))
  )
}

plot_coverage_card_player(9994, family = "man")
plot_coverage_card_player(9994, family = "zone")
# plot_coverage_card_player(61853, family = "slot")


plot_coverage_archetype <- function(profile_df,
                                    player_name    = NULL,
                                    dim_label      = "Archetype",
                                    focal_color    = "#0076B6",
                                    teammate_color = "grey75",
                                    min_targets    = 1,
                                    order_by       = c("tgt_share", "arch")) {
  
  order_by <- match.arg(order_by)
  
  if (is.null(profile_df) || nrow(profile_df) == 0) {
    message("No rows to plot.")
    return(invisible(NULL))
  }
  
  d <- profile_df %>% filter(targets >= min_targets)
  if (nrow(d) == 0) {
    message("All archetypes filtered out by min_targets = ", min_targets, ".")
    return(invisible(NULL))
  }
  
  d <- if (order_by == "tgt_share") {
    d %>% arrange(desc(tgt_share))
  } else {
    d %>% arrange(arch)
  }
  
  d <- d %>%
    mutate(arch_lbl = paste0(arch, "  (n=", targets, " | r=", rest_targets, ")"),
           arch_lbl = factor(arch_lbl, levels = rev(arch_lbl)))
  
  p_diet <- ggplot(d, aes(x = tgt_share, y = arch_lbl)) +
    geom_col(fill = focal_color, width = 0.7) +
    geom_text(aes(label = scales::percent(tgt_share, accuracy = 1)),
              hjust = -0.15, size = 3.2, color = "grey25") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       expand  = expansion(mult = c(0, 0.18))) +
    labs(title = "Target Diet", x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title         = element_text(face = "bold", size = 11),
          axis.text.x        = element_text(size = 8))
  
  kpi <- d %>%
    transmute(
      arch_lbl,
      `Catch %_Focal`       = catch_rate          * 100,
      `Catch %_Teammate`    = teammate_catch_rate * 100,
      `Y/Tgt_Focal`         = ypt,
      `Y/Tgt_Teammate`      = teammate_ypt,
      `QB Rating_Focal`     = passer_rating_vs,
      `QB Rating_Teammate`  = teammate_passer_rating
    ) %>%
    pivot_longer(cols = -arch_lbl,
                 names_to  = c("metric", "side"),
                 names_sep = "_",
                 values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("Catch %", "Y/Tgt", "QB Rating")),
           side   = factor(side,   levels = c("Teammate", "Focal")))
  
  p_kpi <- ggplot(kpi, aes(x = value, y = arch_lbl, fill = side)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    facet_grid(. ~ metric, scales = "free_x") +
    scale_fill_manual(values = c("Focal" = focal_color, "Teammate" = teammate_color),
                      breaks = c("Focal", "Teammate"),
                      name   = NULL) +
    labs(title = "Performance vs Own-def_ssn Teammates", x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title         = element_text(face = "bold", size = 11),
          legend.position    = "bottom",
          strip.text         = element_text(face = "bold", size = 9),
          strip.background   = element_rect(fill = "grey92", color = NA),
          axis.text.x        = element_text(size = 8))
  
  def_ssns  <- unique(d$def_ssn)
  scope     <- if (length(def_ssns) == 1) paste0(" — ", def_ssns) else ""
  title_str <- if (!is.null(player_name)) {
    paste0(player_name, " — Coverage Archetype Profile", scope)
  } else {
    paste0("Coverage Archetype Profile", scope)
  }
  
  library(patchwork)
  (p_diet / p_kpi) +
    plot_layout(heights = c(1, 1.6)) +
    plot_annotation(
      title    = title_str,
      subtitle = paste0("Sliced by ", dim_label,
                        "  |  n = focal targets, r = teammate targets  |  ",
                        "shorter Focal bar than Teammate = tighter coverage"),
      theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                       plot.subtitle = element_text(size = 9, color = "grey30"))
    )
}
# rte_cluster_name (defaults are fine — order by tgt_share desc)

prof <- coverage_archetype_profile(55088,
                                   dim            = "tgt_cluster_name",
                                   def_ssn_filter = "SEA2025")
plot_coverage_archetype(prof,
                        player_name = "Kevin Byard",
                        dim_label   = "TGT Cluster Name")

# pos_rank, WR-filtered (sort ascending so 1→5 reads naturally)
prof <- coverage_archetype_profile(10698,
                                   dim                   = "tgt_cluster_name",
                                   def_ssn_filter        = "CHI2025")
plot_coverage_archetype(prof,
                        player_name = "Kevin Byard",
                        dim_label   = "Tgt Cluster",
                        order_by    = "arch")


build_defender_diet_matrix <- function(
    cov_versus        = receiving_coverage_versus,
    cov_df            = final_coverage_df_qbgrp,
    dims              = c("final_position_group",
                          "pos_rank",
                          "rte_cluster_name",
                          "tgt_cluster_name",
                          "align_cluster_name",
                          "td_grp_cluster",
                          "man_zone_grp_cluster",
                          "z_score_qrtl",
                          "xpass_qrtl",
                          "xtd_qrtl"),
    min_total_targets = 20,
    min_games         = 6
) {
  
  prefix_map <- c(
    final_position_group = "pgrp",
    pos_rank             = "posR",
    rte_cluster_name     = "rte",
    tgt_cluster_name     = "tgt",
    align_cluster_name   = "algn",
    td_grp_cluster       = "tdg",
    man_zone_grp_cluster = "mz",
    z_score_qrtl         = "zsc",
    xpass_qrtl           = "xpa",
    xtd_qrtl             = "xtd"
  )
  
  defender_totals <- cov_versus %>%
    filter(!is.na(coverage_player_id)) %>%
    group_by(coverage_player_id, def_ssn, season) %>%
    summarise(total_targets = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    filter(total_targets >= min_total_targets)
  
  build_diet_wide <- function(dim_col) {
    pfx <- prefix_map[[dim_col]]; if (is.null(pfx)) pfx <- dim_col
    
    cov_versus %>%
      filter(!is.na(.data[[dim_col]]), !is.na(coverage_player_id)) %>%
      mutate(arch = sub("\\s.*$", "", as.character(.data[[dim_col]]))) %>%
      group_by(coverage_player_id, def_ssn, season, arch) %>%
      summarise(arch_targets = sum(targets, na.rm = TRUE), .groups = "drop") %>%
      group_by(coverage_player_id, def_ssn, season) %>%
      mutate(arch_share = arch_targets / sum(arch_targets)) %>%
      ungroup() %>%
      mutate(arch = paste0(pfx, "_", arch)) %>%
      select(coverage_player_id, def_ssn, season, arch, arch_share) %>%
      pivot_wider(names_from = arch, values_from = arch_share, values_fill = 0)
  }
  
  diet_wide <- Reduce(
    function(x, y) full_join(x, y, by = c("coverage_player_id", "def_ssn", "season")),
    lapply(dims, build_diet_wide)
  )
  
  deployment <- cov_df %>%
    filter(!is.na(player_id)) %>%
    rename(coverage_player_id = player_id) %>%
    group_by(coverage_player_id, def_ssn, season, final_position, team_name) %>%
    summarise(
      total_cov_snaps = sum(snap_counts_coverage,      na.rm = TRUE),
      man_snaps       = sum(man_snap_counts_coverage,  na.rm = TRUE),
      zone_snaps      = sum(zone_snap_counts_coverage, na.rm = TRUE),
      slot_snaps      = sum(coverage_snaps_slot,       na.rm = TRUE),
      n_games         = n(),
      .groups = "drop"
    ) %>%
    filter(n_games >= min_games) %>%
    mutate(
      man_pct  = man_snaps  / pmax(total_cov_snaps, 1),
      zone_pct = zone_snaps / pmax(total_cov_snaps, 1),
      slot_pct = slot_snaps / pmax(total_cov_snaps, 1)
    )
  
  ids <- cov_df %>%
    select(player_id, player) %>%
    distinct() %>%
    group_by(player_id) %>% slice(1) %>% ungroup() %>%
    rename(coverage_player_id = player_id)
  
  used_prefixes <- unname(prefix_map[dims])
  pat           <- paste0("^(", paste(used_prefixes, collapse = "|"), ")_")
  diet_cols     <- names(diet_wide)[grepl(pat, names(diet_wide))]
  
  deployment %>%
    semi_join(defender_totals, by = c("coverage_player_id", "def_ssn", "season")) %>%
    left_join(ids,              by = "coverage_player_id") %>%
    left_join(defender_totals,  by = c("coverage_player_id", "def_ssn", "season")) %>%
    left_join(diet_wide,        by = c("coverage_player_id", "def_ssn", "season")) %>%
    mutate(across(all_of(diet_cols), ~ replace_na(.x, 0))) %>%
    select(coverage_player_id, player, def_ssn, season, final_position, team_name,
           n_games, total_cov_snaps, total_targets,
           man_snaps, zone_snaps, slot_snaps,
           man_pct, zone_pct, slot_pct,
           everything())
}


find_similar_defenders <- function(defender_id,
                                   target_def_ssn,
                                   matrix_df,
                                   same_position  = TRUE,
                                   n_top          = NULL,
                                   min_similarity = NULL,
                                   weights        = c(
                                     man_pct  = 1.75,
                                     zone_pct = 1.75,
                                     slot_pct = 1.5,
                                     pgrp = 3,
                                     posR = 3,
                                     rte  = .75,
                                     tgt  = 1,
                                     algn = 1,
                                     tdg  = 0.75,
                                     mz   = 0.75,
                                     zsc  = 1,
                                     xpa  = 1,
                                     xtd  = 1
                                   )) {
  
  # ─────────────────────────────────────────────────────────────────────────
  # n_top + min_similarity behavior:
  #
  #   call                                                  →  result
  #   ───────────────────────────────────────────────────────────────────────
  #   find_similar_defenders(id, ssn, m)                    →  top 15
  #   find_similar_defenders(..., n_top = 30)               →  top 30
  #   find_similar_defenders(..., min_similarity = 0.95)    →  every row at 0.95+
  #   find_similar_defenders(..., min_similarity = 0.95,
  #                          n_top = 100)                   →  up to 100 at 0.95+
  #   find_similar_defenders(..., min_similarity = 0.95,
  #                          n_top = 30)                    →  up to 30 at 0.95+
  #   find_similar_defenders(..., n_top = Inf)              →  every comp, no threshold
  # ─────────────────────────────────────────────────────────────────────────
  
  deploy_cols <- c("man_pct", "zone_pct", "slot_pct")
  diet_cols   <- names(matrix_df)[grepl("^(pgrp|posR|rte|tgt|algn|tdg|mz|zsc|xpa|xtd)_",
                                        names(matrix_df))]
  feat_cols   <- c(deploy_cols, diet_cols)
  
  col_weights <- vapply(feat_cols, function(c) {
    if (c %in% names(weights)) return(unname(weights[c]))
    pfx <- sub("_.*$", "", c)
    if (pfx %in% names(weights)) unname(weights[pfx]) else 1
  }, numeric(1))
  
  target_row <- matrix_df %>%
    filter(coverage_player_id == defender_id, def_ssn == target_def_ssn)
  if (nrow(target_row) == 0) {
    message("Defender ", defender_id, " in ", target_def_ssn, " not in matrix.")
    return(invisible(NULL))
  }
  
  pool <- if (same_position) {
    matrix_df %>% filter(final_position == target_row$final_position[1])
  } else {
    matrix_df
  }
  pool <- pool %>% filter(!(coverage_player_id == defender_id & def_ssn == target_def_ssn))
  
  target_vec <- as.numeric(target_row[1, feat_cols]) * col_weights
  pool_mat   <- sweep(as.matrix(pool[, feat_cols]), 2, col_weights, `*`)
  
  a_norm <- sqrt(sum(target_vec^2))
  b_norm <- sqrt(rowSums(pool_mat^2))
  pool$similarity <- as.numeric(pool_mat %*% target_vec) / (b_norm * a_norm + 1e-9)
  
  result <- pool %>%
    arrange(desc(similarity)) %>%
    select(coverage_player_id, player, final_position,
           def_ssn, season, team_name,
           total_targets, total_cov_snaps, similarity)
  
  if (!is.null(min_similarity)) {
    result <- result %>% filter(similarity >= min_similarity)
  }
  
  effective_n_top <- if (!is.null(n_top)) {
    n_top
  } else if (is.null(min_similarity)) {
    15
  } else {
    Inf
  }
  
  result %>% head(effective_n_top)
}

defender_diet_matrix <- build_defender_diet_matrix()

comps <- find_similar_defenders(41662, "BLT2025", defender_diet_matrix, min_similarity = .96)
nrow(comps)

inspect_defender_comps <- function(defender_id,
                                   target_def_ssn,
                                   matrix_df,
                                   n_top          = 15,
                                   min_similarity = NULL,
                                   same_position  = TRUE,
                                   weights        = NULL) {
  
  # get comps (pass weights through only if user provided them; otherwise let
  # find_similar_defenders use its own defaults)
  comp_args <- list(defender_id    = defender_id,
                    target_def_ssn = target_def_ssn,
                    matrix_df      = matrix_df,
                    n_top          = n_top,
                    min_similarity = min_similarity,
                    same_position  = same_position)
  if (!is.null(weights)) comp_args$weights <- weights
  comps <- do.call(find_similar_defenders, comp_args)
  
  if (is.null(comps) || nrow(comps) == 0) return(invisible(NULL))
  
  focal_row <- matrix_df %>%
    filter(coverage_player_id == defender_id,
           def_ssn == .env$target_def_ssn) %>%
    mutate(similarity = 1.0)
  
  comp_rows <- comps %>%
    select(coverage_player_id, def_ssn, similarity) %>%
    left_join(matrix_df, by = c("coverage_player_id", "def_ssn"))
  
  bind_rows(focal_row, comp_rows) %>%
    arrange(desc(similarity))
}

inspect_defender_comps(40588, "KC2023", defender_diet_matrix, min_similarity = .965, n_top = 50) %>% View()


compare_comps_performance <- function(comps_df,
                                      focal_id,
                                      focal_def_ssn,
                                      family  = c("zone", "man", "slot"),
                                      df_man  = coverage_man_player_season_summary,
                                      df_zone = coverage_zone_player_season_summary,
                                      df_slot = coverage_slot_player_season_summary) {
  
  family  <- match.arg(family)
  perf_df <- switch(family,
                    man  = df_man,
                    zone = df_zone,
                    slot = df_slot)
  
  keys <- comps_df %>%
    select(coverage_player_id, def_ssn) %>%
    bind_rows(tibble(coverage_player_id = focal_id, def_ssn = focal_def_ssn)) %>%
    distinct() %>%
    rename(player_id = coverage_player_id)
  
  sim_lookup <- comps_df %>%
    select(coverage_player_id, def_ssn, similarity) %>%
    rename(player_id = coverage_player_id) %>%
    bind_rows(tibble(player_id  = focal_id,
                     def_ssn    = focal_def_ssn,
                     similarity = 1.0)) %>%
    distinct(player_id, def_ssn, .keep_all = TRUE)
  
  perf_df %>%
    semi_join(keys,        by = c("player_id", "def_ssn")) %>%
    left_join(sim_lookup,  by = c("player_id", "def_ssn")) %>%
    arrange(desc(similarity)) %>%
    select(player_id, player, final_position, def_ssn, season, similarity, n,
           ends_with("_season_pctl"))
}


cohort_perf <- compare_comps_performance(comps, 41662, "BLT2025", family = "man")

# where does Reed land in the cohort distribution on each metric?
focal_row <- cohort_perf %>% filter(player_id == 41662, def_ssn == "BLT2025")
pctl_cols <- names(cohort_perf)[grepl("_season_pctl$", names(cohort_perf))]

vapply(pctl_cols, function(col) {
  mean(cohort_perf[[col]] < focal_row[[col]], na.rm = TRUE)
}, numeric(1)) %>% sort(decreasing = TRUE)


cohort_perf <- compare_comps_performance(comps, 41662, "BLT2025", family = "zone")

# where does Reed land in the cohort distribution on each metric?
focal_row <- cohort_perf %>% filter(player_id == 41662, def_ssn == "BLT2025")
pctl_cols <- names(cohort_perf)[grepl("_season_pctl$", names(cohort_perf))]

vapply(pctl_cols, function(col) {
  mean(cohort_perf[[col]] < focal_row[[col]], na.rm = TRUE)
}, numeric(1)) %>% sort(decreasing = TRUE)


plot_comps_dots <- function(comps_df,
                            focal_id,
                            focal_def_ssn,
                            family  = c("zone", "man", "slot"),
                            df_man  = coverage_man_player_season_summary,
                            df_zone = coverage_zone_player_season_summary,
                            df_slot = coverage_slot_player_season_summary) {
  
  family <- match.arg(family)
  
  family_spec <- list(
    man = list(
      prefix  = "man_",
      metrics = c("grade_cov", "cov_snaps_per_target", "cov_snaps_per_rec",
                  "pass_break_up_rate", "catch_rate", "yards_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against",
                  "missed_tackle_rate", "avg_depth_of_target"),
      labels  = c("Grade", "Snap/Tgt", "Snap/Rec", "PBU %", "Catch %",
                  "Y/Rec", "Y/CovSnp", "YAC", "QBR", "MT %", "aDOT")
    ),
    zone = list(
      prefix  = "zone_",
      metrics = c("grade_cov", "cov_snaps_per_target", "cov_snaps_per_rec",
                  "pass_break_up_rate", "catch_rate", "yards_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against",
                  "missed_tackle_rate", "avg_depth_of_target"),
      labels  = c("Grade", "Snap/Tgt", "Snap/Rec", "PBU %", "Catch %",
                  "Y/Rec", "Y/CovSnp", "YAC", "QBR", "MT %", "aDOT")
    ),
    slot = list(
      prefix  = "slot_",
      metrics = c("cov_snaps_per_target", "cov_snaps_per_rec",
                  "yards_per_cov_snap", "avg_yac", "qb_rating_against"),
      labels  = c("Snap/Tgt", "Snap/Rec", "Y/CovSnp", "YAC", "QBR")
    )
  )
  
  spec          <- family_spec[[family]]
  metrics       <- spec$metrics
  metric_labels <- spec$labels
  pctl_cols     <- paste0(spec$prefix, metrics, "_season_pctl")
  
  perf <- compare_comps_performance(comps_df, focal_id, focal_def_ssn, family,
                                    df_man = df_man, df_zone = df_zone, df_slot = df_slot)
  
  d <- perf %>%
    mutate(is_focal = (player_id == focal_id & def_ssn == focal_def_ssn))
  
  long <- d %>%
    select(player, def_ssn, is_focal, all_of(pctl_cols)) %>%
    pivot_longer(cols = all_of(pctl_cols),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric = sub("_season_pctl$", "", metric),
           metric = sub(paste0("^", spec$prefix), "", metric),
           metric = factor(metric, levels = metrics, labels = metric_labels))
  
  family_label <- switch(family,
                         man  = "Man Coverage",
                         zone = "Zone Coverage",
                         slot = "Slot Coverage")
  focal_name <- d %>% filter(is_focal) %>% pull(player)  %>% .[1]
  focal_def  <- d %>% filter(is_focal) %>% pull(def_ssn) %>% .[1]
  
  ggplot(long, aes(x = pctl, y = metric)) +
    # comp cohort as grey dots, jittered vertically so they don't stack
    geom_point(data = long %>% filter(!is_focal),
               position = position_jitter(width = 0, height = 0.25, seed = 42),
               color = "grey55", alpha = 0.45, size = 2.2) +
    # focal as bigger red dot on top
    geom_point(data = long %>% filter(is_focal),
               color = "firebrick2", size = 4.5, stroke = 1) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40", linewidth = 0.3) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.25),
                       labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_discrete(limits = rev(metric_labels)) +
    labs(
      title    = paste0(focal_name, " — ", focal_def, " vs Comp Cohort"),
      subtitle = paste0(family_label,
                        "  |  grey = comp cohort, red = focal  |  higher pctl = better"),
      x = "Season Percentile (within position, within season)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 9, color = "grey30"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 10)
    )
}

comps <- find_similar_defenders(41662, "BLT2025", defender_diet_matrix,
                                min_similarity = 0.96)

plot_comps_dots(comps, 41662, "BLT2025", family = "zone")
plot_comps_dots(comps, 41662, "BLT2025", family = "man")
plot_comps_dots(comps, 61853, "NE2025", family = "slot")



#####
#####
#####



DEF_FAMILY_SPEC <- list(
  man = list(
    prefix  = "man_",
    metrics = c("grade_cov","cov_snaps_per_target","cov_snaps_per_rec",
                "pass_break_up_rate","catch_rate","yards_per_rec",
                "yards_per_cov_snap","avg_yac","qb_rating_against",
                "missed_tackle_rate","avg_depth_of_target"),
    labels  = c("Grade","Snap/Tgt","Snap/Rec","PBU %","Catch %",
                "Y/Rec","Y/CovSnp","YAC","QBR","MT %","aDOT")
  ),
  zone = list(
    prefix  = "zone_",
    metrics = c("grade_cov","cov_snaps_per_target","cov_snaps_per_rec",
                "pass_break_up_rate","catch_rate","yards_per_rec",
                "yards_per_cov_snap","avg_yac","qb_rating_against",
                "missed_tackle_rate","avg_depth_of_target"),
    labels  = c("Grade","Snap/Tgt","Snap/Rec","PBU %","Catch %",
                "Y/Rec","Y/CovSnp","YAC","QBR","MT %","aDOT")
  ),
  slot = list(
    prefix  = "slot_",
    metrics = c("cov_snaps_per_target","cov_snaps_per_rec",
                "yards_per_cov_snap","avg_yac","qb_rating_against"),
    labels  = c("Snap/Tgt","Snap/Rec","Y/CovSnp","YAC","QBR")
  )
)

defender_comp_card <- function(comps_df, focal_id, focal_def_ssn,
                               family      = c("zone","man","slot"),
                               qual_adj_df = receiving_coverage_defender_final ,
                               df_man      = coverage_man_player_season_summary,
                               df_zone     = coverage_zone_player_season_summary,
                               df_slot     = coverage_slot_player_season_summary) {
  
  family <- match.arg(family)
  spec   <- DEF_FAMILY_SPEC[[family]]
  
  perf <- compare_comps_performance(comps_df, focal_id, focal_def_ssn, family,
                                    df_man = df_man, df_zone = df_zone, df_slot = df_slot)
  
  focal <- perf %>% filter(player_id == focal_id, def_ssn == focal_def_ssn)
  comp  <- perf %>% filter(!(player_id == focal_id & def_ssn == focal_def_ssn))
  if (nrow(focal) == 0) { message("focal not in ", family, " summary at current thresholds"); return(invisible(NULL)) }
  
  pctl_cols <- paste0(spec$prefix, spec$metrics, "_season_pctl")
  
  card <- tibble(
    metric      = spec$metrics,
    label       = spec$labels,
    league_pctl = vapply(pctl_cols, function(cc) focal[[cc]][1], numeric(1)),
    cohort_pctl = vapply(pctl_cols, function(cc) {
      x <- comp[[cc]]; v <- focal[[cc]][1]
      if (is.na(v) || sum(!is.na(x)) == 0) NA_real_ else mean(x <= v, na.rm = TRUE)
    }, numeric(1))
  )
  
  # receiver-quality-adjusted rows: allowed vs what those receivers normally do (lower = better)
  keys <- bind_rows(comps_df %>% select(coverage_player_id, def_ssn),
                    tibble(coverage_player_id = focal_id, def_ssn = focal_def_ssn)) %>% distinct()
  qa       <- qual_adj_df %>% semi_join(keys, by = c("coverage_player_id", "def_ssn"))
  qa_focal <- qa %>% filter(coverage_player_id == focal_id, def_ssn == focal_def_ssn)
  qa_comp  <- qa %>% filter(!(coverage_player_id == focal_id & def_ssn == focal_def_ssn))
  
  if (nrow(qa_focal) == 1) {
    card <- bind_rows(card, tibble(
      metric      = c("catch_rate_vs_exp", "ypt_vs_exp"),
      label       = c("Catch% vs Exp*", "Y/Tgt vs Exp*"),
      league_pctl = c(qa_focal$cr_pctl[1], qa_focal$ypt_pctl[1]),
      cohort_pctl = c(mean(qa_comp$catch_rate_vs_exp >= qa_focal$catch_rate_vs_exp[1], na.rm = TRUE),
                      mean(qa_comp$ypt_vs_exp        >= qa_focal$ypt_vs_exp[1],        na.rm = TRUE))
    ))
  }
  
  list(card       = card,
       focal_name = focal$player[1],
       focal_def  = focal_def_ssn,
       family     = family,
       n_cohort   = nrow(comp),
       n_qa       = nrow(qa_comp))
}

plot_defender_comp_card <- function(card_obj, title = NULL) {
  if (is.null(card_obj)) return(invisible(NULL))
  
  pd <- card_obj$card %>%
    mutate(label = factor(label, levels = rev(unique(label)))) %>%
    pivot_longer(c(league_pctl, cohort_pctl), names_to = "scope", values_to = "pctl") %>%
    mutate(scope = factor(if_else(scope == "league_pctl", "League", "Comps"),
                          levels = c("League", "Comps"))) %>%
    filter(!is.na(pctl))
  
  fam_lbl <- switch(card_obj$family, man = "Man", zone = "Zone", slot = "Slot")
  
  ggplot(pd, aes(scope, label, fill = pctl)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = scales::percent(pctl, accuracy = 1),
                  color = abs(pctl - 0.5) > 0.25),
              size = 4.2, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(title    = title %||% paste0(card_obj$focal_name, " — ", card_obj$focal_def,
                                      " — ", fam_lbl, " Coverage Card"),
         subtitle = paste0("League = pctl within position-season  |  Comps = pctl within ",
                           card_obj$n_cohort, " similarity-matched defenders  |  ",
                           "* = adjusted for receiver quality faced (n=", card_obj$n_qa, ")"),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(face = "bold", size = 16),
          plot.subtitle   = element_text(color = "grey40", size = 9),
          panel.grid      = element_blank(),
          axis.text.x.top = element_text(face = "bold", size = 12),
          axis.text.y     = element_text(size = 11, face = "bold"),
          legend.key.height = unit(1.4, "cm"))
}

comps <- find_similar_defenders(41662, "BLT2025", defender_diet_matrix, min_similarity = 0.965)

player_zone_card <- defender_comp_card(comps, 41662, "BLT2025", family = "zone")
plot_defender_comp_card(player_zone_card)

player_man_card <- defender_comp_card(comps, 41662, "BLT2025", family = "man")
plot_defender_comp_card(player_man_card)



#####
#####
#####

#### COVERAGE EVAL LAYER ####

# metric dictionary: label + question-group + direction + raw column suffix
COV_METRIC_DICT <- tibble::tribble(
  ~metric,                ~label,      ~group,     ~higher_better, ~raw_suffix,
  "grade_cov",            "Grade",     "Grade",    TRUE,           "grades_coverage_defense",
  "cov_snaps_per_target", "Snap/Tgt",  "Avoid",    TRUE,           "coverage_snaps_per_target",
  "cov_snaps_per_rec",    "Snap/Rec",  "Avoid",    TRUE,           "coverage_snaps_per_reception",
  "catch_rate",           "Catch %",   "Suppress", FALSE,          "catch_rate",
  "pass_break_up_rate",   "PBU %",     "Disrupt",  TRUE,           "pass_break_up_rate",
  "yards_per_cov_snap",   "Y/CovSnp",  "Damage",   FALSE,          "yards_per_coverage_snap",
  "yards_per_rec",        "Y/Rec",     "Damage",   FALSE,          "yards_per_reception",
  "avg_yac",              "YAC",       "Damage",   FALSE,          "avg_yac",
  "qb_rating_against",    "QB Rtg",    "Damage",   FALSE,          "qb_rating_against",
  "missed_tackle_rate",   "MT %",      "Damage",   FALSE,          "missed_tackle_rate",
  "avg_depth_of_target",  "aDOT vs",   "Context",  NA,             "avg_depth_of_target"
)
COV_GROUP_ORDER <- c("Grade","Avoid","Suppress","Disrupt","Damage","Context")

# 1) grouped season heatmap: rows = metrics grouped by question, cols = def_ssn seasons
plot_coverage_season_heatmap <- function(player_id_in, seasons = NULL,
                                         family = c("man","zone","slot"),
                                         dfs = list(man  = coverage_man_player_season_summary,
                                                    zone = coverage_zone_player_season_summary,
                                                    slot = coverage_slot_player_season_summary)) {
  family <- match.arg(family)
  # rebuild dict HERE so a stale session copy can't leak in via lazy defaults
  dict <- COV_METRIC_DICT
  df <- dfs[[family]] %>% filter(player_id == player_id_in)
  if (!is.null(seasons)) df <- df %>% filter(season %in% seasons)
  if (!nrow(df)) { message("no rows for ", player_id_in, " (", family, ")"); return(invisible(NULL)) }
  dict <- dict %>% filter(paste0(family, "_", metric, "_season_pctl") %in% names(df))
  
  pd <- bind_rows(lapply(seq_len(nrow(dict)), function(i) {
    m <- dict$metric[i]
    tibble(group  = dict$group[i], metric = dict$label[i],
           season = df$season,
           col_id = paste0(df$def_ssn, "\n(", df$n, "g)"),
           pctl   = df[[paste0(family, "_", m, "_season_pctl")]])
  })) %>%
    mutate(pctl_plot = pctl,
           lbl = ifelse(is.na(pctl), "—", scales::percent(pctl, accuracy = 1)),
           group  = factor(group, levels = COV_GROUP_ORDER),
           metric = factor(metric, levels = rev(dict$label)),
           col_id = factor(col_id, levels = unique(col_id[order(season)])))
  
  nm <- df$player[1]
  ggplot(pd, aes(col_id, metric, fill = pctl_plot)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = lbl, color = !is.na(pctl_plot) & abs(pctl_plot - 0.5) > 0.25),
              size = 3.4, fontface = "bold", show.legend = FALSE) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0,1), labels = scales::percent,
                         name = "Pctl", na.value = "grey88") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(title = paste0(nm, " — ", family, " coverage — percentile by season"),
         subtitle = "rows grouped by question | tile = pctl within position-season | aDOT inverted: high = shallower targets against",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 0, size = 9),
          axis.text.x.top = element_text(face = "bold", size = 8, lineheight = 0.9),
          legend.key.height = unit(1.2, "cm"))
}

plot_coverage_season_heatmap(41662,  family = "man")
plot_coverage_season_heatmap(41662, family = "zone")



# 2) common-opponent percentiles: focal game vs CB pool facing the SAME qbgrp_ssn
coverage_common_opp_pctl <- function(player_id_in, season_in,
                                     family = c("combined","man","zone"),
                                     dict = COV_METRIC_DICT,
                                     df = final_coverage_df_qbgrp,
                                     min_comp = 3) {
  family   <- match.arg(family)
  snap_col <- switch(family, combined = "snap_counts_coverage",
                     man = "man_snap_counts_coverage", zone = "zone_snap_counts_coverage")
  snap_min <- switch(family, combined = 23, man = 7, zone = 13)
  pref     <- if (family == "combined") "" else paste0(family, "_")
  dict     <- dict %>% filter(!is.na(higher_better))   # context metrics aren't scored
  
  d     <- df %>% filter(.data[[snap_col]] >= snap_min, !is.na(final_position))
  focal <- d %>% filter(player_id == player_id_in, season == season_in)
  if (!nrow(focal)) { message("no focal games"); return(invisible(NULL)) }
  comp  <- d %>% filter(final_position == focal$final_position[1],
                        !(player_id == player_id_in & season == season_in))
  
  per_game <- bind_rows(lapply(seq_len(nrow(focal)), function(i) {
    off  <- focal$qbgrp_ssn[i]
    pool <- comp %>% filter(qbgrp_ssn == off)
    tibble(qbgrp_ssn = off, week = focal$week[i],
           label  = dict$label,
           n_comp = nrow(pool),
           pctl   = vapply(seq_len(nrow(dict)), function(j) {
             cn <- paste0(pref, dict$raw_suffix[j])
             v  <- focal[[cn]][i]; x <- pool[[cn]]
             if (!dict$higher_better[j]) { v <- -v; x <- -x }
             if (is.na(v) || sum(!is.na(x)) < min_comp) NA_real_ else mean(x <= v, na.rm = TRUE)
           }, numeric(1)))
  }))
  
  summary <- per_game %>%
    group_by(label) %>%
    summarise(pctl_co = mean(pctl, na.rm = TRUE), n_defs = sum(!is.na(pctl)), .groups = "drop")
  
  list(per_game = per_game, summary = summary,
       focal_name = focal$player[1], focal_season = season_in, family = family)
}

byard_co_man  <- coverage_common_opp_pctl(51276, 2025, "man")
byard_co_zone <- coverage_common_opp_pctl(128375, 2025, "zone")

# SOS honesty check before trusting: comp games per opponent
byard_co_man$per_game %>% distinct(qbgrp_ssn, n_comp) %>% arrange(n_comp)


plot_co_pctl_bars <- function(co_obj, order = RECV_CO_ORDER, title = NULL,
                              focal_name = NULL, focal_season = NULL, n_games = NULL) {
  
  pd <- co_obj$summary %>%
    filter(!is.na(pctl_co)) %>%
    mutate(label = factor(as.character(label), levels = rev(order))) %>%
    filter(!is.na(label))
  
  nm <- focal_name   %||% co_obj$focal_name   %||% "Focal"
  yr <- focal_season %||% co_obj$focal_season %||% ""
  ng <- n_games %||% dplyr::n_distinct(co_obj$per_game$week)
  
  fam_lbl    <- if (!is.null(co_obj$family)) paste0(" — ", toupper(co_obj$family)) else ""
  auto_title <- paste0(nm, " — ", yr, fam_lbl, " — vs common-opponent cohort")
  
  sub_txt <- if (!is.null(co_obj$family)) {
    paste0("percentile among same-position defenders facing the SAME offense, averaged across ",
           ng, " games  |  dashed = 50th")
  } else {
    paste0("percentile among same-archetype receivers facing the SAME defense, averaged across ",
           ng, " games  |  dashed = 50th")
  }
  
  ggplot(pd, aes(x = pctl_co, y = label, fill = pctl_co)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45", linewidth = 0.4) +
    geom_text(aes(label = scales::percent(pctl_co, accuracy = 1)),
              hjust = -0.15, size = 4, fontface = "bold", color = "grey20") +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1.08),
                       breaks = c(0, .25, .5, .75, 1)) +
    labs(title    = title %||% auto_title,
         subtitle = sub_txt,
         x = "Percentile", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title       = element_text(face = "bold", size = 16),
          plot.subtitle    = element_text(color = "grey40", size = 9),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y      = element_text(size = 11, face = "bold"),
          legend.position  = "none")
}


# plots — your existing functions work as-is
plot_co_pctl_bars(byard_co_man,  order = COV_METRIC_DICT$label[COV_METRIC_DICT$group != "Context"])
plot_co_pctl_bars(byard_co_zone, order = COV_METRIC_DICT$label[COV_METRIC_DICT$group != "Context"])



plot_vs_exp_strip <- function(focal_id, focal_def_ssn, comps_df = NULL,
                              qa_df = receiving_coverage_defender_final,
                              title = NULL) {
  
  qa_focal <- qa_df %>% filter(coverage_player_id == focal_id, def_ssn == focal_def_ssn)
  if (nrow(qa_focal) != 1) { message("no vs-exp row for focal"); return(invisible(NULL)) }
  
  pd <- tibble(
    label = c("Catch% vs Exp", "Y/Tgt vs Exp"),
    scope = "League",
    pctl  = c(qa_focal$cr_pctl[1], qa_focal$ypt_pctl[1])
  )
  
  if (!is.null(comps_df)) {
    qa_comp <- qa_df %>%
      semi_join(comps_df %>% select(coverage_player_id, def_ssn),
                by = c("coverage_player_id", "def_ssn"))
    pd <- bind_rows(pd, tibble(
      label = c("Catch% vs Exp", "Y/Tgt vs Exp"),
      scope = "Comps",
      pctl  = c(mean(qa_comp$catch_rate_vs_exp >= qa_focal$catch_rate_vs_exp[1], na.rm = TRUE),
                mean(qa_comp$ypt_vs_exp        >= qa_focal$ypt_vs_exp[1],        na.rm = TRUE))
    ))
  }
  
  pd <- pd %>% mutate(scope = factor(scope, levels = c("League","Comps")),
                      label = factor(label, levels = c("Y/Tgt vs Exp","Catch% vs Exp")))
  
  ggplot(pd, aes(scope, label, fill = pctl)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = scales::percent(pctl, accuracy = 1),
                  color = abs(pctl - 0.5) > 0.25),
              size = 4.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(title = title %||% paste0(qa_focal$final_position[1], " ", focal_id, " — ", focal_def_ssn,
                                   " — vs receiver expectation"),
         subtitle = paste0("all coverage, season grain  |  held receivers below their own norms = high pctl  |  n_tgt = ",
                           qa_focal$n_tgt[1]),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid = element_blank(),
          axis.text.x.top = element_text(face = "bold", size = 12),
          axis.text.y = element_text(size = 11, face = "bold"),
          legend.key.height = unit(1.2, "cm"))
}

plot_vs_exp_strip(51276, "TEN2025", comps_df = comps)
