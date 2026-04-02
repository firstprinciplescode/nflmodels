

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
  final_coverage_df %>% filter(snap_counts_coverage >= 20 | man_snap_counts_coverage >= 7 | zone_snap_counts_coverage >= 12 | coverage_snaps_slot >= 8)


final_coverage_df <- final_coverage_df %>%
  mutate(
    final_position = case_when(
      adv_position == "MLB"                     ~ "MLB",
      str_detect(adv_position, "LB$")           ~ "LB",        # ends in “LB”
      adv_position %in% c("FS", "SS")           ~ "S",
      adv_position == "SCB"                     ~ "SCB",
      str_detect(adv_position, "CB")            ~ "CB",        # anywhere “CB”
      TRUE                                       ~ "DE"         # everything else
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


colnames(final_coverage_df_qbgrp)

final_coverage_df_qbgrp %>%
  filter(snap_counts_coverage >= 20) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 6 - FOR THE GENERAL STATS

final_coverage_df_qbgrp %>%
  filter(man_snap_counts_coverage >= 7) %>%
  group_by(player, player_id, final_position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 4 FOR THE MAN STATS

final_coverage_df_qbgrp %>%
  filter(zone_snap_counts_coverage >= 12) %>%
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
# COMBINED: 20 / 6
# MAN: 7 / 6
# ZONE: 12 / 6
# SLOT: 8 / 4


# ============================================
# 1. COMBINED COVERAGE
# ============================================
coverage_combined_player_agg <- final_coverage_df_qbgrp %>%
  filter(snap_counts_coverage >= 20) %>%
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
  filter(snap_counts_coverage >= 20) %>%
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
  filter(zone_snap_counts_coverage >= 12) %>%
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
  filter(n_zone >= 5) %>%
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
  filter(zone_snap_counts_coverage >= 12) %>%
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
  filter(coverage_snaps_slot >= 10) %>%
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
  filter(coverage_snaps_slot >= 7) %>%
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

View(coverage_man_player_season_summary %>% filter(player_id == 8000))
View(coverage_zone_player_season_summary %>% filter(player_id == 8000))
View(coverage_combined_player_season_summary %>% filter(player_id == 8000))


put_object(
  file = "~/pff_blocking_defense_workspace_AWS.RData",
  object = "temp/pff_blocking_defense_workspace_AWS.RData",
  bucket = "nfl-pff-data-lucas"
)
