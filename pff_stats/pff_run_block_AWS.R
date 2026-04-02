

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

keep_objects <- c("qb_stats_df_final", "con", "bucket", "run_athena_query")
rm(list = setdiff(ls(), keep_objects))

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  cat("Query ID:", query_id, "\n")
  
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
  
  cat("Final status:", status, "\n")
  
  if (status != "SUCCEEDED") {
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
  s3_path <- trimws(system(result_cmd, intern = TRUE))
  cat("S3 path:", s3_path, "\n")
  
  # Check if file exists
  Sys.sleep(2)  # Give S3 a moment
  check_cmd <- sprintf("aws s3 ls %s", s3_path)
  cat("File check:", system(check_cmd, intern = TRUE), "\n")
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


run_block_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.run_block_summary
")

combined_grade_epa_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.combined_grade_epa_summary
")

play_counts <- run_athena_query("
    SELECT  *
    FROM    nfl_data.play_counts
")

games <- run_athena_query("
    SELECT  *
    FROM    nfl_data.games
")

run_block_summary <- run_block_summary %>% filter(position %in% c("T", "G", "C"))

run_block_summary %>%
  summarise(
    gap_snap = list(quantile(gap_snap_counts_run_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    zone_snap = list(quantile(zone_snap_counts_run_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    gap_grade = list(quantile(gap_grades_run_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    zone_grade = list(quantile(zone_grades_run_block, probs = seq(0, 1, 0.1), na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))

run_block_summary$team[which(run_block_summary$team == "ARI")] = "ARZ"
run_block_summary$team[which(run_block_summary$team == "BAL")] = "BLT"
run_block_summary$team[which(run_block_summary$team == "CLE")] = "CLV"
run_block_summary$team[which(run_block_summary$team == "HOU")] = "HST"
run_block_summary$team[which(run_block_summary$team == "LAC" & run_block_summary$season == 2016)] = "SD"
run_block_summary$team[which(run_block_summary$team == "LV" & run_block_summary$season <= 2019)] = "OAK"

run_block_summary$team_name[which(run_block_summary$team_name == "ARI")] = "ARZ"
run_block_summary$team_name[which(run_block_summary$team_name == "BAL")] = "BLT"
run_block_summary$team_name[which(run_block_summary$team_name == "CLE")] = "CLV"
run_block_summary$team_name[which(run_block_summary$team_name == "HOU")] = "HST"
run_block_summary$team_name[which(run_block_summary$team_name == "LAC" & run_block_summary$season == 2016)] = "SD"
run_block_summary$team_name[which(run_block_summary$team_name == "LV" & run_block_summary$season <= 2019)] = "OAK"


run_block_summary_qbgrp <- 
  left_join(run_block_summary %>% select(-scraped_at),
            combined_grade_epa_summary %>% select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc, def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "posteam", 
                   "week" = "week", 
                   "season" = "season"))

play_counts <- left_join(play_counts %>% select(-scraped_at), 
                         games %>% select(-scraped_at), by = c("game_id" = "id"))

run_block_summary_qbgrp <- left_join(run_block_summary_qbgrp,
                                     play_counts %>% select(player_id, week, season, game_id, position),
                                     by = c("player_id", "week", "season"))
colnames(run_block_summary_qbgrp)[16] <- "position"
colnames(run_block_summary_qbgrp)[39] <- "det_position"

gap_block_summary <- run_block_summary_qbgrp %>%
  filter(gap_snap_counts_run_block >= 7)

zone_block_summary <- run_block_summary_qbgrp %>%
  filter(zone_snap_counts_run_block >= 9)


gap_block_summary %>%
  group_by(det_position) %>%
  summarise(
    gap_grade = list(quantile(gap_grades_run_block, probs = seq(0, 1, 0.1), na.rm = TRUE))
  ) %>%
  pivot_longer(cols = -det_position, names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))
# BASICALLY, WE HAVE TO USE DET_POSITION B/C LG VS RG, LT VS RT ARE 2 DIFFERENT THINGS


gap_block_summary <- gap_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  mutate(
    player_gap_perc = case_when(
      n() == 1 ~ 0.5,
      TRUE ~ percent_rank(gap_grades_run_block)
    )
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_gap_def_ssn_perc = case_when(
      n() == 1 ~ 0.5,
      TRUE ~ percent_rank(gap_grades_run_block)
    )
  ) %>%
  ungroup()

zone_block_summary <- zone_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  mutate(
    player_zone_perc = case_when(
      n() == 1 ~ 0.5,
      TRUE ~ percent_rank(zone_grades_run_block)
    )
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_zone_def_ssn_perc = case_when(
      n() == 1 ~ 0.5,
      TRUE ~ percent_rank(zone_grades_run_block)
    )
  ) %>%
  ungroup()


gap_opp_position_percentile <- 
  gap_block_summary %>%
    group_by(def_ssn, det_position) %>%
    summarise(gap_perc = mean(player_gap_perc), .groups = "drop") %>%
    pivot_wider(names_from = det_position, values_from = gap_perc)

zone_opp_position_percentile <- 
  zone_block_summary %>%
    group_by(def_ssn, det_position) %>%
    summarise(zone_perc = mean(player_zone_perc), .groups = "drop") %>%
    pivot_wider(names_from = det_position, values_from = zone_perc)


# Gap
gap_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  summarise(
    gap_perc = mean(player_gap_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 7+, 30%+

# Zone
zone_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  summarise(
    zone_perc = mean(player_zone_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 8+, 30%+

gap_player_season_summary <- gap_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  summarise(
    gap_perc = mean(player_gap_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%  # or whatever threshold
  group_by(det_position, season) %>%
  mutate(gap_season_pctl = percent_rank(gap_perc)) %>%
  ungroup()

zone_player_season_summary <- zone_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  summarise(
    zone_perc = mean(player_zone_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%
  group_by(det_position, season) %>%
  mutate(zone_season_pctl = percent_rank(zone_perc)) %>%
  ungroup()


View(gap_player_season_summary %>% filter(player_id == 98261))
View(zone_player_season_summary %>% filter(player_id == 98261))
View(gap_opp_position_percentile %>% filter(def_ssn == "PHI2025"))
View(zone_opp_position_percentile %>% filter(def_ssn == "PHI2025"))
