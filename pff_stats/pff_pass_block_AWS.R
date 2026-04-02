

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

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


pass_block_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.pass_block_summary
")


pass_block_summary <- pass_block_summary %>% filter(position %in% c("T", "G", "C"))

pass_block_summary %>% 
  #filter(snap_counts_pass_block >= 10, true_pass_set_non_spike_pass_block >= 5) %>%
  summarise(
    pass_snap = list(quantile(snap_counts_pass_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    true_pass_snap = list(quantile(true_pass_set_non_spike_pass_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    pass_grade = list(quantile(grades_pass_block, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    true_pass_grade = list(quantile(true_pass_set_grades_pass_block, probs = seq(0, 1, 0.1), na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))
# snap_counts_pass_block - 25+
# true_pass_set_non_spike_pass_block - 10+

pass_block_summary$team[which(pass_block_summary$team == "ARI")] = "ARZ"
pass_block_summary$team[which(pass_block_summary$team == "BAL")] = "BLT"
pass_block_summary$team[which(pass_block_summary$team == "CLE")] = "CLV"
pass_block_summary$team[which(pass_block_summary$team == "HOU")] = "HST"
pass_block_summary$team[which(pass_block_summary$team == "LAC" & pass_block_summary$season == 2016)] = "SD"
pass_block_summary$team[which(pass_block_summary$team == "LV" & pass_block_summary$season <= 2019)] = "OAK"

pass_block_summary$team_name[which(pass_block_summary$team_name == "ARI")] = "ARZ"
pass_block_summary$team_name[which(pass_block_summary$team_name == "BAL")] = "BLT"
pass_block_summary$team_name[which(pass_block_summary$team_name == "CLE")] = "CLV"
pass_block_summary$team_name[which(pass_block_summary$team_name == "HOU")] = "HST"
pass_block_summary$team_name[which(pass_block_summary$team_name == "LAC" & pass_block_summary$season == 2016)] = "SD"
pass_block_summary$team_name[which(pass_block_summary$team_name == "LV" & pass_block_summary$season <= 2019)] = "OAK"


pass_block_summary_qbgrp <- 
  left_join(pass_block_summary %>% select(-scraped_at),
            combined_grade_epa_summary %>% select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc, def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "posteam", 
                   "week" = "week", 
                   "season" = "season"))

# play_counts <- left_join(play_counts %>% select(-scraped_at), 
#                          games %>% select(-scraped_at), by = c("game_id" = "id"))

pass_block_summary_qbgrp <- left_join(pass_block_summary_qbgrp,
                                     play_counts %>% select(player_id, week, season, game_id, position),
                                     by = c("player_id", "week", "season"))

colnames(pass_block_summary_qbgrp)[25] <- "position"
colnames(pass_block_summary_qbgrp)[47] <- "det_position"


all_pass_block_summary <- pass_block_summary_qbgrp %>%
  filter(snap_counts_pass_block >= 25) %>% 
  select(player, player_id, det_position, team, team_name, opp, week, qbgrp_ssn, def_ssn, season, franchise_id, snap_counts_pass_block, grades_pass_block, pressures_allowed, hurries_allowed)

all_pass_block_summary$pressure_pct = all_pass_block_summary$pressures_allowed / all_pass_block_summary$snap_counts_pass_block
all_pass_block_summary$hurries_pct = all_pass_block_summary$hurries_allowed / all_pass_block_summary$snap_counts_pass_block


tps_pass_block_summary <- pass_block_summary_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_block >= 16) %>%
  select(player, player_id, det_position, team, team_name, opp, week, qbgrp_ssn, def_ssn, season, franchise_id, true_pass_set_snap_counts_pass_block, true_pass_set_grades_pass_block, true_pass_set_pressures_allowed, true_pass_set_hurries_allowed)

tps_pass_block_summary$true_pass_set_pressure_pct = tps_pass_block_summary$true_pass_set_pressures_allowed / tps_pass_block_summary$true_pass_set_snap_counts_pass_block
tps_pass_block_summary$true_pass_set_hurries_pct = tps_pass_block_summary$true_pass_set_hurries_allowed / tps_pass_block_summary$true_pass_set_snap_counts_pass_block


all_pass_block_summary <- all_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  mutate(
    player_grade_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_pass_block)),
    player_pressure_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-pressure_pct)),
    player_hurries_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-hurries_pct))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_grade_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_pass_block)),
    player_pressure_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-pressure_pct)),
    player_hurries_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-hurries_pct))
  ) %>%
  ungroup()

tps_pass_block_summary <- tps_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  mutate(
    player_tps_grade_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_grades_pass_block)),
    player_tps_pressure_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-true_pass_set_pressure_pct)),
    player_tps_hurries_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-true_pass_set_hurries_pct))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_tps_grade_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_grades_pass_block)),
    player_tps_pressure_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-true_pass_set_pressure_pct)),
    player_tps_hurries_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-true_pass_set_hurries_pct))
  ) %>%
  ungroup()


all_pass_block_opp_position_percentile <- 
  all_pass_block_summary %>%
    group_by(def_ssn, det_position) %>%
    summarise(player_grade_perc = mean(player_grade_perc), 
              player_pressure_perc = mean(player_pressure_perc), 
              player_hurries_perc = mean(player_hurries_perc), 
              .groups = "drop") %>%
    pivot_wider(names_from = det_position, values_from = c(player_grade_perc, player_pressure_perc, player_hurries_perc))

tps_pass_block_opp_position_percentile <- 
  tps_pass_block_summary %>%
    group_by(def_ssn, det_position) %>%
    summarise(player_tps_grade_perc = mean(player_tps_grade_perc), 
              player_tps_pressure_perc = mean(player_tps_pressure_perc), 
              player_tps_hurries_perc = mean(player_tps_hurries_perc), 
              .groups = "drop") %>%
    pivot_wider(names_from = det_position, values_from = c(player_tps_grade_perc, player_tps_pressure_perc, player_tps_hurries_perc))


# Gap
all_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 4+, 55%+

# Zone
tps_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 4+, 60%+


all_pass_block_player_season_summary <- all_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  summarise(
    grade_perc = mean(player_grade_def_ssn_perc),
    pressure_perc = mean(player_pressure_def_ssn_perc),
    hurries_perc = mean(player_hurries_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 4) %>%  # or whatever threshold
  group_by(det_position, season) %>%
  mutate(grade_season_pctl = percent_rank(grade_perc),
         pressure_season_pctl = percent_rank(pressure_perc),
         hurries_season_pctl = percent_rank(hurries_perc)) %>%
  ungroup()

tps_pass_block_player_season_summary <- tps_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  summarise(
    grade_perc = mean(player_tps_grade_def_ssn_perc),
    pressure_perc = mean(player_tps_pressure_def_ssn_perc),
    hurries_perc = mean(player_tps_hurries_def_ssn_perc),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 4) %>%  # or whatever threshold
  group_by(det_position, season) %>%
  mutate(grade_season_pctl = percent_rank(grade_perc),
         pressure_season_pctl = percent_rank(pressure_perc),
         hurries_season_pctl = percent_rank(hurries_perc)) %>%
  ungroup()


View(all_pass_block_player_season_summary %>% filter(player_id == 83018))
View(tps_pass_block_player_season_summary %>% filter(player_id == 83018))

View(all_pass_block_opp_position_percentile %>% filter(def_ssn == 'DET2025'))
View(tps_pass_block_opp_position_percentile %>% filter(def_ssn == 'DET2025'))
