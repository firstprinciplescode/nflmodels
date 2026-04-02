
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

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


pass_rush_kpis <- run_athena_query("
    SELECT  *
    FROM    nfl_data.pass_rush_kpis
")

pass_rush_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.pass_rush_summary
")


full_pass_rush <- full_join(pass_rush_summary, pass_rush_kpis, by = c("team_name", "player_game_count", "eligible_season", "team", "position", "player", "franchise_id", "player_id", "week", "season", "draft_season", "jersey_number"))

# What's in summary but not scheme?
anti_join(pass_rush_summary, pass_rush_kpis, 
          by = c("player_id", "week", "season")) %>%
  nrow()

# What's in scheme but not summary?
anti_join(pass_rush_kpis, pass_rush_summary, 
          by = c("player_id", "week", "season")) %>%
  nrow()

full_pass_rush <- full_pass_rush %>%
  rename_with(~ sub("\\.y$", "_kpis", .), ends_with(".y")) %>%
  rename_with(~ sub("\\.x$", "", .), ends_with(".x"))

unique(full_pass_rush$position)


full_pass_rush %>% 
  #filter(snap_counts_pass_block >= 10, true_pass_set_non_spike_pass_block >= 5) %>%
  summarise(
    pass_rush_snap = list(quantile(snap_counts_pass_rush, probs = seq(0, 1, 0.1), na.rm = TRUE)),
    tps_pass_rush_snap = list(quantile(true_pass_set_snap_counts_pass_rush, probs = seq(0, 1, 0.1), na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))
# 7 - snap_counts_pass_rush
# 5 - true_pass_set_snap_counts_pass_rush


full_pass_rush <- full_pass_rush %>%
  filter(snap_counts_pass_rush >= 11 | true_pass_set_snap_counts_pass_rush >= 7)

full_pass_rush$team[which(full_pass_rush$team == "ARI")] = "ARZ"
full_pass_rush$team[which(full_pass_rush$team == "BAL")] = "BLT"
full_pass_rush$team[which(full_pass_rush$team == "CLE")] = "CLV"
full_pass_rush$team[which(full_pass_rush$team == "HOU")] = "HST"
full_pass_rush$team[which(full_pass_rush$team == "LAC" & full_pass_rush$season == 2016)] = "SD"
full_pass_rush$team[which(full_pass_rush$team == "LV" & full_pass_rush$season <= 2019)] = "OAK"

full_pass_rush$team_name[which(full_pass_rush$team_name == "ARI")] = "ARZ"
full_pass_rush$team_name[which(full_pass_rush$team_name == "BAL")] = "BLT"
full_pass_rush$team_name[which(full_pass_rush$team_name == "CLE")] = "CLV"
full_pass_rush$team_name[which(full_pass_rush$team_name == "HOU")] = "HST"
full_pass_rush$team_name[which(full_pass_rush$team_name == "LAC" & full_pass_rush$season == 2016)] = "SD"
full_pass_rush$team_name[which(full_pass_rush$team_name == "LV" & full_pass_rush$season <= 2019)] = "OAK"


full_pass_rush_qbgrp <- 
  left_join(full_pass_rush %>% select(-c(scraped_at_kpis, scraped_at)),
            combined_grade_epa_summary %>% select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc, def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "opp", 
                   "week" = "week", 
                   "season" = "season"))

full_pass_rush_qbgrp <- left_join(full_pass_rush_qbgrp,
                                       play_counts %>% select(player_id, week, season, game_id, position),
                                       by = c("player_id", "week", "season"))

colnames(full_pass_rush_qbgrp)[29] <- "position"
colnames(full_pass_rush_qbgrp)[85] <- "det_position"

# true_pass_set_total_pressures --> RATE THIS true_pass_set_snap_counts_pass_rush
# true_pass_set_prp
# true_pass_set_hurries --> RATE THIS true_pass_set_snap_counts_pass_rush
# true_pass_set_pass_rush_win_rate
# true_pass_set_hits --> RATE THIS true_pass_set_snap_counts_pass_rush
# true_pass_set_grades_pass_rush_defense
# true_pass_set_batted_passes --> RATE THIS true_pass_set_snap_counts_pass_rush

# total_pressures --> RATE THIS snap_counts_pass_rush
# prp
# pass_rush_win_rate
# batted_passes --> RATE THIS snap_counts_pass_rush
# hits --> RATE THIS snap_counts_pass_rush
# grades_pass_rush_defense
# batted_passes --> RATE THIS snap_counts_pass_rush


## SOMETHING NEW - I WANT TO RANK THEM. WITHIN ... 
# ONLY USE POSITION, NO DET_POSITION
# WITHIN EACH POSITION --> RANK 1 FOR .. JUST PRP / PASS_RUSH_WIN_RATE / GRADES_PASS_RUSH_DEFENSE
# IF JUST 1, THEN JUST .5 IT
# WE'RE GOING TO USE PERCENTILE INSTEAD OF RANK ... SO HIGGHEST --> 1, LOWEST 0, MEDIAN IS .5


# ALL
full_pass_rush_qbgrp %>%
  filter(snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 5+ GAMES

# TPS
full_pass_rush_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 5+ GAMES


full_pass_rush_qbgrp <- full_pass_rush_qbgrp %>%
  mutate(
    pressure_rate = total_pressures / snap_counts_pass_rush,
    hit_rate = hits / snap_counts_pass_rush,
    hurry_rate = hurries / snap_counts_pass_rush,
    batted_pass_rate = batted_passes / snap_counts_pass_rush,
    tps_pressure_rate = true_pass_set_total_pressures / true_pass_set_snap_counts_pass_rush,
    tps_hit_rate = true_pass_set_hits / true_pass_set_snap_counts_pass_rush,
    tps_hurry_rate = true_pass_set_hurries / true_pass_set_snap_counts_pass_rush,
    tps_batted_pass_rate = true_pass_set_batted_passes / true_pass_set_snap_counts_pass_rush  
  )


pass_rush_all_player_agg <- full_pass_rush_qbgrp %>%
  filter(snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    # All higher is better for pass rush
    player_grade_pass_rush_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_pass_rush_defense)),
    player_prp_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(prp)),
    player_pass_rush_win_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pass_rush_win_rate)),
    player_pressure_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pressure_rate)),
    player_hit_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(hit_rate)),
    player_hurry_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(hurry_rate)),
    player_batted_pass_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(batted_pass_rate))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    grade_pass_rush_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_pass_rush_defense)),
    prp_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(prp)),
    pass_rush_win_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pass_rush_win_rate)),
    pressure_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(pressure_rate)),
    hit_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(hit_rate)),
    hurry_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(hurry_rate)),
    batted_pass_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(batted_pass_rate)),
    n_all = n()
  ) %>%
  filter(n_all >= 8) %>%
  ungroup()

valid_qbgrp_all <- pass_rush_all_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

pass_rush_all_opp_percentile <- pass_rush_all_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_all) %>%
  group_by(qbgrp_ssn, position) %>%
  summarise(
    grade_pass_rush_perc = mean(grade_pass_rush_def_ssn_perc, na.rm = TRUE),
    prp_perc = mean(prp_def_ssn_perc, na.rm = TRUE),
    pass_rush_win_rate_perc = mean(pass_rush_win_rate_def_ssn_perc, na.rm = TRUE),
    pressure_rate_perc = mean(pressure_rate_def_ssn_perc, na.rm = TRUE),
    hit_rate_perc = mean(hit_rate_def_ssn_perc, na.rm = TRUE),
    hurry_rate_perc = mean(hurry_rate_def_ssn_perc, na.rm = TRUE),
    batted_pass_rate_perc = mean(batted_pass_rate_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position,
    values_from = c(grade_pass_rush_perc, prp_perc, pass_rush_win_rate_perc,
                    pressure_rate_perc, hit_rate_perc, hurry_rate_perc, batted_pass_rate_perc)
  )

# Player season summary - all pass rush
pass_rush_all_player_season_summary <- full_pass_rush_qbgrp %>%
  filter(snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_grade_pass_rush = mean(grades_pass_rush_defense, na.rm = TRUE),
    player_prp = mean(prp, na.rm = TRUE),
    player_pass_rush_win_rate = mean(pass_rush_win_rate, na.rm = TRUE),
    player_pressure_rate = mean(pressure_rate, na.rm = TRUE),
    player_hit_rate = mean(hit_rate, na.rm = TRUE),
    player_hurry_rate = mean(hurry_rate, na.rm = TRUE),
    player_batted_pass_rate = mean(batted_pass_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 8) %>%
  group_by(position, season) %>%
  mutate(
    grade_pass_rush_season_pctl = percent_rank(player_grade_pass_rush),
    prp_season_pctl = percent_rank(player_prp),
    pass_rush_win_rate_season_pctl = percent_rank(player_pass_rush_win_rate),
    pressure_rate_season_pctl = percent_rank(player_pressure_rate),
    hit_rate_season_pctl = percent_rank(player_hit_rate),
    hurry_rate_season_pctl = percent_rank(player_hurry_rate),
    batted_pass_rate_season_pctl = percent_rank(player_batted_pass_rate)
  ) %>%
  ungroup()


####
####


pass_rush_tps_player_agg <- full_pass_rush_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    player_tps_grade_pass_rush_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_grades_pass_rush_defense)),
    player_tps_prp_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_prp)),
    player_tps_pass_rush_win_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_pass_rush_win_rate)),
    player_tps_pressure_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_pressure_rate)),
    player_tps_hit_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_hit_rate)),
    player_tps_hurry_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_hurry_rate)),
    player_tps_batted_pass_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_batted_pass_rate))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    tps_grade_pass_rush_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_grades_pass_rush_defense)),
    tps_prp_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_prp)),
    tps_pass_rush_win_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(true_pass_set_pass_rush_win_rate)),
    tps_pressure_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_pressure_rate)),
    tps_hit_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_hit_rate)),
    tps_hurry_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_hurry_rate)),
    tps_batted_pass_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tps_batted_pass_rate)),
    n_tps = n()
  ) %>%
  filter(n_tps >= 6) %>%
  ungroup()

valid_qbgrp <- pass_rush_tps_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

pass_rush_tps_opp_percentile <- pass_rush_tps_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp) %>%
  group_by(qbgrp_ssn, position) %>%
  summarise(
    tps_grade_pass_rush_perc = mean(tps_grade_pass_rush_def_ssn_perc, na.rm = TRUE),
    tps_prp_perc = mean(tps_prp_def_ssn_perc, na.rm = TRUE),
    tps_pass_rush_win_rate_perc = mean(tps_pass_rush_win_rate_def_ssn_perc, na.rm = TRUE),
    tps_pressure_rate_perc = mean(tps_pressure_rate_def_ssn_perc, na.rm = TRUE),
    tps_hit_rate_perc = mean(tps_hit_rate_def_ssn_perc, na.rm = TRUE),
    tps_hurry_rate_perc = mean(tps_hurry_rate_def_ssn_perc, na.rm = TRUE),
    tps_batted_pass_rate_perc = mean(tps_batted_pass_rate_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position,
    values_from = c(tps_grade_pass_rush_perc, tps_prp_perc, tps_pass_rush_win_rate_perc,
                    tps_pressure_rate_perc, tps_hit_rate_perc, tps_hurry_rate_perc, tps_batted_pass_rate_perc)
  )

# Player season summary - TPS
pass_rush_tps_player_season_summary <- full_pass_rush_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_tps_grade_pass_rush = mean(true_pass_set_grades_pass_rush_defense, na.rm = TRUE),
    player_tps_prp = mean(true_pass_set_prp, na.rm = TRUE),
    player_tps_pass_rush_win_rate = mean(true_pass_set_pass_rush_win_rate, na.rm = TRUE),
    player_tps_pressure_rate = mean(tps_pressure_rate, na.rm = TRUE),
    player_tps_hit_rate = mean(tps_hit_rate, na.rm = TRUE),
    player_tps_hurry_rate = mean(tps_hurry_rate, na.rm = TRUE),
    player_tps_batted_pass_rate = mean(tps_batted_pass_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 7) %>%
  group_by(position, season) %>%
  mutate(
    tps_grade_pass_rush_season_pctl = percent_rank(player_tps_grade_pass_rush),
    tps_prp_season_pctl = percent_rank(player_tps_prp),
    tps_pass_rush_win_rate_season_pctl = percent_rank(player_tps_pass_rush_win_rate),
    tps_pressure_rate_season_pctl = percent_rank(player_tps_pressure_rate),
    tps_hit_rate_season_pctl = percent_rank(player_tps_hit_rate),
    tps_hurry_rate_season_pctl = percent_rank(player_tps_hurry_rate),
    tps_batted_pass_rate_season_pctl = percent_rank(player_tps_batted_pass_rate)
  ) %>%
  ungroup()


pass_rush_all_player_season_summary %>%
  filter(player_id == 8982)
