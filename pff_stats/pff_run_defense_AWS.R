
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


run_defense_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.run_defense_summary
")

run_defense_summary %>% 
  #filter(snap_counts_pass_block >= 10, true_pass_set_non_spike_pass_block >= 5) %>%
  summarise(
    run_snap = list(quantile(snap_counts_run, probs = seq(0, 1, 0.1), na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "quantiles") %>%
  unnest_wider(quantiles, names_sep = "_q") %>%
  rename_with(~ sub("_q", "", .x), starts_with("_q"))
# 10+ SNAPS

run_defense_summary <- run_defense_summary %>%
  filter(snap_counts_run >= 14)


run_defense_summary$team[which(run_defense_summary$team == "ARI")] = "ARZ"
run_defense_summary$team[which(run_defense_summary$team == "BAL")] = "BLT"
run_defense_summary$team[which(run_defense_summary$team == "CLE")] = "CLV"
run_defense_summary$team[which(run_defense_summary$team == "HOU")] = "HST"
run_defense_summary$team[which(run_defense_summary$team == "LAC" & run_defense_summary$season == 2016)] = "SD"
run_defense_summary$team[which(run_defense_summary$team == "LV" & run_defense_summary$season <= 2019)] = "OAK"

run_defense_summary$team_name[which(run_defense_summary$team_name == "ARI")] = "ARZ"
run_defense_summary$team_name[which(run_defense_summary$team_name == "BAL")] = "BLT"
run_defense_summary$team_name[which(run_defense_summary$team_name == "CLE")] = "CLV"
run_defense_summary$team_name[which(run_defense_summary$team_name == "HOU")] = "HST"
run_defense_summary$team_name[which(run_defense_summary$team_name == "LAC" & run_defense_summary$season == 2016)] = "SD"
run_defense_summary$team_name[which(run_defense_summary$team_name == "LV" & run_defense_summary$season <= 2019)] = "OAK"


run_defense_summary <- run_defense_summary %>%
  filter(position %ni% c("FB", "WR"))

run_defense_summary_qbgrp <- 
  left_join(run_defense_summary %>% select(-scraped_at),
            combined_grade_epa_summary %>% select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc, def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "opp", 
                   "week" = "week", 
                   "season" = "season"))

run_defense_summary_qbgrp <- left_join(run_defense_summary_qbgrp,
                                      play_counts %>% select(player_id, week, season, game_id, position),
                                      by = c("player_id", "week", "season"))

colnames(run_defense_summary_qbgrp)[20] <- "position"
colnames(run_defense_summary_qbgrp)[41] <- "det_position"


# GRADES_RUN_DEFENSE
# GRADES_TACKLE
# RUN_STOP_OPP
# STOP_PERCENT
# STOPS
# TACKLES
# ASSISTS
# MISSED_TACKLE_RATE
# MISSED_TACKLES

## YOU FORGOT AVG_DEPTH_OF_TACKLE DAMN IT

### CREATE THESE
# ASSISTS_PCT 
# TACKLES_PERCENT

# WE'D NEED TO GROUP BY, MUTATE ACROSS THE ENTIRE ... DEF_SSN / WEEK SHIT

run_defense_summary_qbgrp <- 
  run_defense_summary_qbgrp %>%
  group_by(def_ssn, week) %>%
  mutate(tackle_opp = sum(tackles, na.rm = T),
         assist_opp = sum(assists, na.rm = T))

run_defense_summary_qbgrp$tackle_pct = run_defense_summary_qbgrp$tackles / run_defense_summary_qbgrp$tackle_opp
run_defense_summary_qbgrp$assists_pct = run_defense_summary_qbgrp$assists / run_defense_summary_qbgrp$assist_opp


# Gap
run_defense_summary_qbgrp %>%
  group_by(player, player_id, position, def_ssn) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, 0.1))
# 6+, 45%+

run_defense_summary_player_agg <- run_defense_summary_qbgrp %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    player_grade_run_def_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_run_defense)),
    player_grade_tackle_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_tackle)),
    player_stop_pct_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(stop_percent)),
    player_tackle_pct_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tackle_pct)),
    player_assists_pct_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(assists_pct)),
    player_missed_tackle_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-missed_tackle_rate)),
    player_avg_depth_of_tackle_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_depth_of_tackle))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    player_grade_run_def_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_run_defense)),
    player_grade_tackle_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(grades_tackle)),
    player_stop_pct_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(stop_percent)),
    player_tackle_pct_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(tackle_pct)),
    player_assists_pct_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(assists_pct)),
    player_missed_tackle_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-missed_tackle_rate)),
    player_avg_depth_of_tackle_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank(-avg_depth_of_tackle)),
    n = n()
  ) %>%
  filter(n >= 6) %>%
  ungroup()

valid_qbgrp_run <- run_defense_summary_player_agg %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

run_defense_opp_position_percentile <- run_defense_summary_player_agg %>%
  filter(qbgrp_ssn %in% valid_qbgrp_run) %>%
  group_by(qbgrp_ssn, position) %>%
  summarise(
    player_grade_run_def_perc = mean(player_grade_run_def_def_ssn_perc),
    player_grade_tackle_perc = mean(player_grade_tackle_def_ssn_perc),
    player_stop_pct_perc = mean(player_stop_pct_def_ssn_perc),
    player_tackle_pct_perc = mean(player_tackle_pct_def_ssn_perc),
    player_assists_pct_perc = mean(player_assists_pct_def_ssn_perc),
    player_missed_tackle_rate_perc = mean(player_missed_tackle_rate_def_ssn_perc),
    player_avg_depth_of_tackle_perc = mean(player_avg_depth_of_tackle_def_ssn_perc),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position, 
    values_from = c(player_grade_run_def_perc, player_grade_tackle_perc, player_stop_pct_perc, 
                    player_tackle_pct_perc, player_assists_pct_perc, player_missed_tackle_rate_perc, 
                    player_avg_depth_of_tackle_perc)
  )

# Gap

run_defense_player_season_summary <- run_defense_summary_qbgrp %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_grade_run_def = mean(grades_run_defense),
    player_grade_tackle = mean(grades_tackle),
    player_stop_pct = mean(stop_percent),
    player_tackle_pct = mean(tackle_pct),
    player_assists_pct = mean(assists_pct),
    player_missed_tackle_rate = mean(missed_tackle_rate),
    player_avg_depth_of_tackle = mean(avg_depth_of_tackle),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%
  group_by(position, season) %>%
  mutate(
    grade_run_def_season_pctl = percent_rank(player_grade_run_def),
    grade_tackle_season_pctl = percent_rank(player_grade_tackle),
    stop_pct_season_pctl = percent_rank(player_stop_pct),
    tackle_pct_season_pctl = percent_rank(player_tackle_pct),
    assists_pct_season_pctl = percent_rank(player_assists_pct),
    missed_tackle_rate_season_pctl = percent_rank(player_missed_tackle_rate),
    avg_depth_of_tackle_season_pctl = percent_rank(player_avg_depth_of_tackle)
  ) %>%
  ungroup()


View(run_defense_player_season_summary %>% filter(player_id == 8982))

# Defense version — same idea, def_ssn filter, no _def suffix on cols
plot_ybc_yac_quadrants_def <- function(team_season, data = rush_stats_high) {
  ssn <- as.numeric(sub("^[A-Z]+", "", team_season))
  pd <- data %>%
    filter(def_ssn == team_season) %>%
    mutate(opp = sub(as.character(ssn), "", off_ssn))
  
  ggplot(pd, aes(ybc_rank, yac_rank)) +
    annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "#a63603", alpha = 0.06) +
    annotate("rect", xmin = 0,   xmax = 0.5, ymin = 0,   ymax = 0.5, fill = "#08519c", alpha = 0.06) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    geom_point(aes(size = attempts), color = "#08519c", alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = paste0("W", week, " ", opp)),
                             size = 3, seed = 1, max.overlaps = Inf) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = paste0(team_season, " — Run defense quadrants"),
         subtitle = "Top-right: got gashed both phases. Bottom-left: shut it down.",
         x = "YBC rank allowed", y = "YAC rank allowed", size = "Attempts") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 10)
    )
}

plot_ybc_yac_quadrants_def("DET2025")
plot_ybc_yac_quadrants_def("BLT2025")