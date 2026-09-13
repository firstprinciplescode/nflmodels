
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

plot_ybc_yac_quadrants_def("DEN2025")


View(gap_opp_position_percentile %>% filter(def_ssn == "DEN2025"))
View(zone_opp_position_percentile %>% filter(def_ssn == "DEN2025"))


View(run_defense_player_season_summary %>% filter(def_ssn == "SEA2025") %>%
       select(player:def_ssn, ends_with("_pctl")))


plot_run_def_card <- function(team_season,
                              df = run_defense_player_season_summary) {
  
  df <- df %>% filter(def_ssn == team_season)
  if (nrow(df) == 0) stop("No matching rows found for ", team_season)
  
  metrics <- c("grade_run_def", "grade_tackle", "stop_pct",
               "tackle_pct", "assists_pct",
               "missed_tackle_rate", "avg_depth_of_tackle")
  metric_labels <- c("Grade", "Tackle Gr", "Stop %",
                     "Tackle %", "Assist %",
                     "MT Rate", "ADoT")
  
  pctl_cols <- paste0(metrics, "_season_pctl")
  
  d_full <- df %>%
    select(player, position, n, all_of(pctl_cols)) %>%
    pivot_longer(ends_with("_season_pctl"),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric   = sub("_season_pctl$", "", metric),
           metric   = factor(metric, levels = metrics, labels = metric_labels),
           position = factor(position,
                             levels = c("DI", "LB", "S", "ED", "CB"),
                             labels = c("DL", "LB", "S", "ED", "CB")))
  
  build_panel <- function(pos, show_x_strip, show_x_axis) {
    d <- d_full %>%
      filter(position == pos) %>%
      arrange(n) %>%
      mutate(player = factor(player, levels = unique(player)))
    if (nrow(d) == 0) return(NULL)
    
    ggplot(d, aes(x = pctl, y = player, fill = pctl)) +
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
            strip.text.x       = if (show_x_strip) element_text(face = "bold", size = 10) else element_blank(),
            strip.background   = element_rect(fill = "grey88", color = NA),
            axis.text.x        = if (show_x_axis)  element_text(size = 7) else element_blank(),
            plot.margin        = margin(t = 10, b = 5))
  }
  
  pos_order <- intersect(c("DL", "LB", "S", "ED", "CB"),
                         as.character(unique(d_full$position)))
  
  panels  <- list()
  heights <- c()
  for (i in seq_along(pos_order)) {
    p <- pos_order[i]
    panels[[i]] <- build_panel(p,
                               show_x_strip = (i == 1),
                               show_x_axis  = (i == length(pos_order)))
    heights[i]  <- d_full %>% filter(position == p) %>% distinct(player) %>% nrow()
  }
  
  library(patchwork)
  if (length(panels) == 1) {
    result <- panels[[1]]
  } else {
    result <- Reduce(`/`, panels) + plot_layout(heights = heights, guides = "collect")
  }
  
  result + plot_annotation(
    title    = paste0(team_season, " — Run Defense Scouting Card"),
    # subtitle back to "elite":
    subtitle = "Each row = one defender  |  Dark = elite season percentile",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10))
  )
}

plot_run_def_card("DEN2025")


plot_run_def_card_player <- function(player_id_vec,
                                     seasons = NULL,
                                     df = run_defense_player_season_summary) {
  
  df <- df %>% filter(player_id %in% player_id_vec)
  if (!is.null(seasons)) df <- df %>% filter(season %in% seasons)
  if (nrow(df) == 0) stop("No matching rows found")
  
  metrics <- c("grade_run_def", "grade_tackle", "stop_pct",
               "tackle_pct", "assists_pct",
               "missed_tackle_rate", "avg_depth_of_tackle")
  metric_labels <- c("Grade", "Tackle Gr", "Stop %",
                     "Tackle %", "Assist %",
                     "MT Rate", "ADoT")
  
  pctl_cols <- paste0(metrics, "_season_pctl")
  
  d_full <- df %>%
    arrange(desc(player), season) %>%                                # player asc top→bottom, newest year at top of each group
    mutate(row_label = paste0(player, " — ", def_ssn, " (", n, " gms)"),
           row_label = factor(row_label, levels = unique(row_label))) %>%
    select(row_label, position, n, all_of(pctl_cols)) %>%
    pivot_longer(ends_with("_season_pctl"),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric   = sub("_season_pctl$", "", metric),
           metric   = factor(metric, levels = metrics, labels = metric_labels),
           position = factor(position,
                             levels = c("DI", "LB", "S", "ED", "CB"),
                             labels = c("DL", "LB", "S", "ED", "CB")))
  
  build_panel <- function(pos, show_x_strip, show_x_axis) {
    d <- d_full %>% filter(position == pos)
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
            strip.text.x       = if (show_x_strip) element_text(face = "bold", size = 10) else element_blank(),
            strip.background   = element_rect(fill = "grey88", color = NA),
            axis.text.x        = if (show_x_axis)  element_text(size = 7) else element_blank(),
            plot.margin        = margin(t = 10, b = 5))
  }
  
  pos_order <- intersect(c("DL", "LB", "S", "ED", "CB"),
                         as.character(unique(d_full$position)))
  
  panels  <- list()
  heights <- c()
  for (i in seq_along(pos_order)) {
    p <- pos_order[i]
    panels[[i]] <- build_panel(p,
                               show_x_strip = (i == 1),
                               show_x_axis  = (i == length(pos_order)))
    heights[i]  <- d_full %>% filter(position == p) %>% distinct(row_label) %>% nrow()
  }
  
  player_names <- unique(df$player)
  title_str <- if (length(player_names) == 1) {
    paste0(player_names, " — Run Defense Scouting Card")
  } else {
    "Run Defense Scouting Card"
  }
  
  library(patchwork)
  if (length(panels) == 1) {
    result <- panels[[1]]
  } else {
    result <- Reduce(`/`, panels) + plot_layout(heights = heights, guides = "collect")
  }
  
  result + plot_annotation(
    title    = title_str,
    subtitle = "Each row = one player-season  |  Dark = elite season percentile",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10))
  )
}


plot_run_def_card_player(c(76662, 81322, 51276, 98319, 10671, 144154, 128375)) # STILL HERE - DL / ED SEA
plot_run_def_card_player(c(55940, 131960, 72191, 101463, 49970, 48415, 49949, 50612)) # STILL HERE - CB / LB / S SEA
plot_run_def_card_player(c(41662, 52000, 156070, 10666, 40588)) # PLAYERS WHO LEFT SEA
plot_run_def_card_player(c(51172)) # NEW PLAYERS SEA



pbp_rush %>% filter(def_ssn == "DET2025") %>% select(defense_personnel, n_dl, n_lb, n_db)
pbp_rush %>% filter(def_ssn == "DET2025") %>% select(offense_personnel, n_ol, n_te)
pbp_rush %>% filter(def_ssn == "DET2025") %>% select(defense_players)
pbp_rush %>% filter(def_ssn == "DET2025") %>% select(offense_players)

pbp_rush %>% filter(def_ssn == "DET2025") %>% select(yardline_100, offense_players, defense_players, n_ol, n_te, n_dl, n_lb, n_db, wp, epa, xpass, pbp_predicted_xpass, part_predicted_xpass, yards_gained, pbp_predicted_ypc, part_predicted_ypc)

combined_ids_defense %>% filter(team == "NE", season == 2025) %>%
  select(player, player_id, gsis_id, position) %>%
  distinct() %>%
  View(.)

unique(pbp_rush$ydstogo_group)


.reorder_within <- function(x, by, within) {
  key <- paste(x, within, sep = "___")
  factor(key, levels = unique(key[order(within, by)]))
}
.scale_x_reordered <- function(...) {
  ggplot2::scale_x_discrete(labels = function(z) sub("___.+$", "", z), ...)
}

plot_on_off_dist <- function(gsis_id,
                             outcome     = "yards_gained",
                             bucket      = "all",
                             player_name = NULL,
                             def_ssn     = NULL,
                             df          = pbp_rush,
                             breaks      = NULL,
                             clip        = c(0.02, 0.98),   # view window (quantiles of outcome)
                             wp_range    = NULL) {          # e.g. c(0.1, 0.9) to drop garbage time
  
  outcome_lab <- c(
    yards_gained       = "Yards / Rush",
    epa                = "EPA / Rush",
    ypc_oe_part        = "Yds Over Exp (part)",
    ypc_oe_pbp         = "Yds Over Exp (pbp)",
    part_predicted_ypc = "Predicted YPC (part)",
    pbp_predicted_ypc  = "Predicted YPC (pbp)"
  )
  spec <- list(
    part_predicted_xpass = list(label = "xPass (part)", breaks = seq(0, 1, 0.2),  discrete = FALSE),
    pbp_predicted_xpass  = list(label = "xPass (pbp)",  breaks = seq(0, 1, 0.2),  discrete = FALSE),
    xpass                = list(label = "xPass (nfl)",  breaks = seq(0, 1, 0.2),  discrete = FALSE),
    wp                   = list(label = "Win Prob",     breaks = seq(0, 1, 0.2),  discrete = FALSE),
    yardline_100         = list(label = "Yardline",     breaks = seq(0, 100, 20), discrete = FALSE),
    shotgun              = list(label = "Shotgun",                                discrete = TRUE),
    n_ol                 = list(label = "# OL",                                   discrete = TRUE),
    n_te                 = list(label = "# TE",                                   discrete = TRUE),
    down = list(label = "Down", discrete = TRUE),
    ydstogo_group = list(label = "Yds-to-Go", discrete = TRUE,
                         levels = c("1st9", "1st10", "1stLong",
                                    "2nd2", "2nd3-5", "2nd6-10", "2ndLong",
                                    "3rd1", "3rd2", "3rd3", "3rdRest",
                                    "4th1", "4thLong"))
    
  )
  all_vars <- c("part_predicted_xpass", "wp", "yardline_100", "n_ol", "n_te")
  
  if (!outcome %in% names(outcome_lab))
    stop("outcome must be one of: ", paste(names(outcome_lab), collapse = ", "))
  if (bucket != "all" && !bucket %in% names(spec))
    stop("bucket must be 'all' or one of: ", paste(names(spec), collapse = ", "))
  
  use_vars <- if (bucket == "all") all_vars else bucket
  
  if (!is.null(def_ssn))  df <- df %>% filter(def_ssn == !!def_ssn)
  
  d <- df %>%
    ungroup() %>%
    mutate(
      on_field    = grepl(gsis_id, defense_players, fixed = TRUE),
      grp         = factor(if_else(on_field, "ON", "OFF"), levels = c("OFF", "ON")),
      ypc_oe_part = yards_gained - part_predicted_ypc,
      ypc_oe_pbp  = yards_gained - pbp_predicted_ypc
    )
  
  if (!is.null(wp_range)) d <- d %>% filter(wp >= wp_range[1], wp <= wp_range[2])
  
  ylim_vals <- quantile(d[[outcome]], probs = clip, na.rm = TRUE)
  
  one_block <- function(v) {
    s  <- spec[[v]]
    br <- if (!is.null(breaks) && length(use_vars) == 1) breaks else s$breaks
    if (isTRUE(s$discrete)) {
      lab  <- as.character(d[[v]])
      sort <- if (!is.null(s$levels)) match(lab, s$levels) else as.numeric(lab)
    } else {
      cx   <- cut(d[[v]], breaks = br, include.lowest = TRUE, right = FALSE, dig.lab = 4)
      sort <- as.integer(cx);     lab <- as.character(cx)
    }
    tibble(var = s$label, sort = sort, bin_lab = lab,
           grp = d$grp, y = d[[outcome]])
  }
  
  lvl  <- vapply(use_vars, function(v) spec[[v]]$label, character(1))
  long <- bind_rows(lapply(use_vars, one_block)) %>%
    filter(!is.na(bin_lab), !is.na(y)) %>%
    mutate(var = factor(var, levels = lvl),
           xr  = .reorder_within(bin_lab, sort, var))
  
  cell_n   <- long %>% count(var, xr, grp, name = "n")
  bin_rate <- long %>%
    group_by(var, xr) %>%
    summarise(on_rate = mean(grp == "ON"), .groups = "drop") %>%
    mutate(lab = sprintf("%.0f%%", 100 * on_rate))
  
  ttl <- if (!is.null(player_name)) player_name else gsis_id
  sub <- sprintf("%s  |  on vs off%s%s", outcome_lab[[outcome]],
                 if (!is.null(def_ssn))  paste0("  |  ", def_ssn) else "",
                 if (!is.null(wp_range)) sprintf("  |  wp %.2f–%.2f", wp_range[1], wp_range[2]) else "")
  
  dodge     <- position_dodge(width = 0.8)
  zero_line <- outcome %in% c("epa", "ypc_oe_part", "ypc_oe_pbp")
  single    <- length(use_vars) == 1
  
  p <- ggplot(long, aes(x = xr, y = y, fill = grp, color = grp))
  
  if (zero_line)
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50", linewidth = 0.4)
  
  p <- p +
    geom_violin(position = dodge, width = 0.75, alpha = 0.18,
                color = NA, trim = TRUE, scale = "width") +
    geom_boxplot(position = dodge, width = 0.16, alpha = 0.9,
                 outlier.shape = NA, color = "grey20", linewidth = 0.3,
                 show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", position = dodge,
                 shape = 23, size = 1.6, fill = "white", color = "black",
                 aes(group = grp), show.legend = FALSE) +
    geom_text(data = cell_n, aes(x = xr, y = -Inf, label = n, group = grp, color = grp),
              position = dodge, vjust = -0.8, size = 2.4, fontface = "bold",
              inherit.aes = FALSE)
  
  if (single)
    p <- p + geom_text(data = bin_rate, aes(x = xr, y = Inf, label = lab),
                       vjust = 1.4, size = 3, fontface = "bold", color = "grey15",
                       inherit.aes = FALSE)
  
  p +
    .scale_x_reordered() +
    scale_fill_manual(values = c(OFF = "#bdbdbd", ON = "#08519c"), name = NULL) +
    scale_color_manual(values = c(OFF = "#969696", ON = "#08519c"),
                       name = NULL, guide = "none") +
    coord_cartesian(ylim = ylim_vals) +
    facet_wrap(~ var, scales = "free_x", ncol = if (single) 1 else 2) +
    labs(title = paste0(ttl, " — On/Off Distribution"),
         subtitle = sub,
         caption = sprintf("Box = median + IQR  |  diamond = mean  |  violin = shape  |  view clipped to %.0f–%.0f pctl (all plays kept)  |  counts on(blue)/off(grey)",
                           100 * clip[1], 100 * clip[2]),
         x = NULL, y = outcome_lab[[outcome]]) +
    theme_minimal(base_size = 10) +
    theme(legend.position  = "top",
          axis.text.x      = element_text(angle = 35, hjust = 1, size = 7),
          strip.text       = element_text(face = "bold", size = 10),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 14),
          plot.caption     = element_text(size = 7, color = "grey40"))
}

# ── plot_on_off_dist() reference ───────────────────────────────────────────────
#
# OUTCOMES (y-axis):
#   "yards_gained"        # Yards / Rush               (raw)
#   "epa"                 # EPA / Rush                 (raw)
#   "ypc_oe_part"         # Yds Over Exp (part)        = yards_gained - part_predicted_ypc
#   "ypc_oe_pbp"          # Yds Over Exp (pbp)         = yards_gained - pbp_predicted_ypc
#   "part_predicted_ypc"  # Predicted YPC (part)       (model expectation only, diagnostic)
#   "pbp_predicted_ypc"   # Predicted YPC (pbp)        (model expectation only, diagnostic)
#
# BUCKETS (x-axis):
#   "part_predicted_xpass"  # xPass (part)  (cont)
#   "pbp_predicted_xpass"   # xPass (pbp)   (cont)
#   "xpass"                 # xPass (nfl)   (cont)  ← raw nflfastR
#   "wp"                    # Win Prob      (cont; = OPPONENT wp on DET D snaps)
#   "yardline_100"          # Yardline      (cont)
#   "shotgun"               # Shotgun 0/1   (discrete)
#   "n_ol"                  # # OL          (discrete)
#   "n_te"                  # # TE          (discrete)               
#   "down"                  # down
#
# OPTIONAL ARGS:
#   breaks   = seq(0,1,0.1)   # override bin edges (single bucket only)
#   clip     = c(0.02,0.98)   # y view window as quantiles of outcome (view-only)
#   wp_range = c(0.1,0.9)     # drop garbage-time snaps before everything (off by default)

plot_on_off_dist("00-0036193", outcome = "epa", bucket = "part_predicted_xpass",
                 player_name = "Anfernee Jennings", def_ssn = "NE2025")

plot_on_off_dist("00-0036193", outcome = "epa", bucket = "yardline_100",
                 player_name = "Anfernee Jennings", def_ssn = "NE2025")


combined_ids_defense %>% 
  filter(gsis_id == "00-0032424") %>% 
  select(team, season) %>%
  distinct()

combined_ids_defense %>% filter(player == "Kevin Byard") %>%
  select(player, player_id, gsis_id, position) %>%
  distinct() %>%
  View(.)


plot_def_ssn_dist <- function(def_ssn,
                              outcome  = "yards_gained",
                              bucket   = "all",
                              df       = pbp_rush,
                              breaks   = NULL,
                              clip     = c(0.02, 0.98),
                              wp_range = NULL) {
  
  outcome_lab <- c(
    yards_gained       = "Yards / Rush",
    epa                = "EPA / Rush",
    ypc_oe_part        = "Yds Over Exp (part)",
    ypc_oe_pbp         = "Yds Over Exp (pbp)",
    part_predicted_ypc = "Predicted YPC (part)",
    pbp_predicted_ypc  = "Predicted YPC (pbp)"
  )
  spec <- list(
    part_predicted_xpass = list(label = "xPass (part)", breaks = seq(0, 1, 0.1),  discrete = FALSE),
    pbp_predicted_xpass  = list(label = "xPass (pbp)",  breaks = seq(0, 1, 0.1),  discrete = FALSE),
    xpass                = list(label = "xPass (nfl)",  breaks = seq(0, 1, 0.1),  discrete = FALSE),
    wp                   = list(label = "Win Prob",     breaks = seq(0, 1, 0.1),  discrete = FALSE),
    yardline_100         = list(label = "Yardline",     breaks = seq(0, 100, 10), discrete = FALSE),
    shotgun              = list(label = "Shotgun",                                discrete = TRUE),
    n_ol                 = list(label = "# OL",                                   discrete = TRUE),
    n_te                 = list(label = "# TE",                                   discrete = TRUE),
    down                 = list(label = "Down",                                   discrete = TRUE),
    ydstogo_group = list(label = "Yds-to-Go", discrete = TRUE,
                         levels = c("1st9", "1st10", "1stLong",
                                    "2nd2", "2nd3-5", "2nd6-10", "2ndLong",
                                    "3rd1", "3rd2", "3rd3", "3rdRest",
                                    "4th1", "4thLong"))
  )
  all_vars <- c("part_predicted_xpass", "wp", "yardline_100", "n_ol", "n_te")
  
  if (!outcome %in% names(outcome_lab))
    stop("outcome must be one of: ", paste(names(outcome_lab), collapse = ", "))
  if (bucket != "all" && !bucket %in% names(spec))
    stop("bucket must be 'all' or one of: ", paste(names(spec), collapse = ", "))
  
  use_vars <- if (bucket == "all") all_vars else bucket
  
  d <- df %>%
    ungroup() %>%
    filter(def_ssn == !!def_ssn) %>%
    mutate(
      ypc_oe_part = yards_gained - part_predicted_ypc,
      ypc_oe_pbp  = yards_gained - pbp_predicted_ypc
    )
  
  if (!is.null(wp_range)) d <- d %>% filter(wp >= wp_range[1], wp <= wp_range[2])
  
  ylim_vals <- quantile(d[[outcome]], probs = clip, na.rm = TRUE)
  
  one_block <- function(v) {
    s  <- spec[[v]]
    br <- if (!is.null(breaks) && length(use_vars) == 1) breaks else s$breaks
    if (isTRUE(s$discrete)) {
      lab  <- as.character(d[[v]])
      sort <- if (!is.null(s$levels)) match(lab, s$levels) else as.numeric(lab)
    } else {
      cx   <- cut(d[[v]], breaks = br, include.lowest = TRUE, right = FALSE, dig.lab = 4)
      sort <- as.integer(cx);     lab <- as.character(cx)
    }
    tibble(var = s$label, sort = sort, bin_lab = lab, y = d[[outcome]])
  }
  
  lvl  <- vapply(use_vars, function(v) spec[[v]]$label, character(1))
  long <- bind_rows(lapply(use_vars, one_block)) %>%
    filter(!is.na(bin_lab), !is.na(y)) %>%
    mutate(var = factor(var, levels = lvl),
           xr  = .reorder_within(bin_lab, sort, var))
  
  cell_n <- long %>% count(var, xr, name = "n")
  
  sub <- sprintf("%s%s", outcome_lab[[outcome]],
                 if (!is.null(wp_range)) sprintf("  |  wp %.2f–%.2f", wp_range[1], wp_range[2]) else "")
  
  zero_line <- outcome %in% c("epa", "ypc_oe_part", "ypc_oe_pbp")
  single    <- length(use_vars) == 1
  
  p <- ggplot(long, aes(x = xr, y = y))
  
  if (zero_line)
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50", linewidth = 0.4)
  
  p +
    geom_violin(width = 0.8, alpha = 0.18, fill = "#08519c", color = NA,
                trim = TRUE, scale = "width") +
    geom_boxplot(width = 0.22, alpha = 0.9, fill = "#08519c",
                 outlier.shape = NA, color = "grey20", linewidth = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 1.6,
                 fill = "white", color = "black") +
    geom_text(data = cell_n, aes(x = xr, y = -Inf, label = n),
              vjust = -0.8, size = 2.4, color = "grey35", inherit.aes = FALSE) +
    .scale_x_reordered() +
    coord_cartesian(ylim = ylim_vals) +
    facet_wrap(~ var, scales = "free_x", ncol = if (single) 1 else 2) +
    labs(title = paste0(def_ssn, " — Rush Distribution"),
         subtitle = sub,
         caption = sprintf("Box = median + IQR  |  diamond = mean  |  violin = shape  |  view clipped to %.0f–%.0f pctl (all plays kept)  |  number = rushes in bin",
                           100 * clip[1], 100 * clip[2]),
         x = NULL, y = outcome_lab[[outcome]]) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x      = element_text(angle = 35, hjust = 1, size = 7),
          strip.text       = element_text(face = "bold", size = 10),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 14),
          plot.caption     = element_text(size = 7, color = "grey40"))
}

# single bucket — deciles really shine here
plot_def_ssn_dist("SEA2025", outcome = "epa", bucket = "yardline_100")

# all faceted (dense at deciles, but there if you want the overview)
plot_def_ssn_dist("SEA2025", outcome = "ypc_oe_part", bucket = "part_predicted_xpass")

# all faceted (dense at deciles, but there if you want the overview)
plot_def_ssn_dist("SEA2025", outcome = "ypc_oe_part", bucket = "yardline_100")
