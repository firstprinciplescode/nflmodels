

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
  group_by(player, player_id, det_position, team_name, qbgrp_ssn, season) %>%
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
  group_by(player, player_id, det_position, team_name, qbgrp_ssn, season) %>%
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


View(all_pass_block_player_season_summary %>% filter(player_id == 124034))
View(tps_pass_block_player_season_summary %>% filter(player_id == 83018))

View(pass_rush_tps_opp_percentile %>% filter(qbgrp_ssn == "DETGoff-2025"))
View(pass_rush_all_opp_percentile %>% filter(qbgrp_ssn == "DETGoff-2024"))

View(qb_stats_df_final %>% filter(qbgrp_ssn == "DETGoff-2025") %>% select(pressure_rate_rank_def, sack_rate_rank_def))


plot_ol_pass_block <- function(player_ids,
                               data = all_pass_block_player_season_summary,
                               metric = "grade_season_pctl",
                               title = NULL,
                               cache_dir = file.path(tempdir(), "pff_headshots")) {
  
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  get_headshot <- function(pid) {
    dest <- file.path(cache_dir, paste0(pid, ".png"))
    if (!file.exists(dest)) {
      url <- paste0("https://media.pff.com/player-photos/nfl/", pid, ".png")
      try(download.file(url, dest, mode = "wb", quiet = TRUE), silent = TRUE)
    }
    if (!file.exists(dest) || file.size(dest) < 100) return(NA_character_)
    con <- file(dest, "rb"); bytes <- readBin(con, "raw", 8); close(con)
    png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
    if (identical(bytes, png_magic)) gsub("\\\\", "/", dest) else NA_character_
  }
  
  pos_colors <- c(
    LT = "#08519c", LG = "#6baed6", C  = "#525252",
    RG = "#fd8d3c", RT = "#a63603"
  )
  
  pd <- data %>%
    filter(player_id %in% player_ids) %>%
    mutate(
      val = .data[[metric]],
      det_position = factor(det_position, levels = c("LT","LG","C","RG","RT"))
    )
  
  bg_df <- build_bg_df(pd)
  
  hs_map <- pd %>%
    distinct(player_id, player) %>%
    rowwise() %>%
    mutate(headshot_path = get_headshot(player_id)) %>%
    ungroup() %>%
    mutate(
      strip_label = ifelse(
        is.na(headshot_path),
        paste0("<span style='vertical-align:middle;'>", player, "</span>"),
        paste0(
          "<img src='", headshot_path, "' width='38' ",
          "style='vertical-align:middle; margin-right:6px;'/>",
          "<span style='vertical-align:middle;'>", player, "</span>"
        )
      )
    )
  
  pd    <- pd    %>% left_join(hs_map %>% select(player_id, strip_label), by = "player_id")
  bg_df <- bg_df %>% left_join(hs_map %>% select(player_id, strip_label), by = "player_id")
  
  x_min <- min(pd$season); x_max <- max(pd$season)
  
  ggplot() +
    geom_rect(
      data = bg_df,
      aes(xmin = season - 0.5, xmax = season + 0.5,
          ymin = 0, ymax = 1.15, fill = I(bg_color)),
      inherit.aes = FALSE
    ) +
    geom_col(
      data = pd,
      aes(season, val, fill = det_position),
      position = position_dodge2(width = 0.85, preserve = "single", padding = 0),
      width = 0.8
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_text(
      data = pd,
      aes(season, val, label = scales::percent(val, accuracy = 1),
          group = det_position),
      position = position_dodge2(width = 0.85, preserve = "single", padding = 0),
      vjust = -0.4, size = 2.5, color = "grey25"
    ) +
    scale_fill_manual(values = pos_colors, drop = FALSE, name = "Position") +
    scale_y_continuous(limits = c(0, 1.15), breaks = c(0, 0.5, 1), labels = scales::percent) +
    scale_x_continuous(breaks = seq(x_min, x_max, 1),
                       expand = expansion(add = 0.5)) +
    facet_wrap(~ strip_label, ncol = 3) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 16),
      strip.text         = ggtext::element_markdown(face = "bold", size = 11, hjust = 0.5),
      strip.background   = element_rect(fill = "grey95", color = NA),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing      = unit(1.2, "lines"),
      legend.position    = "bottom"
    )
}

ol_ids <- c(124034, 81788, 10650, 10729, 98261, 44909, 39137, 7032, 37070)

# All pass block snaps
plot_ol_pass_block(ol_ids, all_pass_block_player_season_summary, "grade_season_pctl",    "DET — Pass Block Grade Pctl")
plot_ol_pass_block(ol_ids, all_pass_block_player_season_summary, "pressure_season_pctl", "DET — Pass Block Pressure Pctl")

# True pass set only
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "grade_season_pctl",    "DET — True Pass Set Grade Pctl")
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "pressure_season_pctl", "DET — True Pass Set Pressure Pctl")






