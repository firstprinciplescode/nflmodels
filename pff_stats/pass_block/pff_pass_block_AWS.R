

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


percent_rank_avg <- function(x) {
  n_valid <- sum(!is.na(x))
  if (n_valid <= 1) return(rep(0.5, length(x)))
  (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
}


all_pass_block_summary <- all_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  mutate(
    player_grade_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(grades_pass_block)),
    player_pressure_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-pressure_pct)),
    player_hurries_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-hurries_pct))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_grade_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(grades_pass_block)),
    player_pressure_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-pressure_pct)),
    player_hurries_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-hurries_pct))
  ) %>%
  ungroup()

tps_pass_block_summary <- tps_pass_block_summary %>%
  group_by(player, player_id, det_position, qbgrp_ssn, season) %>%
  mutate(
    player_tps_grade_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_block)),
    player_tps_pressure_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-true_pass_set_pressure_pct)),
    player_tps_hurries_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-true_pass_set_hurries_pct))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, det_position) %>%
  mutate(
    player_tps_grade_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_block)),
    player_tps_pressure_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-true_pass_set_pressure_pct)),
    player_tps_hurries_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(-true_pass_set_hurries_pct))
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
  mutate(grade_season_pctl = percent_rank_avg(grade_perc),
         pressure_season_pctl = percent_rank_avg(pressure_perc),
         hurries_season_pctl = percent_rank_avg(hurries_perc)) %>%
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
  mutate(grade_season_pctl = percent_rank_avg(grade_perc),
         pressure_season_pctl = percent_rank_avg(pressure_perc),
         hurries_season_pctl = percent_rank_avg(hurries_perc)) %>%
  ungroup()


minmax01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

pass_rush_all_opp_percentile <- pass_rush_all_opp_percentile %>%
  mutate(season = as.integer(sub(".*-(\\d{4})$", "\\1", qbgrp_ssn))) %>%
  group_by(season) %>%
  mutate(across(matches("_perc_(DI|ED|LB)$"),
                minmax01,
                .names = "{.col}_scaled")) %>%
  ungroup()

pass_rush_tps_opp_percentile <- pass_rush_tps_opp_percentile %>%
  mutate(season = as.integer(sub(".*-(\\d{4})$", "\\1", qbgrp_ssn))) %>%
  group_by(season) %>%
  mutate(across(matches("_perc_(DI|ED|LB)$"),
                minmax01,
                .names = "{.col}_scaled")) %>%
  ungroup()

pass_rush_all_opp_percentile <- pass_rush_all_opp_percentile %>%
  mutate(across(matches("_perc_(DI|ED|LB)$"),
                percent_rank_avg,
                .names = "{.col}_rank"))

pass_rush_tps_opp_percentile <- pass_rush_tps_opp_percentile %>%
  mutate(across(matches("_perc_(DI|ED|LB)$"),
                percent_rank_avg,
                .names = "{.col}_rank"))


View(all_pass_block_player_season_summary %>% filter(player_id == 124034))
View(tps_pass_block_player_season_summary %>% filter(player_id == 83018))

View(pass_rush_tps_opp_percentile %>% filter(qbgrp_ssn == "DENNix-2025")) 
View(pass_rush_all_opp_percentile %>% filter(qbgrp_ssn == "DETGoff-2024"))

View(qb_stats_df_final %>% filter(qbgrp_ssn == "DETGoff-2025") %>% select(pressure_rate_rank_def, less_rate_rank_def, sack_rate_rank_def))


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
      ok <- tryCatch(
        { suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE)); TRUE },
        error = function(e) FALSE, warning = function(w) FALSE
      )
      if (!ok && file.exists(dest)) try(file.remove(dest), silent = TRUE)
    }
    if (!file.exists(dest)) return(NA_character_)
    if (file.size(dest) < 100) { try(file.remove(dest), silent = TRUE); return(NA_character_) }
    bytes <- tryCatch({
      con <- file(dest, "rb"); on.exit(close(con), add = TRUE)
      readBin(con, "raw", 8)
    }, error = function(e) raw(0))
    png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
    if (length(bytes) == 8 && identical(bytes, png_magic)) gsub("\\\\", "/", dest)
    else { try(file.remove(dest), silent = TRUE); NA_character_ }
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
  
  pd <- pd %>% left_join(hs_map %>% select(player_id, strip_label), by = "player_id")
  
  x_min <- min(pd$season); x_max <- max(pd$season)
  
  ggplot() +
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
    scale_y_continuous(limits = c(0, 1.08), breaks = c(0, 0.5, 1), labels = scales::percent) +
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


ol_ids <- c(46275, 11775, 112915, 41024, 59738, 41482)

# True pass set only
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "grade_season_pctl",    "SEA — True Pass Set Grade Pctl")
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "pressure_season_pctl", "SEA — True Pass Set Pressure Pctl")


ol_ids <- c(41665, 174220)

# True pass set only
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "grade_season_pctl",    "NE Signings — True Pass Set Grade Pctl")
plot_ol_pass_block(ol_ids, tps_pass_block_player_season_summary, "pressure_season_pctl", "NE Signings — True Pass Set Pressure Pctl")



View(pass_rush_tps_opp_percentile %>% 
       filter(qbgrp_ssn %in% c("DENNix-2025")))

ne_long <- bind_rows(
  pass_rush_all_opp_percentile %>%
    mutate(across(matches("_perc_(DI|ED|LB)$"),
                  percent_rank_avg, .names = "{.col}_rank")) %>%
    filter(qbgrp_ssn %in% c("DENNix-2025")) %>%
    select(qbgrp_ssn, ends_with("_rank")) %>%
    mutate(snap_type = "All Pass-Rush"),
  pass_rush_tps_opp_percentile %>%
    mutate(across(matches("_perc_(DI|ED|LB)$"),
                  percent_rank_avg, .names = "{.col}_rank")) %>%
    filter(qbgrp_ssn %in% c("SEADarnold-2025")) %>%
    select(qbgrp_ssn, ends_with("_rank")) %>%
    rename_with(~ sub("^tps_", "", .x)) %>%
    mutate(snap_type = "True Pass Set")
) %>%
  pivot_longer(ends_with("_rank"), names_to = "var", values_to = "pctl") %>%
  mutate(
    position  = sub(".*_perc_(DI|ED|LB)_rank$", "\\1", var),
    metric    = sub("_perc_(DI|ED|LB)_rank$", "", var),
    season    = sub("^.*-", "", qbgrp_ssn),
    metric    = factor(metric,
                       levels = c("grade_pass_rush","prp","pass_rush_win_rate",
                                  "pressure_rate","hurry_rate"),
                       labels = c("Grade","PRP","Win Rate","Pressure %","Hurry %")),
    position  = factor(position, levels = c("DI","ED","LB")),
    snap_type = factor(snap_type, levels = c("All Pass-Rush","True Pass Set"))
  )

ggplot(ne_long, aes(position, metric, fill = pctl)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f", pctl * 100),
                color = abs(pctl - 0.5) > 0.3),
            size = 3.8, fontface = "bold") +
  scale_color_manual(values = c("grey20","white"), guide = "none") +
  scale_fill_gradient2(
    low = "#1a5490", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0.5, limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Defender\nPercentile"
  ) +
  facet_grid(snap_type ~ season, switch = "y") +
  labs(
    title = "NE Maye Offense -- Opposing Pass-Rush Production",
    subtitle = "Higher (red) = defenders did better vs NE than their season norm  |  Lower (blue) = OL/QB suppressed them",
    x = "Defender Position", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 15),
    plot.subtitle     = element_text(color = "grey30", size = 10),
    strip.text        = element_text(face = "bold", size = 11),
    strip.placement   = "outside",
    panel.grid        = element_blank(),
    axis.text         = element_text(size = 10),
    legend.position   = "right"
  )

ggplot(ne_long, aes(position, metric, fill = pctl)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f", pctl * 100),
                color = abs(pctl - 0.5) > 0.3),
            size = 3.8, fontface = "bold") +
  scale_color_manual(values = c("grey20","white"), guide = "none") +
  scale_fill_gradient2(
    low = "#1a5490", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0.5, limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Defender\nPercentile"
  ) +
  facet_grid(snap_type ~ season, switch = "y") +
  labs(
    title = "DET Goff Offense — Opposing Pass-Rush Production",
    subtitle = "Higher (red) = defenders did better vs NE than their season norm  |  Lower (blue) = OL/QB suppressed them",
    x = "Defender Position", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 15),
    plot.subtitle     = element_text(color = "grey30", size = 10),
    strip.text        = element_text(face = "bold", size = 11),
    strip.placement   = "outside",
    panel.grid        = element_blank(),
    axis.text         = element_text(size = 10),
    legend.position   = "right"
  )

ne_protection <- qb_stats_df_final %>% 
  filter(posteam == "DEN", season %in% c(2025)) %>%
  select(qbgrp_ssn, week, season, defteam,
         # Scheme tendencies
         less_rate,         less_rate_rank_def,
         pa_rate,           pa_rate_rank_def,
         behind_los_rate,   behind_los_rate_rank_def,
         short_rate,        short_rate_rank_def,
         adot,              adot_rank_def,
         # Expected pressure 
         pbp_pressure,          pbp_pressure_rank_def,
         part_pressure_before,  part_pressure_before_rank_def,
         part_pressure_after,   part_pressure_after_rank_def,
         # Actual
         pressure_rate,     pressure_rate_rank_def,
         sack_rate,         sack_rate_rank_def,
         # Splits
         less_pressure_rate,         more_pressure_rate,
         less_pressure_rate_rank_def, more_pressure_rate_rank_def,
         npa_pressure_rate,          pa_pressure_rate,
         npa_pressure_rate_rank_def, pa_pressure_rate_rank_def,
         #
         pressure_gr_rank_def, no_pressure_gr_rank_def,
         less_gr_rank_def, more_gr_rank_def,
         npa_gr_rank_def, pa_gr_rank_def
         
         )

ne_protection %>%
  group_by(season) %>%
  summarise(
    n_games = n(),
    across(ends_with("_rank_def"), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    .groups = "drop"
  )

## SHOULD ... PROBABLY ADD ... TTT TO ALL THESE METRICS



# Build the game-level frame
ne_games <- qb_stats_df_final %>% 
  filter(posteam == "KC", season %in% c(2025)) %>%
  select(qbgrp_ssn, week, season, defteam, tds, pbp_xtds, part_xtds,
         # Scheme
         less_rate_rank_def, pa_rate_rank_def, behind_los_rate_rank_def,
         adot_rank_def, no_huddle_rank_def,
         # Expected pressure
         pbp_pressure_rank_def, part_pressure_before_rank_def, 
         part_pressure_after_rank_def,
         # Actual pressure
         pressure_rate_rank_def, sack_rate_rank_def,
         # Pressure splits
         less_pressure_rate_rank_def, more_pressure_rate_rank_def,
         pa_pressure_rate_rank_def, npa_pressure_rate_rank_def,
         # TD production
         pbp_xtds_rank_def, part_xtds_rank_def,
         # Goff grades by context
         pressure_gr_rank_def, no_pressure_gr_rank_def,
         less_gr_rank_def, more_gr_rank_def,
         pa_gr_rank_def, npa_gr_rank_def)

# Scatter helper
plot_det <- function(df, x_var, y_var, x_lab = x_var, y_lab = y_var, title = NULL) {
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], color = factor(season))) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_smooth(method = "lm", se = FALSE, aes(group = season),
                linewidth = 0.5, linetype = "dashed", alpha = 0.6) +
    geom_point(size = 3, alpha = 0.85) +
    ggrepel::geom_text_repel(
      aes(label = paste0("W", week, " ", defteam)),
      size = 2.8, show.legend = FALSE, max.overlaps = 20, segment.size = 0.2
    ) +
    scale_color_manual(values = c("2024" = "#1f77b4", "2025" = "#d62728"), name = "Season") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    labs(x = x_lab, y = y_lab, title = title) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 12))
}

# 1. Quick game suppresses pressure? Positive slope = yes.
p1 <- plot_det(ne_games, "less_rate_rank_def", "pressure_rate_rank_def",
               "Quick Game Rate (vs def avg)", "Pressure Suppression",
               "More quick game → less pressure?")

# 2. PA suppresses pressure? Positive slope = yes.
p2 <- plot_det(ne_games, "pa_rate_rank_def", "pressure_rate_rank_def",
               "PA Rate (vs def avg)", "Pressure Suppression",
               "More PA → less pressure?")

# 3. Pressure quality → TD production? Positive slope = protection drives scoring.
p3 <- plot_det(ne_games, "pressure_rate_rank_def", "pbp_xtds_rank_def",
               "Pressure Suppression", "Expected TDs (pbp)",
               "Protection → expected TDs?")

# 4. Quick game → TD production? Floor analysis.
p4 <- plot_det(ne_games, "less_rate_rank_def", "pbp_xtds_rank_def",
               "Quick Game Rate", "Expected TDs (pbp)",
               "Does quick game cap TD upside?")

patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2, guides = "collect")


# League-level aggregate, one row per qbgrp_ssn
league_protect <- qb_stats_df_final %>%
  group_by(qbgrp_ssn, posteam, season) %>%
  summarise(
    n_games  = n(),
    less_rate = mean(less_rate_rank_def, na.rm = TRUE),
    pa_rate   = mean(pa_rate_rank_def, na.rm = TRUE),
    adot      = mean(adot_rank_def, na.rm = TRUE),
    pressure  = mean(pressure_rate_rank_def, na.rm = TRUE),
    sack      = mean(sack_rate_rank_def, na.rm = TRUE),
    xtds      = mean(pbp_xtds_rank_def, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_games >= 8) %>%                       # full(ish) season only
  mutate(is_det = grepl("^DEN", qbgrp_ssn))

# Helper
plot_league <- function(df, x_var, y_var, x_lab, y_lab, title) {
  ggplot(df, aes(.data[[x_var]], .data[[y_var]])) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey80", linewidth = 0.3) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey80", linewidth = 0.3) +
    geom_smooth(method = "lm", se = TRUE, color = "grey40",
                fill = "grey85", linewidth = 0.5) +
    geom_point(data = filter(df, !is_det), color = "grey70", size = 1.8, alpha = 0.5) +
    geom_point(data = filter(df, is_det), color = "#0076b6", size = 4) +
    ggrepel::geom_text_repel(
      data = filter(df, is_det),
      aes(label = qbgrp_ssn), color = "#0076b6",
      fontface = "bold", size = 3.4, segment.size = 0.3, min.segment.length = 0
    ) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    labs(x = x_lab, y = y_lab, title = title,
         subtitle = "Each dot = team-QB-season  |  Blue = DET") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(color = "grey40", size = 9))
}

pl1 <- plot_league(league_protect, "less_rate", "pressure",
                   "Quick Game Rate (mean rank_def)", "Pressure Suppression",
                   "Quick game → protection: where does NE sit?")

pl2 <- plot_league(league_protect, "pa_rate", "pressure",
                   "PA Rate (mean rank_def)", "Pressure Suppression",
                   "PA → protection: where does NE sit?")

patchwork::wrap_plots(pl1, pl2, ncol = 2)



det_games_25 <- qb_stats_df_final %>%
  filter(posteam == "DEN", season == 2025) %>%
  select(qbgrp_ssn, week, season, defteam,
         less_rate_rank_def, pa_rate_rank_def, adot_rank_def,
         blitz_rate_rank_def,
         pressure_rate_rank_def, sack_rate_rank_def,
         blitz_pressure_rate_rank_def, no_blitz_pressure_rate_rank_def,
         pbp_pressure_rank_def, part_pressure_before_rank_def, 
         part_pressure_after_rank_def,
         pbp_xtds_rank_def, part_xtds_rank_def,
         blitz_gr_rank_def, no_blitz_gr_rank_def)

b1 <- plot_det(det_games_25, "blitz_rate_rank_def", "pressure_rate_rank_def",
               "Blitz Rate Faced (vs def avg)", "Pressure Suppression",
               "More blitzing → more pressure?")

b2 <- plot_det(det_games_25, "less_rate_rank_def", "blitz_pressure_rate_rank_def",
               "Quick Game Rate", "Blitz Pressure Suppression",
               "Quick game beat the blitz?")

b3 <- plot_det(det_games_25, "pa_rate_rank_def", "blitz_pressure_rate_rank_def",
               "PA Rate", "Blitz Pressure Suppression",
               "PA beat the blitz?")

b4 <- plot_det(det_games_25, "blitz_rate_rank_def", "pbp_xtds_rank_def",
               "Blitz Rate Faced", "Expected TDs (pbp)",
               "Blitz rate → TD upside?")

patchwork::wrap_plots(b1, b2, b3, b4, ncol = 2)



# Rebuild league_protect with blitz metrics included
league_protect <- qb_stats_df_final %>%
  group_by(qbgrp_ssn, posteam, season) %>%
  summarise(
    n_games           = n(),
    less_rate         = mean(less_rate_rank_def, na.rm = TRUE),
    pa_rate           = mean(pa_rate_rank_def, na.rm = TRUE),
    adot              = mean(adot_rank_def, na.rm = TRUE),
    blitz_rate        = mean(blitz_rate_rank_def, na.rm = TRUE),
    pressure          = mean(pressure_rate_rank_def, na.rm = TRUE),
    sack              = mean(sack_rate_rank_def, na.rm = TRUE),
    blitz_pressure    = mean(blitz_pressure_rate_rank_def, na.rm = TRUE),
    no_blitz_pressure = mean(no_blitz_pressure_rate_rank_def, na.rm = TRUE),
    xtds              = mean(pbp_xtds_rank_def, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_games >= 8) %>%
  mutate(is_det = grepl("^DEN", qbgrp_ssn))

# Same plot_league helper from before
pl_b1 <- plot_league(league_protect, "less_rate", "blitz_pressure",
                     "Quick Game Rate (mean rank_def)", "Blitz Pressure Suppression",
                     "Quick game → blitz protection: where does NE sit?")

pl_b2 <- plot_league(league_protect, "pa_rate", "blitz_pressure",
                     "PA Rate (mean rank_def)", "Blitz Pressure Suppression",
                     "PA → blitz protection: where does NE sit?")

patchwork::wrap_plots(pl_b1, pl_b2, ncol = 2)
