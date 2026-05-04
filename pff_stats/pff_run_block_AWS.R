library(ggpath)
library(ggimage)
library(ggtext)

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
    team_name = paste(sort(unique(team_name)), collapse = "/"),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%
  group_by(det_position, season) %>%
  mutate(gap_season_pctl = percent_rank(gap_perc)) %>%
  ungroup()

zone_player_season_summary <- zone_block_summary %>%
  group_by(player, player_id, det_position, season) %>%
  summarise(
    zone_perc = mean(player_zone_def_ssn_perc),
    n = n(),
    team_name = paste(sort(unique(team_name)), collapse = "/"),
    .groups = "drop"
  ) %>%
  filter(n >= 6) %>%
  group_by(det_position, season) %>%
  mutate(zone_season_pctl = percent_rank(zone_perc)) %>%
  ungroup()


View(gap_player_season_summary %>% filter(season == 2025, player_id %in% c(124034, 81788, 10650, 10729, 98261, 44909, 12307)))
View(zone_player_season_summary %>% filter(season == 2025, player_id %in% c(124034, 81788, 10650, 10729, 98261, 44909, 12307)))

View(gap_player_season_summary %>% filter(player_id %in% c(124034, 81788, 10650, 10729, 98261, 44909, 12307)) %>% arrange(player_id, season))
View(zone_player_season_summary %>% filter(player_id %in% c(124034, 81788, 10650, 10729, 98261, 44909, 12307)) %>% arrange(player_id, season))

View(gap_player_season_summary %>% filter(player_id %in% c(81995, 59879)))
View(zone_player_season_summary %>% filter(player_id %in% c(81995, 59879)))

View(run_defense_opp_position_percentile %>% filter(qbgrp_ssn == "DETGoff-2025"))


View(gap_player_season_summary %>% filter(player_id == 98261))
View(zone_player_season_summary %>% filter(player_id == 98261))
View(gap_opp_position_percentile %>% filter(def_ssn == "PHI2025"))
View(zone_opp_position_percentile %>% filter(def_ssn == "PHI2025"))


View(rush_stats_high %>% filter(off_ssn == "DET2025"))

View(rush_stats_final %>% filter(team == "DET", season == 2025))


# install.packages("nflplotR")  # if needed

# Lookup: PFF team code -> nflverse team code (so we can pull team colors)
pff_to_nflverse <- c(
  ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU",
  SD  = "LAC", OAK = "LV"
)

# Helper: build a season-level background df keyed off whatever bg_by we choose
build_bg_df <- function(pd) {
  pff_to_nflverse <- c(
    ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU",
    SD  = "LAC", OAK = "LV"
  )
  
  bg <- pd %>%
    distinct(player_id, season, team_name) %>%
    mutate(team_nflverse = dplyr::coalesce(pff_to_nflverse[team_name], team_name))
  
  team_color_lookup <- nflreadr::load_teams() %>%
    dplyr::select(team_abbr, team_color) %>%
    tibble::deframe()
  
  bg %>%
    mutate(bg_color = team_color_lookup[team_nflverse]) %>%
    mutate(bg_color = ifelse(is.na(bg_color), "#cccccc", bg_color)) %>%
    mutate(bg_color = scales::alpha(bg_color, 0.18))
}


plot_ol_run_block <- function(player_ids,
                              data = gap_player_season_summary,
                              metric = "gap_season_pctl",
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

# DET 2025
ol_ids <- c(124034, 81788, 10650, 10729, 98261, 44909, 12307)
plot_ol_run_block(ol_ids, gap_player_season_summary, "gap_season_pctl",
                  "DET — Run Block Gap Pctl")
plot_ol_run_block(ol_ids, zone_player_season_summary, "zone_season_pctl", "DET — Run Block Zone Pctl")


# DET FA SIGNINGS
ol_ids <- c(81995, 59879, 84236)
plot_ol_run_block(ol_ids, gap_player_season_summary,  "gap_season_pctl",  "DET FA Signings — Run Block Gap Pctl")
plot_ol_run_block(ol_ids, zone_player_season_summary, "zone_season_pctl", "DET FA Signings — Run Block Zone Pctl")

# DET 2024 / 3
ol_ids <- c(81788, 10729, 39137, 7032, 10650, 37070)
plot_ol_run_block(ol_ids, gap_player_season_summary,  "gap_season_pctl",  "DET — Run Block Gap Pctl")
plot_ol_run_block(ol_ids, zone_player_season_summary, "zone_season_pctl", "DET — Run Block Zone Pctl")

# ARZ 23-25
ol_ids <- c(44959, 129749, 38709, 158192, 7229, 41651, 46180, 94574, 46238)
plot_ol_run_block(ol_ids, gap_player_season_summary,  "gap_season_pctl",  "ARZ — Run Block Gap Pctl")
plot_ol_run_block(ol_ids, zone_player_season_summary, "zone_season_pctl", "ARZ — Run Block Zone Pctl")

# I COULD SAY ... SHIT. THE BEFORE / AFTER - SOME ROOM FOR ... DISCOMFORT - WHAT WILL COME NEXT? 


plot_team_rush_off <- function(team_season,
                               data = rush_stats_high,
                               title = NULL) {
  
  ssn <- as.numeric(sub("^[A-Z]+", "", team_season))
  
  pd <- data %>%
    filter(off_ssn == team_season) %>%
    arrange(week) %>%
    mutate(
      opp = sub(as.character(ssn), "", def_ssn),
      week_lab = paste0("W", week, "\n", opp),
      week_lab = forcats::fct_reorder(week_lab, week)
    ) %>%
    select(week_lab, attempts, ybc_rank_def, yac_rank_def, ypc_rank_def) %>%
    pivot_longer(c(ybc_rank_def, yac_rank_def),
                 names_to = "metric", values_to = "rank") %>%
    mutate(metric = factor(recode(metric,
                                  ybc_rank_def = "YBC (line)",
                                  yac_rank_def = "YAC (back)"),
                           levels = c("YBC (line)", "YAC (back)")))
  
  ggplot(pd, aes(week_lab, rank, fill = metric)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_text(aes(label = scales::percent(rank, accuracy = 1)),
              position = position_dodge(width = 0.85),
              vjust = -0.3, size = 2.3, color = "grey25") +
    scale_fill_manual(values = c("YBC (line)" = "#08519c", "YAC (back)" = "#a63603"),
                      name = NULL) +
    scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.5, 1), labels = scales::percent) +
    labs(title = title %||% paste0(team_season, " — Rushing offense by game"),
         subtitle = "Rank within opponent's season (higher = beat their defense's norm)",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 16),
      plot.subtitle      = element_text(color = "grey40", size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom"
    )
}

plot_team_rush_off("DET2025")
plot_team_rush_off("CHI2025")
plot_team_rush_off("DET2024")
plot_team_rush_off("DET2023")


plot_team_rush <- function(team_season,
                           data = rush_stats_high,
                           side = c("off", "def"),
                           sort_by = c("week", "ybc", "yac", "ypc"),
                           title = NULL) {
  
  side    <- match.arg(side)
  sort_by <- match.arg(sort_by)
  
  ssn <- as.numeric(sub("^[A-Z]+", "", team_season))
  
  if (side == "off") {
    pd <- data %>% filter(off_ssn == team_season) %>%
      mutate(opp = sub(as.character(ssn), "", def_ssn))
    ybc_col <- "ybc_rank_def"; yac_col <- "yac_rank_def"; ypc_col <- "ypc_rank_def"
    fills   <- c("YBC (line)" = "#08519c", "YAC (back)" = "#a63603")
    lvls    <- c("YBC (line)", "YAC (back)")
    base_sub <- "Rank within opponent's season (higher = beat their defense's norm)"
    ttl     <- title %||% paste0(team_season, " — Rushing offense by game")
    descending <- TRUE
  } else {
    pd <- data %>% filter(def_ssn == team_season) %>%
      mutate(opp = sub(as.character(ssn), "", off_ssn))
    ybc_col <- "ybc_rank"; yac_col <- "yac_rank"; ypc_col <- "ypc_rank"
    fills   <- c("YBC allowed" = "#08519c", "YAC allowed" = "#a63603")
    lvls    <- c("YBC allowed", "YAC allowed")
    base_sub <- "Rank within opponent's season (higher = worse defense)"
    ttl     <- title %||% paste0(team_season, " — Rushing defense by game")
    descending <- FALSE
  }
  
  sort_col <- switch(sort_by,
                     week = "week",
                     ybc  = ybc_col,
                     yac  = yac_col,
                     ypc  = ypc_col)
  
  sort_label <- switch(sort_by,
                       week = "chronological",
                       ybc  = if (side == "off") "best YBC \u2192 worst" else "fewest YBC allowed \u2192 most",
                       yac  = if (side == "off") "best YAC \u2192 worst" else "fewest YAC allowed \u2192 most",
                       ypc  = if (side == "off") "best YPC \u2192 worst" else "fewest YPC allowed \u2192 most")
  
  sub_txt <- paste0(base_sub, "\nSorted: ", sort_label)
  
  pd <- pd %>%
    mutate(.sort_val = .data[[sort_col]],
           .sort_val = if (sort_by == "week") .sort_val
           else if (descending) -.sort_val
           else .sort_val) %>%
    arrange(.sort_val) %>%
    mutate(week_lab = factor(paste0("W", week, "\n", opp),
                             levels = paste0("W", week, "\n", opp))) %>%
    select(week_lab, attempts, all_of(c(ybc_col, yac_col))) %>%
    pivot_longer(all_of(c(ybc_col, yac_col)),
                 names_to = "metric", values_to = "rank") %>%
    mutate(metric = factor(
      recode(metric, !!ybc_col := lvls[1], !!yac_col := lvls[2]),
      levels = lvls
    ))
  
  ggplot(pd, aes(week_lab, rank, fill = metric)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_text(aes(label = scales::percent(rank, accuracy = 1)),
              position = position_dodge(width = 0.85),
              vjust = -0.3, size = 2.3, color = "grey25") +
    scale_fill_manual(values = fills, name = NULL) +
    scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.5, 1), labels = scales::percent) +
    labs(title = ttl, subtitle = sub_txt, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 16),
      plot.subtitle      = element_text(color = "grey40", size = 10, lineheight = 1.2),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom"
    )
}

plot_team_rush("DET2025", side = "off", sort_by = "week")
plot_team_rush("DET2025", side = "off", sort_by = "ybc")
plot_team_rush("DET2025", side = "off", sort_by = "ypc")
plot_team_rush("DET2025", side = "off", sort_by = "yac")

plot_team_rush("CHI2025", side = "off", sort_by = "week")
plot_team_rush("CHI2025", side = "off", sort_by = "ybc")
plot_team_rush("CHI2025", side = "off", sort_by = "ypc")
plot_team_rush("CHI2025", side = "off", sort_by = "yac")

plot_team_rush("CHI2024", side = "off", sort_by = "week")
plot_team_rush("CHI2024", side = "off", sort_by = "ybc")
plot_team_rush("CHI2024", side = "off", sort_by = "ypc")
plot_team_rush("CHI2024", side = "off", sort_by = "yac")

plot_team_rush("DET2024", side = "off", sort_by = "week")
plot_team_rush("DET2024", side = "off", sort_by = "ybc")
plot_team_rush("DET2024", side = "off", sort_by = "ypc")
plot_team_rush("DET2024", side = "off", sort_by = "yac")

plot_team_rush("DET2023", side = "off", sort_by = "week")
plot_team_rush("DET2023", side = "off", sort_by = "ybc")
plot_team_rush("DET2023", side = "off", sort_by = "ypc")
plot_team_rush("DET2023", side = "off", sort_by = "yac")

plot_team_rush("ARZ2025", side = "off", sort_by = "week")
plot_team_rush("ARZ2025", side = "off", sort_by = "ybc")
plot_team_rush("ARZ2025", side = "off", sort_by = "ypc")
plot_team_rush("ARZ2025", side = "off", sort_by = "yac")

plot_team_rush("ARZ2024", side = "off", sort_by = "week")
plot_team_rush("ARZ2024", side = "off", sort_by = "ybc")
plot_team_rush("ARZ2024", side = "off", sort_by = "ypc")
plot_team_rush("ARZ2024", side = "off", sort_by = "yac")

plot_team_rush("ARZ2023", side = "off", sort_by = "week")
plot_team_rush("ARZ2023", side = "off", sort_by = "ybc")
plot_team_rush("ARZ2023", side = "off", sort_by = "ypc")
plot_team_rush("ARZ2023", side = "off", sort_by = "yac")

# THESE RESULTS REALLY GOOD THOUGH. LIKE REALLY GOOD. 



plot_ybc_yac_quadrants <- function(team_season, data = rush_stats_high) {
  ssn <- as.numeric(sub("^[A-Z]+", "", team_season))
  pd <- data %>%
    filter(off_ssn == team_season) %>%
    mutate(opp = sub(as.character(ssn), "", def_ssn))
  
  ggplot(pd, aes(ybc_rank_def, yac_rank_def)) +
    annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "#08519c", alpha = 0.06) +
    annotate("rect", xmin = 0,   xmax = 0.5, ymin = 0,   ymax = 0.5, fill = "#a63603", alpha = 0.06) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    geom_point(aes(size = attempts), color = "#08519c", alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = paste0("W", week, " ", opp)),
                             size = 3, seed = 1, max.overlaps = Inf) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = paste0(team_season, " — Run game quadrants"),
         subtitle = "Top-right: both won. Bottom-left: both lost.",
         x = "YBC rank (line)", y = "YAC rank (back)", size = "Attempts") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 10)
    )
}

plot_ybc_yac_quadrants("DET2025")
plot_ybc_yac_quadrants("CHI2025")
plot_ybc_yac_quadrants("CHI2024")
plot_ybc_yac_quadrants("DET2024")
plot_ybc_yac_quadrants("DET2023")
plot_ybc_yac_quadrants("ARZ2025")
plot_ybc_yac_quadrants("ARZ2024")
plot_ybc_yac_quadrants("ARZ2023")

# THESE RESULTS REALLY GOOD THOUGH. LIKE REALLY GOOD. 


plot_def_pos_heatmap <- function(qbgrp,
                                 data = run_defense_opp_position_percentile,
                                 exclude_positions = c("CB"),
                                 title = NULL) {
  
  metric_lookup <- c(
    player_grade_run_def_perc       = "Run def grade",
    player_grade_tackle_perc        = "Tackle grade",
    player_stop_pct_perc            = "Stop %",
    player_tackle_pct_perc          = "Tackle %",
    player_assists_pct_perc         = "Assist %",
    player_missed_tackle_rate_perc  = "Missed tkl rate*",
    player_avg_depth_of_tackle_perc = "Avg depth of tkl*"
  )
  
  pd <- data %>%
    filter(qbgrp_ssn == qbgrp) %>%
    pivot_longer(-qbgrp_ssn, names_to = "var", values_to = "pctl") %>%
    tidyr::extract(var, into = c("metric_key", "position"),
                   regex = "^(.*)_([A-Z]+)$") %>%
    filter(position %ni% exclude_positions, !is.na(pctl)) %>%
    mutate(
      metric   = factor(metric_lookup[metric_key], levels = unname(metric_lookup)),
      position = factor(position, levels = c("DI","ED","LB","S"))
    )
  
  ggplot(pd, aes(position, metric, fill = pctl)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = scales::percent(pctl, accuracy = 1),
                  color = abs(pctl - 0.5) > 0.25),
              size = 4, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(
      low = "#08519c", mid = "#f7f7f7", high = "#a63603",
      midpoint = 0.5, limits = c(0, 1),
      labels = scales::percent, name = "Pctl"
    ) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(
      title = title %||% paste0(qbgrp, " — Opposing run defenders' avg pctl"),
      subtitle = "Blue = DET made them look worse than their season norm (good for DET)\n*Inverted in source: high pctl already means fewer missed tackles / shorter depth",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 16),
      plot.subtitle      = element_text(color = "grey40", size = 9, lineheight = 1.2),
      panel.grid         = element_blank(),
      axis.text.x.top    = element_text(face = "bold", size = 12),
      axis.text.y        = element_text(size = 11),
      legend.position    = "right",
      legend.key.height  = unit(1.5, "cm")
    )
}

plot_def_pos_heatmap("DETGoff-2025")
plot_def_pos_heatmap("DETGoff-2024")
plot_def_pos_heatmap("DETGoff-2023")
plot_def_pos_heatmap("CHIWilliams-2025")
plot_def_pos_heatmap("CHIWilliams-2024")

plot_def_pos_heatmap("ARZDobbs-2023")
plot_def_pos_heatmap("ARZMurray-2023")
plot_def_pos_heatmap("ARZMurray-2024")
plot_def_pos_heatmap("ARZBrissett-2025")

# THE WEAKNESS - ALWAYS THE INTERIOR. GIVEN THIS + THE GRADES ... MAHOGANY / CADE MAYS ... THIS IS THE ... THIS IS THE PART TO WORRY ABOUT. AND ... WE'LL SEE

### OVERALL - 
### COMBO OF - MASSIVE PERSONNEL TURNOVER, OL INJURIES, AGING, RBs NOT AS GOOD, ETC.
### MOVING FROM A MIXED SYSTEM TO A HUGELY GAP DOMINANT - NEVER DONE
