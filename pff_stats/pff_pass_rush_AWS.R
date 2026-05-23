
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


percent_rank_avg <- function(x) {
  n_valid <- sum(!is.na(x))
  if (n_valid <= 1) return(rep(0.5, length(x)))
  (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
}


pass_rush_all_player_agg <- full_pass_rush_qbgrp %>%
  filter(snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    player_grade_pass_rush_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(grades_pass_rush_defense)),
    player_prp_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(prp)),
    player_pass_rush_win_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(pass_rush_win_rate)),
    player_pressure_rate_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(pressure_rate)),
    player_hurry_rate_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(hurry_rate))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    grade_pass_rush_def_ssn_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(grades_pass_rush_defense)),
    prp_def_ssn_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(prp)),
    pass_rush_win_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(pass_rush_win_rate)),
    pressure_rate_def_ssn_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(pressure_rate)),
    hurry_rate_def_ssn_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(hurry_rate)),
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
    grade_pass_rush_perc    = mean(grade_pass_rush_def_ssn_perc, na.rm = TRUE),
    prp_perc                = mean(prp_def_ssn_perc, na.rm = TRUE),
    pass_rush_win_rate_perc = mean(pass_rush_win_rate_def_ssn_perc, na.rm = TRUE),
    pressure_rate_perc      = mean(pressure_rate_def_ssn_perc, na.rm = TRUE),
    hurry_rate_perc         = mean(hurry_rate_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position,
    values_from = c(grade_pass_rush_perc, prp_perc, pass_rush_win_rate_perc,
                    pressure_rate_perc, hurry_rate_perc)
  )

pass_rush_all_player_season_summary <- full_pass_rush_qbgrp %>%
  filter(snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_grade_pass_rush    = mean(grades_pass_rush_defense, na.rm = TRUE),
    player_prp                = mean(prp, na.rm = TRUE),
    player_pass_rush_win_rate = mean(pass_rush_win_rate, na.rm = TRUE),
    player_pressure_rate      = mean(pressure_rate, na.rm = TRUE),
    player_hurry_rate         = mean(hurry_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 8) %>%
  group_by(position, season) %>%
  mutate(
    grade_pass_rush_season_pctl    = percent_rank_avg(player_grade_pass_rush),
    prp_season_pctl                = percent_rank_avg(player_prp),
    pass_rush_win_rate_season_pctl = percent_rank_avg(player_pass_rush_win_rate),
    pressure_rate_season_pctl      = percent_rank_avg(player_pressure_rate),
    hurry_rate_season_pctl         = percent_rank_avg(player_hurry_rate)
  ) %>%
  ungroup()


####
####


pass_rush_tps_player_agg <- full_pass_rush_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    player_tps_grade_pass_rush_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_rush_defense)),
    player_tps_prp_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_prp)),
    player_tps_pass_rush_win_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_pass_rush_win_rate)),
    player_tps_pressure_rate_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_pressure_rate)),
    player_tps_hurry_rate_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hurry_rate))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    tps_grade_pass_rush_def_ssn_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_rush_defense)),
    tps_prp_def_ssn_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_prp)),
    tps_pass_rush_win_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_pass_rush_win_rate)),
    tps_pressure_rate_def_ssn_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_pressure_rate)),
    tps_hurry_rate_def_ssn_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hurry_rate)),
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
    tps_grade_pass_rush_perc    = mean(tps_grade_pass_rush_def_ssn_perc, na.rm = TRUE),
    tps_prp_perc                = mean(tps_prp_def_ssn_perc, na.rm = TRUE),
    tps_pass_rush_win_rate_perc = mean(tps_pass_rush_win_rate_def_ssn_perc, na.rm = TRUE),
    tps_pressure_rate_perc      = mean(tps_pressure_rate_def_ssn_perc, na.rm = TRUE),
    tps_hurry_rate_perc         = mean(tps_hurry_rate_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position,
    values_from = c(tps_grade_pass_rush_perc, tps_prp_perc, tps_pass_rush_win_rate_perc,
                    tps_pressure_rate_perc, tps_hurry_rate_perc)
  )

pass_rush_tps_player_season_summary <- full_pass_rush_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_tps_grade_pass_rush    = mean(true_pass_set_grades_pass_rush_defense, na.rm = TRUE),
    player_tps_prp                = mean(true_pass_set_prp, na.rm = TRUE),
    player_tps_pass_rush_win_rate = mean(true_pass_set_pass_rush_win_rate, na.rm = TRUE),
    player_tps_pressure_rate      = mean(tps_pressure_rate, na.rm = TRUE),
    player_tps_hurry_rate         = mean(tps_hurry_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 7) %>%
  group_by(position, season) %>%
  mutate(
    tps_grade_pass_rush_season_pctl    = percent_rank_avg(player_tps_grade_pass_rush),
    tps_prp_season_pctl                = percent_rank_avg(player_tps_prp),
    tps_pass_rush_win_rate_season_pctl = percent_rank_avg(player_tps_pass_rush_win_rate),
    tps_pressure_rate_season_pctl      = percent_rank_avg(player_tps_pressure_rate),
    tps_hurry_rate_season_pctl         = percent_rank_avg(player_tps_hurry_rate)
  ) %>%
  ungroup()


percent_rank_avg <- function(x) {
  n_valid <- sum(!is.na(x))
  if (n_valid <= 1) return(rep(0.5, length(x)))
  (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
}


all_pass_block_opp_position_pctl <- all_pass_block_opp_position_percentile %>%
  mutate(across(starts_with("player_"),
                ~ percent_rank_avg(.x),
                .names = "{.col}_rank"))

tps_pass_block_opp_position_pctl <- tps_pass_block_opp_position_percentile %>%
  mutate(across(starts_with("player_"),
                ~ percent_rank_avg(.x),
                .names = "{.col}_rank"))


det_def_games <- qb_stats_df_final %>%
  filter(defteam == "DET", season == 2025)

ggplot(det_def_games, aes(adot_rank_def, pbp_xtds_rank_def)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dotted") +
  geom_point(size = 3, alpha = 0.85, color = "#0076b6") +
  ggrepel::geom_text_repel(aes(label = paste0("W", week, " ", posteam)),
                           size = 2.8, color = "grey25", segment.size = 0.2) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "DET2025 Defense: pressure level → offensive TD output",
       subtitle = "Bottom-left = DET pressured + suppressed scoring  |  Top-right = no pressure + offense rolled",
       x = "Pressure suppression by offense (high = DET didn't pressure)",
       y = "Expected TDs by offense (high = offense scored a lot)") +
  theme_minimal()


# Z-score across the full population so the cohort comparison is meaningful
tps_pblk_z <- tps_pass_block_opp_position_pctl %>%
  mutate(across(ends_with("_rank"), ~ as.numeric(scale(.x))))

# Pivot long, filter to cohort
tps_pblk_long <- tps_pblk_z %>%
  filter(def_ssn %in% c(sim_def$QB, "DET2025")) %>%
  pivot_longer(ends_with("_rank"), names_to = "var", values_to = "z") %>%
  mutate(
    position = sub(".*_([A-Z]{1,2})_rank$", "\\1", var),
    metric   = case_when(
      grepl("grade",    var) ~ "Grade",
      grepl("pressure", var) ~ "Pressure",
      grepl("hurries",  var) ~ "Hurries"
    ),
    bucket    = "TPS",
    var_label = sub("_rank$", "", var) %>% sub("^player_tps_", "", .)
  ) %>%
  filter(!is.na(z))

# Order y-axis: grade rows, then pressure rows, then hurries rows
ordered_pblk <- c(
  paste0("grade_perc_",    c("LT","LG","C","RG","RT")),
  paste0("pressure_perc_", c("LT","LG","C","RG","RT")),
  paste0("hurries_perc_",  c("LT","LG","C","RG","RT"))
)
tps_pblk_long <- tps_pblk_long %>%
  mutate(var_label = factor(var_label, levels = rev(ordered_pblk)))

# Aggregate: avg across positions per metric
tps_pblk_agg <- tps_pblk_long %>%
  group_by(def_ssn, metric, bucket) %>%
  summarise(z = mean(z, na.rm = TRUE), .groups = "drop") %>%
  mutate(var_label = factor(metric, levels = rev(c("Grade","Pressure","Hurries"))))

# Summaries for dumbbells
tps_pblk_summary <- tps_pblk_long %>%
  group_by(var_label, bucket) %>%
  summarise(
    vs      = z[def_ssn == "DET2025"],
    cc      = median(z[def_ssn != "DET2025"], na.rm = TRUE),
    cc_mean = mean(z[def_ssn != "DET2025"], na.rm = TRUE),
    .groups = "drop"
  )

tps_pblk_agg_summary <- tps_pblk_agg %>%
  group_by(var_label, bucket) %>%
  summarise(
    vs      = z[def_ssn == "DET2025"],
    cc      = median(z[def_ssn != "DET2025"], na.rm = TRUE),
    cc_mean = mean(z[def_ssn != "DET2025"], na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate (3 rows)
plot_strip(df = tps_pblk_agg,         bkt = "TPS", focal = "DET2025", id_col = "def_ssn")
plot_dumb(df  = tps_pblk_agg_summary, bkt = "TPS", focal = "DET2025")

# Position-split (15 rows)
plot_strip(df = tps_pblk_long,        bkt = "TPS", focal = "DET2025", id_col = "def_ssn")
plot_dumb(df  = tps_pblk_summary,     bkt = "TPS", focal = "DET2025")


all_pblk_z <- all_pass_block_opp_position_pctl %>%
  mutate(across(ends_with("_rank"), ~ as.numeric(scale(.x))))

all_pblk_long <- all_pblk_z %>%
  filter(def_ssn %in% c(sim_def$QB, "DET2025")) %>%
  pivot_longer(ends_with("_rank"), names_to = "var", values_to = "z") %>%
  mutate(
    position  = sub(".*_([A-Z]{1,2})_rank$", "\\1", var),
    metric    = case_when(
      grepl("grade",    var) ~ "Grade",
      grepl("pressure", var) ~ "Pressure",
      grepl("hurries",  var) ~ "Hurries"
    ),
    bucket    = "All",
    var_label = sub("_rank$", "", var) %>% sub("^player_", "", .)
  ) %>%
  filter(!is.na(z)) %>%
  mutate(var_label = factor(var_label, levels = rev(ordered_pblk_all)))

# ordered_pblk_all has same structure, no "tps_" prefix needed
ordered_pblk_all <- c(
  paste0("grade_perc_",    c("LT","LG","C","RG","RT")),
  paste0("pressure_perc_", c("LT","LG","C","RG","RT")),
  paste0("hurries_perc_",  c("LT","LG","C","RG","RT"))
)

# Build summary + agg same way, run plot_strip/plot_dumb

plot_pblk_games <- function(df = all_pass_block_summary,
                            focal = "DET2025",
                            pos = "LT",
                            metric = "grade",
                            sort_by = "week") {
  
  prefix     <- if (any(grepl("^player_tps_", names(df)))) "player_tps_" else "player_"
  metric_col <- paste0(prefix, metric, "_perc")
  
  d <- df %>%
    filter(def_ssn == focal, det_position == pos) %>%
    transmute(player, week, team_name, opp,
              perc = .data[[metric_col]],
              label_raw = paste0("W", week, " ", player, " (", team_name, ")"))
  
  d <- if (sort_by == "perc") {
    d %>% arrange(perc) %>% mutate(label = factor(label_raw, levels = label_raw))
  } else {
    d %>% arrange(week) %>% mutate(label = factor(label_raw, levels = rev(label_raw)))
  }
  
  ggplot(d, aes(x = perc, y = label, fill = perc)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") +
    scale_fill_gradient(
      low    = "#deebf7",
      high   = "#08306b",
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name   = "Player\npercentile"
    ) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.25),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste0(focal, " — Opposing ", pos, "s by ",
                        tools::toTitleCase(metric), " percentile"),
         subtitle = paste0("Each bar = one game's OL vs ", focal,
                           "  |  Dark blue = OL played well (DET soft)  |  Light = DET tough"),
         x = paste0(metric, " percentile (vs player's own season)"),
         y = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.major.y = element_blank(),
          legend.position = "right")
}


# Usage
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LT", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LT", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LT", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LT", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LT", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LT", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LG", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LG", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "LG", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LG", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LG", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "LG", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "C", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "C", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "C", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "C", metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "C", metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "C", metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RG",  metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RG",  metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RG",  metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RG",  metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RG",  metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RG",  metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RT",  metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RT",  metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2025", pos = "RT",  metric = "hurries", sort_by = "perc")

plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RT",  metric = "grade", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RT",  metric = "pressure", sort_by = "perc")
plot_pblk_games(df = tps_pass_block_summary, focal = "DET2024", pos = "RT",  metric = "hurries", sort_by = "perc")


tps_pass_block_opp_position_pctl %>% filter(def_ssn %in% c("DET2024", "DET2025"))


View(qb_stats_df_final %>% filter(def_ssn == "DET2025"))


plot_prush_card <- function(team_season,
                            view   = "all",
                            df_all = pass_rush_all_player_season_summary,
                            df_tps = pass_rush_tps_player_season_summary) {
  
  df <- if (view == "tps") df_tps else df_all
  df <- df %>% filter(def_ssn == team_season)
  title_suffix <- if (view == "tps") "True Pass Set" else "All Snaps"
  
  metrics <- c("grade_pass_rush", "prp", "pass_rush_win_rate",
               "pressure_rate", "hit_rate", "hurry_rate")
  metric_labels <- c("Grade","PRP","Win Rate","Pressure %","Hit %","Hurry %")
  
  prefix    <- if (any(grepl("^tps_", names(df)))) "tps_" else ""
  pctl_cols <- paste0(prefix, metrics, "_season_pctl")
  
  d_full <- df %>%
    select(player, position, n, all_of(pctl_cols)) %>%
    rename_with(~ paste0(metrics, "_season_pctl"), all_of(pctl_cols)) %>%
    pivot_longer(ends_with("_season_pctl"),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric   = sub("_season_pctl$", "", metric),
           metric   = factor(metric, levels = metrics, labels = metric_labels),
           position = factor(position,
                             levels = c("ED", "DI", "LB"),
                             labels = c("ED", "DL", "LB")))
  
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
  
  p_ed <- build_panel("ED", show_x_strip = TRUE,  show_x_axis = FALSE)
  p_dl <- build_panel("DL", show_x_strip = FALSE, show_x_axis = FALSE)
  p_lb <- build_panel("LB", show_x_strip = FALSE, show_x_axis = TRUE)
  
  n_ed <- d_full %>% filter(position == "ED") %>% distinct(player) %>% nrow()
  n_dl <- d_full %>% filter(position == "DL") %>% distinct(player) %>% nrow()
  n_lb <- d_full %>% filter(position == "LB") %>% distinct(player) %>% nrow()
  
  library(patchwork)
  (p_ed / p_dl / p_lb) +
    plot_layout(heights = c(n_ed, n_dl, n_lb), guides = "collect") +
    plot_annotation(
      title    = paste0(team_season, " — Pass Rusher Scouting Card (", title_suffix, ")"),
      subtitle = "Each row = one rusher  |  Dark = elite season percentile",
      theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                       plot.subtitle = element_text(size = 10))
    )
}


plot_prush_players <- function(team_season,
                               metric = "grade_pass_rush",
                               pos    = NULL,
                               view   = "all",
                               df_all = pass_rush_all_player_season_summary,
                               df_tps = pass_rush_tps_player_season_summary) {
  
  df <- if (view == "tps") df_tps else df_all
  df <- df %>% filter(def_ssn == team_season)
  
  prefix   <- if (any(grepl("^tps_", names(df)))) "tps_" else ""
  pctl_col <- paste0(prefix, metric, "_season_pctl")
  
  d_full <- df %>%
    transmute(player, position, games = n,
              pctl = .data[[pctl_col]],
              label_raw = paste0(player, " (", games, " games)")) %>%
    mutate(position = factor(position,
                             levels = c("ED", "DI", "LB"),
                             labels = c("ED", "DL", "LB")))
  
  metric_title <- tools::toTitleCase(gsub("_", " ", metric))
  pos_input    <- if (!is.null(pos)) { if (pos == "DI") "DL" else pos } else NULL
  
  build_panel <- function(p, show_x_axis) {
    d <- d_full %>%
      filter(position == p) %>%
      arrange(games) %>%
      mutate(label = factor(label_raw, levels = label_raw))
    if (nrow(d) == 0) return(NULL)
    
    ggplot(d, aes(x = pctl, y = label, fill = pctl)) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") +
      scale_fill_gradient(low = "#deebf7", high = "#08306b",
                          limits = c(0, 1),
                          labels = scales::percent_format(accuracy = 1),
                          name = "Season\npctl") +
      scale_x_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.25),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(title = p,
           x = if (show_x_axis) "Season percentile" else NULL,
           y = NULL) +
      theme_minimal(base_size = 10) +
      theme(plot.title         = element_text(face = "bold", size = 14, hjust = 0),
            panel.grid.major.y = element_blank(),
            axis.text.x        = if (show_x_axis) element_text(size = 8) else element_blank(),
            plot.margin        = margin(t = 10, b = 5))
  }
  
  if (!is.null(pos_input)) {
    return(build_panel(pos_input, show_x_axis = TRUE) +
             labs(subtitle = paste0(team_season, " — ", metric_title)))
  }
  
  p_ed <- build_panel("ED", show_x_axis = FALSE)
  p_dl <- build_panel("DL", show_x_axis = FALSE)
  p_lb <- build_panel("LB", show_x_axis = TRUE)
  
  n_ed <- d_full %>% filter(position == "ED") %>% nrow()
  n_dl <- d_full %>% filter(position == "DL") %>% nrow()
  n_lb <- d_full %>% filter(position == "LB") %>% nrow()
  
  library(patchwork)
  (p_ed / p_dl / p_lb) +
    plot_layout(heights = c(n_ed, n_dl, n_lb), guides = "collect") +
    plot_annotation(
      title    = paste0(team_season, " — ", metric_title, " (by position)"),
      subtitle = "Each bar = one rusher  |  Dark = elite season  |  Light = below avg",
      theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                       plot.subtitle = element_text(size = 10))
    )
}


# plot_prush_card("DET2025")                          # all snaps
plot_prush_card("DET2025", view = "tps")            # true pass set
#plot_prush_card("DET2024", view = "tps")
plot_prush_card("DET2024_wo_Hutch", view = "tps", df_tps = pass_rush_tps_player_season_summary_hutch_split)

plot_prush_players("DET2025", metric = "hit_rate", view = "tps")
plot_prush_players("PHI2024", metric = "hit_rate", view = "tps")
plot_prush_players("DET2025", metric = "grade_pass_rush", view = "tps")

# metrics: grade_pass_rush, prp, pass_rush_win_rate, pressure_rate, hit_rate, hurry_rate, batted_pass_rate

plot_prush_card_player <- function(player_id_vec,
                                   seasons = NULL,
                                   view    = "all",
                                   df_all  = pass_rush_all_player_season_summary,
                                   df_tps  = pass_rush_tps_player_season_summary) {
  
  df <- if (view == "tps") df_tps else df_all
  df <- df %>% filter(player_id %in% player_id_vec)
  if (!is.null(seasons)) df <- df %>% filter(season %in% seasons)
  
  if (nrow(df) == 0) stop("No matching rows found")
  
  title_suffix <- if (view == "tps") "True Pass Set" else "All Snaps"
  
  metrics <- c("grade_pass_rush", "prp", "pass_rush_win_rate",
               "pressure_rate", "hit_rate", "hurry_rate")
  metric_labels <- c("Grade","PRP","Win Rate","Pressure %","Hit %","Hurry %")
  
  prefix    <- if (any(grepl("^tps_", names(df)))) "tps_" else ""
  pctl_cols <- paste0(prefix, metrics, "_season_pctl")
  
  d_full <- df %>%
    arrange(desc(player), season) %>%                                # player asc top→bottom, newest year at top of each group
    mutate(row_label = paste0(player, " — ", def_ssn, " (", n, " gms)"),
           row_label = factor(row_label, levels = unique(row_label))) %>%
    select(row_label, position, n, all_of(pctl_cols)) %>%
    rename_with(~ paste0(metrics, "_season_pctl"), all_of(pctl_cols)) %>%
    pivot_longer(ends_with("_season_pctl"),
                 names_to = "metric", values_to = "pctl") %>%
    mutate(metric   = sub("_season_pctl$", "", metric),
           metric   = factor(metric, levels = metrics, labels = metric_labels),
           position = factor(position,
                             levels = c("ED", "DI", "LB"),
                             labels = c("ED", "DL", "LB")))
  
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
  
  pos_order <- intersect(c("ED", "DL", "LB"),
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
    paste0(player_names, " — Pass Rusher Scouting Card (", title_suffix, ")")
  } else {
    paste0("Pass Rusher Scouting Card (", title_suffix, ")")
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


plot_prush_players_player <- function(player_id_vec,
                                      metric  = "grade_pass_rush",
                                      seasons = NULL,
                                      view    = "all",
                                      df_all  = pass_rush_all_player_season_summary,
                                      df_tps  = pass_rush_tps_player_season_summary) {
  
  df <- if (view == "tps") df_tps else df_all
  df <- df %>% filter(player_id %in% player_id_vec)
  if (!is.null(seasons)) df <- df %>% filter(season %in% seasons)
  
  if (nrow(df) == 0) stop("No matching rows found")
  
  prefix   <- if (any(grepl("^tps_", names(df)))) "tps_" else ""
  pctl_col <- paste0(prefix, metric, "_season_pctl")
  
  d_full <- df %>%
    arrange(desc(player), season) %>%
    transmute(player, position, def_ssn, games = n,
              pctl = .data[[pctl_col]],
              label_raw = paste0(player, " — ", def_ssn, " (", games, " gms)")) %>%
    mutate(label = factor(label_raw, levels = unique(label_raw)),
           position = factor(position,
                             levels = c("ED", "DI", "LB"),
                             labels = c("ED", "DL", "LB")))
  
  metric_title <- tools::toTitleCase(gsub("_", " ", metric))
  
  build_panel <- function(p, show_x_axis) {
    d <- d_full %>% filter(position == p)
    if (nrow(d) == 0) return(NULL)
    d <- d %>% mutate(label = droplevels(label))
    
    ggplot(d, aes(x = pctl, y = label, fill = pctl)) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") +
      scale_fill_gradient(low = "#deebf7", high = "#08306b",
                          limits = c(0, 1),
                          labels = scales::percent_format(accuracy = 1),
                          name = "Season\npctl") +
      scale_x_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.25),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(title = p,
           x = if (show_x_axis) "Season percentile" else NULL,
           y = NULL) +
      theme_minimal(base_size = 10) +
      theme(plot.title         = element_text(face = "bold", size = 14, hjust = 0),
            panel.grid.major.y = element_blank(),
            axis.text.x        = if (show_x_axis) element_text(size = 8) else element_blank(),
            plot.margin        = margin(t = 10, b = 5))
  }
  
  pos_order <- intersect(c("ED", "DL", "LB"),
                         as.character(unique(d_full$position)))
  
  panels  <- list()
  heights <- c()
  for (i in seq_along(pos_order)) {
    p <- pos_order[i]
    panels[[i]] <- build_panel(p, show_x_axis = (i == length(pos_order)))
    heights[i]  <- d_full %>% filter(position == p) %>% nrow()
  }
  
  player_names <- unique(df$player)
  title_str <- if (length(player_names) == 1) {
    paste0(player_names, " — ", metric_title)
  } else {
    paste0(metric_title, " (multi-player)")
  }
  
  library(patchwork)
  if (length(panels) == 1) {
    result <- panels[[1]]
  } else {
    result <- Reduce(`/`, panels) + plot_layout(heights = heights, guides = "collect")
  }
  
  result + plot_annotation(
    title    = title_str,
    subtitle = "Each bar = one player-season  |  Dark = elite season  |  Light = below avg",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10))
  )
}

plot_prush_card_player(82330)                              # Hutch, all seasons, all snaps
plot_prush_card_player(82330, view = "tps")                # Hutch, TPS
plot_prush_card_player(c(11951, 10800, 48676, 76836, 50291), view = "tps")                    # Hutch + DJ Reader
plot_prush_card_player(c(56913, 44550, 43679), view = "tps") 
plot_prush_card_player(c(82330, 76996, 145080), view = "tps") 


plot_prush_players_player(82330, metric = "pressure_rate", view = "tps")
plot_prush_players_player(c(82330, 50291), metric = "prp", view = "tps")


# Detroit Lions
det_blue   <- "#0076B6"  # Honolulu Blue
det_silver <- "#B0B7BC"  # Silver

plot_det_opp_tps_long <- tps_pass_block_opp_position_pctl %>%
  filter(def_ssn %in% c("DET2024", "DET2025")) %>%
  pivot_longer(ends_with("_rank"),
               names_to = c("metric", "position"),
               names_pattern = "player_tps_(.+)_perc_(.+)_rank") %>%
  mutate(
    position = factor(position, levels = c("LT","LG","C","RG","RT")),
    metric   = factor(metric, levels = c("grade","pressure","hurries"),
                      labels = c("Grade","Pressure","Hurries"))
  )

plot_det_opp_tps_seg <- plot_det_opp_tps_long %>%
  pivot_wider(names_from = def_ssn, values_from = value)

ggplot() +
  geom_vline(xintercept = 0.5, color = "grey92", linewidth = 0.3) +
  geom_segment(data = plot_det_opp_tps_seg,
               aes(x = DET2024, xend = DET2025,
                   y = fct_rev(position), yend = fct_rev(position)),
               color = "grey70", linewidth = 1.2) +
  geom_point(data = plot_det_opp_tps_long,
             aes(x = value, y = fct_rev(position), color = def_ssn),
             size = 5.5, stroke = 0) +
  scale_color_manual(values = c("DET2024" = det_silver, "DET2025" = det_blue)) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, .25, .5, .75, 1),
                     labels = c("0", ".25", ".5", ".75", "1"),
                     expand = expansion(mult = c(0.03, 0.03))) +
  ggh4x::facet_wrap2(
    ~ metric, nrow = 1,
    strip = ggh4x::strip_themed(
      background_x = list(
        element_rect(fill = "#E3EFF7", color = NA),  # Grade — light blue
        element_rect(fill = "#EDEFF1", color = NA),  # Pressure — light silver
        element_rect(fill = "#D6E5F0", color = NA)   # Hurries — mid blue-grey
      )
    )
  ) +
  labs(x = NULL, y = NULL, color = NULL,
       title    = "DET — opposing OL true-pass-set percentile, 2024 vs 2025",
       subtitle = "Higher = OL performed better against DET defense") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position     = "bottom",
    legend.text         = element_text(size = 11),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.spacing.x     = unit(20, "pt"),
    panel.border        = element_rect(color = "grey85", fill = NA, linewidth = 0.4),
    strip.text          = element_text(face = "bold", size = 12,
                                       margin = margin(5, 0, 5, 0)),
    axis.text           = element_text(size = 10),
    plot.title          = element_text(face = "bold", size = 13),
    plot.title.position = "plot"
  )