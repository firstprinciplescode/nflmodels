# Hutch's DET2024 weeks (per PFF: 1, 2, 3, 4, 6)
hutch_weeks_2024 <- c(1, 2, 3, 4, 6)

# ─────────────────────────────────────────────────────────────────
# Build a brand-new parallel dataset — DET2024 split by Hutch presence
# Originals (full_pass_rush_qbgrp, pass_rush_tps_player_agg, etc.) are untouched.
# ─────────────────────────────────────────────────────────────────

full_pass_rush_qbgrp_hutch_split <- full_pass_rush_qbgrp %>%
  mutate(def_ssn = case_when(
    def_ssn == "DET2024" &  week %in% hutch_weeks_2024 ~ "DET2024_w_Hutch",
    def_ssn == "DET2024"                                ~ "DET2024_wo_Hutch",
    TRUE                                                ~ def_ssn
  ))

# ─────────────────────────────────────────────────────────────────
# Re-run the exact same pipeline on the split dataset
# ─────────────────────────────────────────────────────────────────

pass_rush_tps_player_agg_hutch_split <- full_pass_rush_qbgrp_hutch_split %>%
  filter(true_pass_set_snap_counts_pass_rush >= 7) %>%
  group_by(player, player_id, position, qbgrp_ssn, season) %>%
  mutate(
    player_tps_grade_pass_rush_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_rush_defense)),
    player_tps_prp_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_prp)),
    player_tps_pass_rush_win_rate_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_pass_rush_win_rate)),
    player_tps_pressure_rate_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_pressure_rate)),
    player_tps_hit_rate_perc           = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hit_rate)),
    player_tps_hurry_rate_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hurry_rate)),
    player_tps_batted_pass_rate_perc   = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_batted_pass_rate))
  ) %>%
  ungroup() %>%
  group_by(def_ssn, position) %>%
  mutate(
    tps_grade_pass_rush_def_ssn_perc    = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_grades_pass_rush_defense)),
    tps_prp_def_ssn_perc                = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_prp)),
    tps_pass_rush_win_rate_def_ssn_perc = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(true_pass_set_pass_rush_win_rate)),
    tps_pressure_rate_def_ssn_perc      = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_pressure_rate)),
    tps_hit_rate_def_ssn_perc           = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hit_rate)),
    tps_hurry_rate_def_ssn_perc         = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_hurry_rate)),
    tps_batted_pass_rate_def_ssn_perc   = case_when(n() == 1 ~ 0.5, TRUE ~ percent_rank_avg(tps_batted_pass_rate)),
    n_tps = n()
  ) %>%
  filter(n_tps >= 3) %>%
  ungroup()

valid_qbgrp_hutch_split <- pass_rush_tps_player_agg_hutch_split %>%
  group_by(qbgrp_ssn) %>%
  summarise(n_games = n_distinct(week, season), .groups = 'drop') %>%
  filter(n_games >= 8) %>%
  pull(qbgrp_ssn)

pass_rush_tps_opp_percentile_hutch_split <- pass_rush_tps_player_agg_hutch_split %>%
  filter(qbgrp_ssn %in% valid_qbgrp_hutch_split) %>%
  group_by(qbgrp_ssn, position) %>%
  summarise(
    tps_grade_pass_rush_perc    = mean(tps_grade_pass_rush_def_ssn_perc, na.rm = TRUE),
    tps_prp_perc                = mean(tps_prp_def_ssn_perc, na.rm = TRUE),
    tps_pass_rush_win_rate_perc = mean(tps_pass_rush_win_rate_def_ssn_perc, na.rm = TRUE),
    tps_pressure_rate_perc      = mean(tps_pressure_rate_def_ssn_perc, na.rm = TRUE),
    tps_hit_rate_perc           = mean(tps_hit_rate_def_ssn_perc, na.rm = TRUE),
    tps_hurry_rate_perc         = mean(tps_hurry_rate_def_ssn_perc, na.rm = TRUE),
    tps_batted_pass_rate_perc   = mean(tps_batted_pass_rate_def_ssn_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = position,
    values_from = c(tps_grade_pass_rush_perc, tps_prp_perc, tps_pass_rush_win_rate_perc,
                    tps_pressure_rate_perc, tps_hit_rate_perc,
                    tps_hurry_rate_perc, tps_batted_pass_rate_perc)
  )

pass_rush_tps_player_season_summary_hutch_split <- full_pass_rush_qbgrp_hutch_split %>%
  filter(true_pass_set_snap_counts_pass_rush >= 11) %>%
  group_by(player, player_id, position, def_ssn, season) %>%
  summarise(
    player_tps_grade_pass_rush    = mean(true_pass_set_grades_pass_rush_defense, na.rm = TRUE),
    player_tps_prp                = mean(true_pass_set_prp, na.rm = TRUE),
    player_tps_pass_rush_win_rate = mean(true_pass_set_pass_rush_win_rate, na.rm = TRUE),
    player_tps_pressure_rate      = mean(tps_pressure_rate, na.rm = TRUE),
    player_tps_hit_rate           = mean(tps_hit_rate, na.rm = TRUE),
    player_tps_hurry_rate         = mean(tps_hurry_rate, na.rm = TRUE),
    player_tps_batted_pass_rate   = mean(tps_batted_pass_rate, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  group_by(position, season) %>%
  mutate(
    tps_grade_pass_rush_season_pctl    = percent_rank_avg(player_tps_grade_pass_rush),
    tps_prp_season_pctl                = percent_rank_avg(player_tps_prp),
    tps_pass_rush_win_rate_season_pctl = percent_rank_avg(player_tps_pass_rush_win_rate),
    tps_pressure_rate_season_pctl      = percent_rank_avg(player_tps_pressure_rate),
    tps_hit_rate_season_pctl           = percent_rank_avg(player_tps_hit_rate),
    tps_hurry_rate_season_pctl         = percent_rank_avg(player_tps_hurry_rate),
    tps_batted_pass_rate_season_pctl   = percent_rank_avg(player_tps_batted_pass_rate)
  ) %>%
  ungroup()


# Plot
plot_prush_card("DET2024_w_Hutch",  view = "tps", df_tps = pass_rush_tps_player_season_summary_hutch_split)
plot_prush_card("DET2024_wo_Hutch", view = "tps", df_tps = pass_rush_tps_player_season_summary_hutch_split)

