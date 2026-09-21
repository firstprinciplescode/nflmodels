# ============================================================================
# 2026 WEEK 1 vs 2025 -- where each team's week lands in its own distribution
# ============================================================================
#
# THE QUESTION THIS ANSWERS: not just "how far from the median", but "is this
# still in character?" A team can sit a long way off its median and still be
# inside a range it hit six times last year. So every row carries the whole
# 2025 shape -- min / 25th / median / 75th / max -- plus where the 2026 value
# falls inside it, and how many of that team's own 2025 games were MORE extreme.
#
# BASELINES, one row each:
#   "ALL"            every 2025 regular-season game the team played
#   <qbgrp_ssn>      that team's 2025 games under one starting QB
# 2025 was not a one-QB year for most of the league: 10 teams used three
# starters, 14 used two, 8 used one. So a 2026 offense is compared against each
# 2025 QB group separately as well as against the whole season.
# Defense keeps only "ALL" -- def_ssn is already one group per team-season.
#
# NO COMPOSITE SCORE. Per Andy: nothing here boils down to one number. Metrics
# carry a `focus` flag per side instead:
#   offense  pass_rate, fastr_xpass_rate, pbp_xpass_rate, scr_rate, pa_rate, less_rate
#   defense  blitz_rate, pressure_rate, deep_rate, sack_rate, less_rate, pass_rate
#
# SIDES: offense = the team with the ball (group by posteam). Defense = what the
# team allowed (same team-game rows grouped by defteam) -- the convention in
# qb_stats_df_build_AWS.R, where offense reads _rank and defense reads _rank_def.
#
# MEASUREMENT ONLY: nothing is appended to qb_stats_df_final, no model is
# re-fit, no S3 or Athena writes. The 2026 side is read from
# cache/qb_stats_2026.rds (data_build/pbp_2026_qb_lane_AWS.R). The 2025 side is
# built here from load_pbp(2025) with the same formulas, scoring only the xpass
# model -- every other metric is raw pbp or PFF.
#
# BASELINE = 2025 REGULAR SEASON ONLY (weeks 1-18).
# NOT IN HERE: coaching continuity. The repo has no coach data of any kind.
#
# RUN IT: in your normal working session, from the repo root -- NO restart
# needed (corrected 2026-09-20). This file never assigns combined_pbp, pbp_base,
# part_nfl, qb_stats_df_final, receiving_func_base or rush_stats_final, never
# calls rm(), load() or source().
# LOOK AT IT AFTERWARDS: source("pff_stats/evaluation/cmp26_peek.R")
# ============================================================================


# ---- 0. session wall -- COMMENTED OUT 2026-09-20 ---------------------------
# It forced an R restart, and the way back from a restart is load()-ing a saved
# workspace, which silently puts OLD copies of rush_stats_final /
# receiving_func_base over fresh ones. Nothing below assigns any of the six
# frames it named (checked line by line).

# .cmp_live <- c("combined_pbp", "pbp_base", "part_nfl", "qb_stats_df_final",
#                "receiving_func_base", "rush_stats_final")
# .cmp_found <- .cmp_live[vapply(.cmp_live, exists, logical(1))]
# if (length(.cmp_found) > 0) {
#   stop("STOP -- run this in a FRESH R session. Found live objects: ",
#        paste(.cmp_found, collapse = ", "))
# }

library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(xgboost)
library(aws.s3)

'%ni%' <- Negate('%in%')

SEASON_NEW  <- 2026
SEASON_BASE <- 2025
BASE_WEEKS  <- 1:18
USABLE_N    <- 4        # a baseline with fewer games than this is flagged, not dropped

METRICS <- c("pass_rate", "fastr_xpass_rate", "pbp_xpass_rate", "scr_rate",
             "pa_rate", "less_rate", "blitz_rate", "pressure_rate", "sack_rate",
             "deep_rate", "short_rate", "plays", "no_huddle", "adot",
             "acc_rate", "ypa")

FOCUS_OFF <- c("pass_rate", "fastr_xpass_rate", "pbp_xpass_rate",
               "scr_rate", "pa_rate", "less_rate")
FOCUS_DEF <- c("blitz_rate", "pressure_rate", "deep_rate",
               "sack_rate", "less_rate", "pass_rate")


# ---- 1. Athena reader ------------------------------------------------------

run_athena_query <- function(sql, max_wait = 180) {
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  status <- "RUNNING"; elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2); elapsed <- elapsed + 2
    status <- trimws(system(sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id), intern = TRUE))
  }
  if (status != "SUCCEEDED") {
    stop(sprintf("Athena %s: %s", status, paste(system(sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id), intern = TRUE), collapse = " ")))
  }
  s3_path <- trimws(system(sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id), intern = TRUE))
  Sys.sleep(2)
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


# ---- 2. 2025 team-games from pbp -------------------------------------------

cat("loading", SEASON_BASE, "pbp ...\n")
b_raw <- load_pbp(SEASON_BASE)

for (col in c("posteam", "defteam", "td_team")) {
  b_raw[[col]][which(b_raw[[col]] == "ARI")] <- "ARZ"
  b_raw[[col]][which(b_raw[[col]] == "BAL")] <- "BLT"
  b_raw[[col]][which(b_raw[[col]] == "CLE")] <- "CLV"
  b_raw[[col]][which(b_raw[[col]] == "HOU")] <- "HST"
}
b_raw$week <- ifelse(b_raw$week == 19, 28, b_raw$week)
b_raw$week <- ifelse(b_raw$week == 20, 29, b_raw$week)
b_raw$week <- ifelse(b_raw$week == 21, 30, b_raw$week)
b_raw$week <- ifelse(b_raw$week == 22, 32, b_raw$week)

b <- b_raw %>%
  filter(play_type %in% c("pass", "run"),
         special_teams_play == 0,
         play_type %ni% c("no_play", "qb_kneel", "qb_spike", "punt"),
         !is.na(play_type), !is.na(posteam),
         is.na(two_point_conv_result)) %>%
  filter(!grepl("TWO-POINT CONVERSION", desc)) %>%
  filter(qb_spike == 0, qb_kneel == 0, two_point_attempt == 0)

b$mod_ydstogo    <- ifelse(b$goal_to_go == 1, b$yardline_100, b$ydstogo)
b$posteam_ind    <- ifelse(b$home_team == b$posteam, 1, 0)
b$down_one_ind   <- ifelse(!is.na(b$down) & b$down == 1, 1, 0)
b$down_two_ind   <- ifelse(!is.na(b$down) & b$down == 2, 1, 0)
b$down_three_ind <- ifelse(!is.na(b$down) & b$down == 3, 1, 0)

xgb_pbp_xpass <- local({
  tf <- tempfile(fileext = ".model")
  save_object("models/xgb_pbp_xpass_no_temp.model", bucket = "nfl-pff-data-lucas", file = tf)
  m <- xgb.load(tf); unlink(tf); m
})

xp_feats <- c("yardline_100","half_seconds_remaining","posteam_timeouts_remaining",
              "defteam_timeouts_remaining","mod_ydstogo","posteam_ind","down_one_ind",
              "down_two_ind","down_three_ind","down","shotgun","no_huddle","score_differential")
b$pbp_predicted_xpass <- NA_real_
elig <- which(b$two_point_attempt == 0 & (b$pass_attempt == 1 | b$rush_attempt == 1) &
                b$play_type %in% c("run","pass") & b$qb_kneel == 0 & b$qb_spike == 0)
b$pbp_predicted_xpass[elig] <- predict(
  xgb_pbp_xpass, newdata = xgb.DMatrix(data = as.matrix(b[elig, xp_feats])))

base_pbp <- b %>%
  group_by(posteam, defteam, week, season) %>%
  dplyr::summarise(
    pass_rate = sum(pass_attempt, na.rm = T) / sum(play, na.rm = T),
    fastr_xpass_rate = sum(xpass, na.rm = T) / sum(play, na.rm = T),
    pbp_xpass_rate = sum(pbp_predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    scr_rate = (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))) / sum(qb_dropback, na.rm = TRUE),
    sack_rate = sum(sack, na.rm = TRUE) / sum(qb_dropback, na.rm = TRUE),
    acc_rate = sum(complete_pass, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    ypa = sum(passing_yards, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    adot = sum(air_yards, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    plays = sum(play, na.rm = TRUE),
    no_huddle = sum(no_huddle, na.rm = TRUE) / sum(play, na.rm = TRUE),
    .groups = "drop"
  )
cat("2025 team-games from pbp:", nrow(base_pbp), "\n")


# ---- 3. PFF splits + the 2025 QB groups ------------------------------------

cat("pulling PFF splits for", SEASON_BASE, "...\n")
pff_less  <- run_athena_query(sprintf("SELECT team_name, week, season, less_rate FROM nfl_data.vw_passing_tip WHERE season = %d", SEASON_BASE))
pff_pa    <- run_athena_query(sprintf("SELECT team_name, week, season, pa_rate FROM nfl_data.vw_combined_pa WHERE season = %d", SEASON_BASE))
pff_blitz <- run_athena_query(sprintf("SELECT team_name, week, season, blitz_rate FROM nfl_data.vw_combined_blitz WHERE season = %d", SEASON_BASE))
pff_press <- run_athena_query(sprintf("SELECT team_name, week, season, pressure_rate FROM nfl_data.vw_combined_pressure WHERE season = %d", SEASON_BASE))
pff_depth <- run_athena_query(sprintf("SELECT team_name, week, season, short_rate, deep_rate FROM nfl_data.vw_combined_depth WHERE season = %d", SEASON_BASE))

base_pff <- pff_less %>%
  full_join(pff_pa,    by = c("team_name","week","season")) %>%
  full_join(pff_blitz, by = c("team_name","week","season")) %>%
  full_join(pff_press, by = c("team_name","week","season")) %>%
  full_join(pff_depth, by = c("team_name","week","season"))

# qbgrp_ssn per 2025 team-week, built the way pbp_nfl_base.R:305-319 builds it
qb_wk <- run_athena_query(sprintf("
  SELECT team, season, week, starting_qb, starting_qb_id
  FROM   nfl_data.vw_team_passing_summary
  WHERE  season = %d", SEASON_BASE)) %>%
  mutate(
    last_name = case_when(
      str_count(starting_qb, "\\S+") == 3 ~ word(starting_qb, 2),
      TRUE ~ word(starting_qb, -1)),
    qbgrp_ssn = paste0(team, last_name, "-", season)) %>%
  dplyr::select(team, week, season, qbgrp_ssn, starting_qb)

base_tg <- base_pbp %>%
  left_join(base_pff, by = c("posteam" = "team_name", "week" = "week", "season" = "season")) %>%
  left_join(qb_wk,    by = c("posteam" = "team",      "week" = "week", "season" = "season")) %>%
  filter(week %in% BASE_WEEKS)

cat("2025 regular-season team-games kept:", nrow(base_tg),
    "| QB groups:", length(unique(base_tg$qbgrp_ssn)), "\n")


# ---- 4. the 2026 side ------------------------------------------------------

if (!file.exists("cache/qb_stats_2026.rds")) {
  stop("cache/qb_stats_2026.rds missing -- run data_build/pbp_2026_qb_lane_AWS.R first.")
}
new_tg <- readRDS("cache/qb_stats_2026.rds")
missing_metric <- setdiff(METRICS, names(new_tg))
if (length(missing_metric) > 0) stop("2026 frame missing: ", paste(missing_metric, collapse = ", "))

new_tg <- new_tg %>% dplyr::select(posteam, defteam, week, season, qbgrp_ssn, any_of(METRICS))
cat("2026 team-games:", nrow(new_tg), "\n")


# ---- 5. long form, both sides ----------------------------------------------

to_long <- function(d, side) {
  team_col <- if (side == "offense") "posteam" else "defteam"
  opp_col  <- if (side == "offense") "defteam" else "posteam"
  d %>%
    dplyr::select(team = all_of(team_col), opp = all_of(opp_col), week, season,
                  qbgrp_ssn, any_of(METRICS)) %>%
    pivot_longer(cols = any_of(METRICS), names_to = "metric", values_to = "value") %>%
    mutate(side = side)
}

base_long <- bind_rows(to_long(base_tg, "offense"), to_long(base_tg, "defense")) %>%
  filter(!is.na(value))
new_long <- bind_rows(to_long(new_tg, "offense"), to_long(new_tg, "defense")) %>%
  rename(v26 = value, opp26 = opp, qbgrp26 = qbgrp_ssn)

# baseline rows: "ALL" for both sides, plus one per QB group on offense
base_all <- base_long %>% mutate(baseline = "ALL", baseline_qb = "all 2025")
qb_names <- qb_wk %>% distinct(qbgrp_ssn, starting_qb) %>%
  group_by(qbgrp_ssn) %>% dplyr::slice(1) %>% ungroup()
base_qb  <- base_long %>% filter(side == "offense") %>%
  mutate(baseline = qbgrp_ssn) %>%
  left_join(qb_names, by = c("baseline" = "qbgrp_ssn")) %>%
  rename(baseline_qb = starting_qb)
base_stack <- bind_rows(base_all, base_qb)


# ---- 6. the distribution, per team x side x metric x baseline --------------

shape <- base_stack %>%
  group_by(team, side, metric, baseline, baseline_qb) %>%
  dplyr::summarise(
    n25   = n(),
    min25 = min(value), q25_25 = quantile(value, .25, type = 4),
    med25 = median(value), q75_25 = quantile(value, .75, type = 4),
    max25 = max(value), sd25 = sd(value),
    .groups = "drop") %>%
  mutate(iqr25 = q75_25 - q25_25)

# the 2025 values themselves, kept as a list column so each row can be placed
vals <- base_stack %>%
  group_by(team, side, metric, baseline) %>%
  dplyr::summarise(x = list(value), opps = list(opp), .groups = "drop")

lg <- base_long %>%
  group_by(side, metric) %>%
  dplyr::summarise(lg_med25 = median(value), lg_sd25 = sd(value), .groups = "drop")

cmp26 <- new_long %>%
  dplyr::select(team, side, metric, week, opp26, qbgrp26, v26) %>%
  left_join(shape, by = c("team", "side", "metric"),
            relationship = "many-to-many") %>%
  left_join(vals,  by = c("team", "side", "metric", "baseline")) %>%
  left_join(lg,    by = c("side", "metric")) %>%
  rowwise() %>%
  mutate(
    pctl25        = mean(x <= v26),
    in_range      = v26 >= min25 & v26 <= max25,
    in_iqr        = v26 >= q25_25 & v26 <= q75_25,
    n_more_extreme = sum(abs(x - med25) > abs(v26 - med25)),
    n_opp25       = sum(opps == opp26),
    v25_vs_opp    = if (sum(opps == opp26) == 0) NA_real_ else mean(x[opps == opp26]),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    delta        = v26 - med25,
    delta_sd     = ifelse(is.na(sd25) | sd25 == 0, NA_real_, delta / sd25),
    delta_iqr    = ifelse(is.na(iqr25) | iqr25 == 0, NA_real_, delta / iqr25),
    delta_lg_sd  = ifelse(is.na(lg_sd25) | lg_sd25 == 0, NA_real_, delta / lg_sd25),
    delta_vs_opp = v26 - v25_vs_opp,
    share_more_extreme = n_more_extreme / n25,
    place = case_when(
      v26 > max25 ~ "above 2025 max",
      v26 < min25 ~ "below 2025 min",
      in_iqr      ~ "middle half",
      v26 > q75_25 ~ "high, in range",
      TRUE        ~ "low, in range"),
    focus = (side == "offense" & metric %in% FOCUS_OFF) |
            (side == "defense" & metric %in% FOCUS_DEF),
    usable = n25 >= USABLE_N
  ) %>%
  dplyr::select(team, side, metric, focus, baseline, baseline_qb, qbgrp26, opp26,
                n25, usable, v26, min25, q25_25, med25, q75_25, max25, sd25, iqr25,
                delta, delta_sd, delta_iqr, delta_lg_sd, pctl25, place, in_range, in_iqr,
                n_more_extreme, share_more_extreme,
                v25_vs_opp, n_opp25, delta_vs_opp, lg_med25, lg_sd25) %>%
  arrange(team, side, metric, desc(baseline == "ALL"), baseline)


# ---- 7. receipts + save ----------------------------------------------------

cat("\n=================== BUILT ===================\n")
cat("cmp26 rows:", nrow(cmp26),
    "| teams:", length(unique(cmp26$team)),
    "| metrics:", length(unique(cmp26$metric)),
    "| baselines per team-side-metric:",
    paste(range(cmp26 %>% count(team, side, metric) %>% pull(n)), collapse = "-"), "\n")

cat("\nwhere the focus metrics landed (ALL baseline):\n")
print(cmp26 %>% filter(focus, baseline == "ALL") %>%
        count(side, place) %>%
        pivot_wider(names_from = place, values_from = n, values_fill = 0) %>%
        as.data.frame(), row.names = FALSE)

cat("\nfurthest from the median but STILL IN RANGE (focus, ALL baseline):\n")
print(cmp26 %>% filter(focus, baseline == "ALL", in_range, !in_iqr) %>%
        arrange(desc(abs(delta_sd))) %>% head(12) %>%
        dplyr::select(team, side, metric, v26, med25, min25, max25, delta, delta_sd,
                      pctl25, n_more_extreme) %>%
        as.data.frame(), row.names = FALSE, digits = 3)

cat("\noutside the 2025 range entirely (focus, ALL baseline):\n")
print(cmp26 %>% filter(focus, baseline == "ALL", !in_range) %>%
        arrange(desc(abs(delta_sd))) %>%
        dplyr::select(team, side, metric, v26, min25, max25, delta, delta_sd, place) %>%
        as.data.frame(), row.names = FALSE, digits = 3)

if (!dir.exists("cache")) dir.create("cache")
saveRDS(cmp26, "cache/cmp26.rds")
saveRDS(base_tg, "cache/base25_tg.rds")
write.csv(cmp26, "cache/cmp26.csv", row.names = FALSE)
cat("\nwrote cache/cmp26.rds and cache/cmp26.csv  (", nrow(cmp26), "rows )\n")
cat('load it:  uncache_frames("cmp26")\n')
cat('browse it: source("pff_stats/evaluation/cmp26_peek.R")\n')
