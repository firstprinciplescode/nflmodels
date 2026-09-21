# ============================================================================
# 2026 QB LANE -- pbp only, no participation, nothing appended to history
# ============================================================================
#
# WHAT THIS MAKES: q26_qb_stats, one row per team-game for 2026, in the same
# shape as qb_stats_df_final MINUS every part_* column (nflreadr participation
# stops after 2023, so 2026 has none and never will until after the season).
#
# WHY IT IS SEPARATE: qb_stats_df_final's ranks are keyed on qbgrp_ssn /
# def_ssn, which embed the season, so 2026 rows cannot move a 2016-2025 rank.
# But four things in the pipeline DO pool across seasons, and this lane stays
# clear of all four:
#   1. epa_pass_tiebreaker / epa_run_tiebreaker  (pbp_nfl_base.R:234-235)
#      -- league-wide medians; 2026 rows would re-flip historical good/bad games.
#      This lane never touches pbp_base and never re-uploads
#      combined_grade_epa_summary (pbp_nfl_base.R:364-371).
#   2. df_builds/*_NEW.R scale() across every qbgrp_ssn (e.g.
#      df_blitz_build_AWS_NEW.R:301-311) -- do not re-run those with 2026 loaded.
#   3. model_funcs live sd()/mean() over qb_stats_df_final (scr_func_AWS.R:198-235,
#      plays_func_AWS.R:104-114, xpass_func_AWS.R:104-120) -- they have no season
#      filter, so never rbind q26_qb_stats into qb_stats_df_final.
#   4. every cluster fit in the receiving / rushing chains.
#
# WHAT IT DOES NOT DO: no ranks (one game per team-season makes (rank-1)/(n-1)
# NaN), no S3 writes, no Athena writes, no pbp_xtd.csv upload
# (pbp_combined_AWS.R:530-531 would replace 2016-2025 with one week).
#
# HOW TO RUN: in your normal working session -- NO restart needed (corrected
# 2026-09-20). This file never assigns combined_pbp, pbp_base, part_nfl,
# qb_stats_df_final, receiving_func_base or rush_stats_final, never calls rm(),
# load() or source(). Everything it makes is named p26* / q26* / .p26*. The only
# shared names it re-assigns are ones it sets to the SAME thing pbp_combined_AWS.R
# does: run_athena_query, load_model_from_s3, impute_weather (byte-identical),
# bucket, prefix, '%ni%', and the 11 xgb_pbp_* models (same S3 keys).
# When it finishes it writes cache/qb_stats_2026.rds; load it yourself with
#   uncache_frames("qb_stats_2026")
#
# WEATHER: checked 2026-09-15 -- 2026 week 1 has zero outdoor games missing
# temp/wind, so the ~/weather_backfill.rds block (pbp_nfl_base.R:182-192) is
# skipped here. (That block matches on nflverse_game_id, which pbp does not
# have in 2025 or 2026 -- it is a participation column.) Domes fall through to
# the same blanket wind 0 / temp 70 the models were trained on.
# ============================================================================


# ---- 0. session wall -- COMMENTED OUT 2026-09-20 ---------------------------
# It forced an R restart, and the way back from a restart is load()-ing a saved
# workspace, which silently puts OLD copies of rush_stats_final /
# receiving_func_base over fresh ones. The wall's claim was also wrong: nothing
# below assigns any of the six frames it named (checked line by line).

# .p26_live <- c("combined_pbp", "pbp_base", "part_nfl", "qb_stats_df_final",
#                "receiving_func_base", "rush_stats_final")
# .p26_found <- .p26_live[vapply(.p26_live, exists, logical(1))]
# if (length(.p26_found) > 0) {
#   stop("STOP -- run this in a FRESH R session. Found live objects: ",
#        paste(.p26_found, collapse = ", "),
#        "\nScoring here would overwrite them.")
# }

library(nflreadr)
library(dplyr)
library(stringr)
library(xgboost)
library(aws.s3)

conflicted::conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, .quiet = TRUE)

'%ni%' <- Negate('%in%')

SEASON_26 <- 2026


# ---- 1. Athena reader (verbatim from qb_stats_df_build_AWS.R:128-177) -------

run_athena_query <- function(sql, max_wait = 120) {
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  cat("Query ID:", query_id, "\n")

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

  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- trimws(system(result_cmd, intern = TRUE))
  Sys.sleep(2)
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


# ---- 2. raw 2026 pbp + real_scores -----------------------------------------
# real_scores must come off the UNFILTERED frame: the model frame below drops
# field goals, so FG attempts would vanish (same reason qb_stats_df_build_AWS.R
# loads pbp a second time at :14-51).

p26_raw <- load_pbp(SEASON_26)
cat("2026 pbp rows:", nrow(p26_raw), "| games:", length(unique(p26_raw$game_id)), "\n")

for (col in c("posteam", "defteam", "td_team")) {
  p26_raw[[col]][which(p26_raw[[col]] == "ARI")] <- "ARZ"
  p26_raw[[col]][which(p26_raw[[col]] == "BAL")] <- "BLT"
  p26_raw[[col]][which(p26_raw[[col]] == "CLE")] <- "CLV"
  p26_raw[[col]][which(p26_raw[[col]] == "HOU")] <- "HST"
}

# postseason week remap, 2020+ branch only (pbp_nfl_base.R:20-27)
p26_raw$week <- ifelse(p26_raw$week == 19, 28, p26_raw$week)
p26_raw$week <- ifelse(p26_raw$week == 20, 29, p26_raw$week)
p26_raw$week <- ifelse(p26_raw$week == 21, 30, p26_raw$week)
p26_raw$week <- ifelse(p26_raw$week == 22, 32, p26_raw$week)

p26_raw$td_side <- ifelse((!is.na(p26_raw$td_team) & p26_raw$posteam == p26_raw$td_team), 1, 0)

q26_real_scores <- p26_raw %>%
  group_by(posteam, week, season) %>%
  dplyr::summarise(tds = sum(td_side, na.rm = TRUE),
                   fgs = sum(field_goal_attempt, na.rm = TRUE),
                   .groups = "drop")


# ---- 3. prep (pbp_nfl_base.R:15-196, row-wise parts only) ------------------

p26 <- p26_raw

p26$mod_ydstogo <- ifelse(p26$goal_to_go == 1, p26$yardline_100, p26$ydstogo)

p26$lead_timeout <- dplyr::lag(p26$timeout_team, 1)
p26$last_timeout_ind <- ifelse(is.na(p26$lead_timeout), 0,
                        ifelse(p26$lead_timeout == p26$posteam, 1, -1))

p26$posteam_ind <- ifelse(p26$home_team == p26$posteam, 1, 0)

p26$air_yards[which(p26$air_yards <= -54)] <- 0

p26$down_one_ind   <- ifelse(!is.na(p26$down) & p26$down == 1, 1, 0)
p26$down_two_ind   <- ifelse(!is.na(p26$down) & p26$down == 2, 1, 0)
p26$down_three_ind <- ifelse(!is.na(p26$down) & p26$down == 3, 1, 0)
p26$down_four_ind  <- ifelse(!is.na(p26$down) & p26$down == 4, 1, 0)

p26$distance_to_sticks <- p26$mod_ydstogo - p26$air_yards

p26$season_type <- ifelse(p26$season_type == "REG", 0, 1)
p26$roof        <- ifelse(p26$roof %in% c("dome", "closed"), 1, 0)
p26$surface     <- ifelse(p26$surface == "grass", 0, 1)

p26 <- p26 %>%
  filter(play_type %in% c("pass", "run"),
         special_teams_play == 0,
         play_type %ni% c("no_play", "qb_kneel", "qb_spike", "punt"),
         !is.na(play_type),
         !is.na(posteam),
         is.na(two_point_conv_result))

p26 <- p26 %>% filter(!grepl("TWO-POINT CONVERSION", desc))

# REPRODUCED ON PURPOSE (pbp_nfl_base.R:147-149): these three lines re-apply the
# recodes above to columns that are already numeric, so season_type becomes 1
# everywhere, roof 0 everywhere, surface 1 everywhere. The models were fit on
# frames that went through exactly this, so "fixing" it here would move the
# predictions away from training.
p26$season_type <- ifelse(p26$season_type == "REG", 0, 1)
p26$roof        <- ifelse(p26$roof %in% c("dome", "closed"), 1, 0)
p26$surface     <- ifelse(p26$surface == "grass", 0, 1)

p26$run_location <- ifelse(is.na(p26$run_location), 2,
                    ifelse(p26$run_location %in% c("left", "right"), 1, 0))
p26$run_gap <- ifelse(is.na(p26$run_gap), 0,
               ifelse(p26$run_gap == "guard", 1,
               ifelse(p26$run_gap == "tackle", 2, 3)))

p26$center_ind <- ifelse(p26$run_gap == 0, 1, 0)
p26$guard_ind  <- ifelse(p26$run_gap == 1, 1, 0)
p26$tackle_ind <- ifelse(p26$run_gap == 2, 1, 0)
p26$end_ind    <- ifelse(p26$run_gap == 3, 1, 0)

p26$pass_length   <- ifelse(is.na(p26$pass_length), 0,
                     ifelse(p26$pass_length == "short", 1, 2))
p26$pass_location <- ifelse(is.na(p26$pass_location), 0,
                     ifelse(p26$pass_location == "middle", 1, 2))
p26$middle_ind  <- ifelse(p26$pass_location == 1, 1, 0)
p26$outside_ind <- ifelse(p26$pass_location == 2, 1, 0)

p26 <- p26 %>% filter(qb_spike == 0, qb_kneel == 0)
p26 <- p26 %>% filter(two_point_attempt == 0)

p26$play_type_scr <- ifelse(p26$qb_scramble == 1 & p26$rush_attempt == 1, "pass", p26$play_type)

p26$half_ind <- ifelse(p26$game_half == "Half1", 1,
                ifelse(p26$game_half == "Half2", 2, 3))

p26$rain_ind   <- ifelse(grepl("Rain|Showers|Rainy|Raining", p26$weather), 1, 0)
p26$snow_ind   <- ifelse(grepl("Snow|Snowing|Flurries", p26$weather), 1, 0)
p26$precip_ind <- ifelse(p26$rain_ind == 1 | p26$snow_ind == 1, 1, 0)
p26$rain_ind[is.na(p26$rain_ind)] <- 0
p26$snow_ind[is.na(p26$snow_ind)] <- 0

# no weather backfill for 2026 -- see header
p26$wind[which(is.na(p26$wind))] <- 0
p26$temp[which(is.na(p26$temp))] <- 70

cat("2026 model frame rows:", nrow(p26),
    "| outdoor games missing temp before impute: 0 (verified 2026-09-15)\n")


# ---- 4. models (pbp_combined_AWS.R:6-30) -----------------------------------
# 11 of the 12; before_xtd is only used by the drive chain, which this lane
# does not build.

bucket <- "nfl-pff-data-lucas"
prefix <- "models/"

load_model_from_s3 <- function(model_name, bucket, prefix) {
  full_path <- paste0(prefix, model_name)
  temp_file <- tempfile(fileext = ".model")
  save_object(full_path, bucket = bucket, file = temp_file)
  model <- xgb.load(temp_file)
  unlink(temp_file)
  return(model)
}

xgb_pbp_after_run_xtd   <- load_model_from_s3("xgb_pbp_run_after_td_no_temp.model",  bucket, prefix)
xgb_pbp_after_pass_xtd  <- load_model_from_s3("xgb_pbp_pass_after_td_no_temp.model", bucket, prefix)
xgb_pbp_scramble_xtd    <- load_model_from_s3("xgb_pbp_scramble_xtd.model",          bucket, prefix)
xgb_pbp_pressure        <- load_model_from_s3("xgb_pbp_pressure_no_temp.model",      bucket, prefix)
xgb_pbp_xpass           <- load_model_from_s3("xgb_pbp_xpass_no_temp.model",         bucket, prefix)
xgb_pbp_sack            <- load_model_from_s3("xgb_pbp_sack.model",                  bucket, prefix)
xgb_pbp_cp              <- load_model_from_s3("xgb_pbp_cp_temp.model",               bucket, prefix)
xgb_pbp_ypc             <- load_model_from_s3("xgb_pbp_ypc.model",                   bucket, prefix)
xgb_pbp_ypa             <- load_model_from_s3("xgb_pbp_ypa.model",                   bucket, prefix)
xgb_pbp_yac             <- load_model_from_s3("xgb_pbp_yac.model",                   bucket, prefix)
xgb_pbp_scramble_ypc    <- load_model_from_s3("xgb_pbp_scramble_ypc.model",          bucket, prefix)

impute_weather <- function(data) {
  data %>%
    mutate(
      wind = ifelse(is.na(wind), 0, wind),
      temp = ifelse(is.na(temp), 70, temp),
      rain_ind = ifelse(is.na(rain_ind), 0, rain_ind),
      snow_ind = ifelse(is.na(snow_ind), 0, snow_ind)
    )
}

# feature sets + eligibility, copied from pbp_combined_AWS.R (see line refs)

score_block <- function(data, model, feats, eligible, impute = FALSE, drop = NULL) {
  out <- rep(NA_real_, nrow(data))
  keep <- which(eligible %in% TRUE)
  if (length(keep) > 0) {
    d <- data[keep, ]
    if (impute) d <- impute_weather(d)
    X <- d %>% select(all_of(feats))
    if (!is.null(drop)) X <- X %>% select(-all_of(drop))
    out[keep] <- predict(model, newdata = xgb.DMatrix(data = as.matrix(X)))
  }
  out
}

# after_run_xtd (:87-107)
p26$predicted_after_run_xtd <- score_block(
  p26, xgb_pbp_after_run_xtd,
  c("season_type","yardline_100","half_seconds_remaining","down","mod_ydstogo",
    "shotgun","no_huddle","last_timeout_ind","score_differential","roof","surface",
    "posteam_ind","run_location","guard_ind","tackle_ind","end_ind",
    "posteam_timeouts_remaining","defteam_timeouts_remaining"),
  p26$two_point_attempt == 0 & p26$qb_kneel == 0 & p26$qb_spike == 0 &
    p26$qb_scramble == 0 & p26$play_type == "run")

# after_pass_xtd (:122-145)
p26$predicted_after_pass_xtd <- score_block(
  p26, xgb_pbp_after_pass_xtd,
  c("yardline_100","half_seconds_remaining","shotgun","no_huddle","mod_ydstogo",
    "qb_dropback","air_yards","pass_length","middle_ind","outside_ind",
    "score_differential","qb_hit","roof","surface",
    "posteam_timeouts_remaining","defteam_timeouts_remaining"),
  p26$two_point_attempt == 0 & p26$qb_kneel == 0 & p26$qb_spike == 0 &
    p26$sack == 0 & p26$pass_attempt == 1 & p26$qb_scramble == 0 &
    p26$play_type == "pass" & !is.na(p26$air_yards))

# scramble_xtd (:159-179)
p26$predicted_after_scramble_xtd <- score_block(
  p26, xgb_pbp_scramble_xtd,
  c("yardline_100","season_type","half_seconds_remaining","down","down_one_ind",
    "down_two_ind","down_three_ind","mod_ydstogo","shotgun","no_huddle",
    "score_differential","surface","posteam_ind","end_ind","guard_ind","tackle_ind",
    "outside_ind","temp","wind","rain_ind","snow_ind"),
  p26$qb_scramble == 1 & p26$rush_attempt == 1 & p26$qb_spike == 0 &
    p26$qb_kneel == 0 & p26$two_point_attempt == 0 & p26$pass_attempt == 0,
  impute = TRUE)

# xpass (:202-221)
p26$predicted_xpass <- score_block(
  p26, xgb_pbp_xpass,
  c("yardline_100","half_seconds_remaining","posteam_timeouts_remaining",
    "defteam_timeouts_remaining","mod_ydstogo","posteam_ind","down_one_ind",
    "down_two_ind","down_three_ind","down","shotgun","no_huddle","score_differential"),
  p26$two_point_attempt == 0 & (p26$pass_attempt == 1 | p26$rush_attempt == 1) &
    p26$play_type %in% c("run","pass") & p26$qb_kneel == 0 & p26$qb_spike == 0)

# pressure (:236-257)
p26$predicted_pbp_pressure <- score_block(
  p26, xgb_pbp_pressure,
  c("yardline_100","season_type","half_seconds_remaining","down","down_one_ind",
    "down_two_ind","down_three_ind","mod_ydstogo","shotgun","no_huddle",
    "score_differential","surface","posteam_ind"),
  p26$two_point_attempt == 0 & p26$pass_attempt == 1 & p26$play_type == "pass" &
    p26$qb_kneel == 0 & p26$qb_spike == 0 & p26$sack == 0 & p26$rush_attempt == 0)

# sack (:266-286)
p26$predicted_sack <- score_block(
  p26, xgb_pbp_sack,
  c("yardline_100","season_type","half_seconds_remaining","down","down_one_ind",
    "down_two_ind","down_three_ind","mod_ydstogo","shotgun","no_huddle",
    "score_differential","surface","posteam_ind","temp","wind","rain_ind","snow_ind"),
  p26$play_type == "pass" & p26$qb_spike == 0 & p26$qb_kneel == 0 &
    p26$two_point_attempt == 0,
  impute = TRUE)

# cp (:301-326)
p26$predicted_cp <- score_block(
  p26, xgb_pbp_cp,
  c("yardline_100","half_seconds_remaining","shotgun","no_huddle","mod_ydstogo",
    "air_yards","pass_length","middle_ind","outside_ind","score_differential",
    "qb_hit","roof","surface","posteam_timeouts_remaining","defteam_timeouts_remaining",
    "down_one_ind","down_two_ind","down_three_ind","down_four_ind","wind","temp"),
  p26$two_point_attempt == 0 & p26$pass_attempt == 1 & p26$play_type == "pass" &
    p26$qb_kneel == 0 & p26$qb_spike == 0 & p26$qb_scramble == 0 & p26$sack == 0 &
    !is.na(p26$air_yards))

# ypc (:341-365) -- yards_gained is selected then dropped upstream; same here
p26$predicted_ypc <- score_block(
  p26, xgb_pbp_ypc,
  c("yards_gained","yardline_100","season_type","half_seconds_remaining","down",
    "down_one_ind","down_two_ind","down_three_ind","mod_ydstogo","shotgun","no_huddle",
    "score_differential","surface","posteam_ind","end_ind","guard_ind","tackle_ind",
    "temp","wind","rain_ind","snow_ind"),
  p26$two_point_attempt == 0 & p26$pass_attempt == 0 & p26$rush_attempt == 1 &
    p26$play_type %in% c("run","pass") & p26$qb_kneel == 0 & p26$qb_spike == 0 &
    p26$qb_scramble == 0,
  impute = TRUE, drop = "yards_gained")

# scramble_ypc (:380-404)
p26$predicted_scramble_ypc <- score_block(
  p26, xgb_pbp_scramble_ypc,
  c("yardline_100","season_type","half_seconds_remaining","down","down_one_ind",
    "down_two_ind","down_three_ind","mod_ydstogo","shotgun","no_huddle",
    "score_differential","surface","posteam_ind","end_ind","guard_ind","tackle_ind",
    "outside_ind","temp","wind","rain_ind","snow_ind"),
  p26$two_point_attempt == 0 & p26$rush_attempt == 1 & p26$qb_kneel == 0 &
    p26$qb_spike == 0 & p26$qb_scramble == 1 & p26$sack == 0 & p26$pass_attempt == 0,
  impute = TRUE)

# ypa (:419-445)
p26$predicted_ypa <- score_block(
  p26, xgb_pbp_ypa,
  c("yardline_100","half_seconds_remaining","shotgun","no_huddle","mod_ydstogo",
    "air_yards","pass_length","middle_ind","outside_ind","score_differential",
    "qb_hit","roof","surface","posteam_timeouts_remaining","defteam_timeouts_remaining",
    "temp","wind","rain_ind","snow_ind"),
  p26$play_type == "pass" & p26$rush_attempt == 0 & p26$two_point_attempt == 0 &
    p26$qb_kneel == 0 & p26$qb_spike == 0 & p26$qb_scramble == 0 & p26$sack == 0 &
    p26$pass_attempt == 1 & !is.na(p26$air_yards),
  impute = TRUE)

# yac (:460-488)
p26$predicted_yds_after_catch <- score_block(
  p26, xgb_pbp_yac,
  c("yardline_100","half_seconds_remaining","shotgun","no_huddle","mod_ydstogo",
    "qb_dropback","air_yards","pass_length","middle_ind","outside_ind",
    "score_differential","qb_hit","roof","surface","posteam_timeouts_remaining",
    "defteam_timeouts_remaining","down_one_ind","down_two_ind","down_three_ind",
    "temp","wind","rain_ind","snow_ind"),
  p26$play_type == "pass" & p26$qb_spike == 0 & p26$qb_kneel == 0 &
    p26$two_point_attempt == 0 & p26$complete_pass == 1 & p26$sack == 0 &
    p26$pass_attempt == 1 & p26$qb_scramble == 0 & !is.na(p26$air_yards) &
    !is.na(p26$yards_after_catch),
  impute = TRUE)

# rename to the pbp_ names the aggregation expects
# (pbp_part_combined_join_AWS.R:4-10, minus the drive-chain columns)
p26_prefix <- c("predicted_after_run_xtd", "predicted_after_pass_xtd",
                "predicted_after_scramble_xtd", "predicted_xpass",
                "predicted_pbp_pressure", "predicted_sack", "predicted_cp",
                "predicted_ypc", "predicted_scramble_ypc", "predicted_ypa",
                "predicted_yds_after_catch")
stopifnot(all(p26_prefix %in% colnames(p26)))
colnames(p26)[match(p26_prefix, colnames(p26))] <- paste0("pbp_", p26_prefix)

cat("scored rows per model:\n")
for (nm in paste0("pbp_", p26_prefix)) cat(sprintf("  %-34s %d\n", nm, sum(!is.na(p26[[nm]]))))


# ---- 5. qbgrp_ssn / def_ssn from PFF, in session only ----------------------
# combined_grade_epa_summary in Athena stops at 2025 BY DESIGN (see header), so
# the keys are rebuilt here from vw_team_passing_summary, which already carries
# 2026. Construction is pbp_nfl_base.R:305-319, verbatim.

q26_keys <- run_athena_query(sprintf("
    SELECT  season, week, team, opp, passing_grade, starting_qb, starting_qb_id
    FROM    nfl_data.vw_team_passing_summary
    WHERE   season = %d
", SEASON_26))

q26_keys <- q26_keys %>%
  mutate(
    last_name = case_when(
      str_count(starting_qb, "\\S+") == 3 ~ word(starting_qb, 2),
      TRUE ~ word(starting_qb, -1)
    ),
    qbgrp_ssn = paste0(team, last_name, "-", season),
    def_ssn   = paste0(opp, season)
  ) %>%
  select(-last_name)

cat("2026 key rows:", nrow(q26_keys), "| teams:", length(unique(q26_keys$team)), "\n")


# ---- 6. team-game aggregation (qb_stats_df_build_AWS.R:59-95, no part_*) ---

q26_base <- p26 %>%
  left_join(q26_keys %>% select(team, week, season, qbgrp_ssn, def_ssn),
            by = c("posteam" = "team", "week" = "week", "season" = "season"))

# team-code wall: every team-game must find its PFF key row
q26_lost <- q26_base %>% filter(is.na(qbgrp_ssn) | is.na(def_ssn)) %>%
  distinct(posteam, week, season)
if (nrow(q26_lost) > 0) {
  print(q26_lost)
  stop("STOP -- team-weeks with no vw_team_passing_summary match (team codes or a missing PFF week).")
}

q26_qb_stats <- q26_base %>%
  group_by(qbgrp_ssn, def_ssn, posteam, defteam, week, season, wind, temp, rain_ind, snow_ind) %>%
  dplyr::summarise(
    pass_rate = sum(pass_attempt, na.rm = T) / sum(play, na.rm = T),
    fastr_xpass_rate = sum(xpass, na.rm = T) / sum(play, na.rm = T),
    pbp_xpass_rate = sum(pbp_predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    scr_rate = (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))) / sum(qb_dropback, na.rm = TRUE),
    scr_ypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), yards_gained, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    pbp_scr_xypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), pbp_predicted_scramble_ypc, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    acc_rate = sum(complete_pass, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    fastr_cp = sum(cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_cp = sum(pbp_predicted_cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_pressure = sum(pbp_predicted_pbp_pressure, na.rm = T) / sum(qb_dropback, na.rm = T),
    sack_rate = sum(sack, na.rm = TRUE) / sum(qb_dropback, na.rm = TRUE),
    pbp_sack_rate = sum(pbp_predicted_sack, na.rm = T) / sum(qb_dropback, na.rm = TRUE),
    ypa = sum(passing_yards, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_xypa = sum(pbp_predicted_ypa, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    ypc = sum(ifelse(rush_attempt == 1 & qb_scramble == 0, yards_gained, 0)) / (sum(rush_attempt, na.rm = T) - sum(qb_scramble, na.rm = T)),
    pbp_xypc = mean(pbp_predicted_ypc, na.rm = T),
    yac = sum(yards_after_catch, na.rm = TRUE) / sum(complete_pass, na.rm = T),
    pbp_yac = sum(pbp_predicted_yds_after_catch, na.rm = T) / sum(complete_pass, na.rm = T),
    adot = sum(air_yards, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    plays = sum(play, na.rm = TRUE),
    no_huddle = sum(no_huddle, na.rm = TRUE) / sum(play, na.rm = TRUE),
    pbp_xtds = sum(dplyr::coalesce(pbp_predicted_after_run_xtd, pbp_predicted_after_pass_xtd, pbp_predicted_after_scramble_xtd), na.rm = T),
    .groups = "drop"
  )

q26_qb_stats <- left_join(q26_qb_stats, q26_real_scores,
                          by = c("posteam", "week", "season"))

q26_qb_stats$temp[which(is.na(q26_qb_stats$temp))] <- 71
q26_qb_stats$wind[which(is.na(q26_qb_stats$wind))] <- 0


# ---- 7. PFF splits ---------------------------------------------------------
# Same five marts qb_stats_df_build_AWS.R:180-238 uses, minus the
# combined_grade_epa_summary INNER JOIN (that table has no 2026 by design, so
# the joined form returns zero rows). Same select() lists as :243-259.

q26_less <- run_athena_query(sprintf(
  "SELECT * FROM nfl_data.vw_passing_tip WHERE season = %d", SEASON_26))
q26_pa <- run_athena_query(sprintf(
  "SELECT * FROM nfl_data.vw_combined_pa WHERE season = %d", SEASON_26))
q26_depth <- run_athena_query(sprintf(
  "SELECT * FROM nfl_data.vw_combined_depth WHERE season = %d", SEASON_26))
q26_blitz <- run_athena_query(sprintf(
  "SELECT * FROM nfl_data.vw_combined_blitz WHERE season = %d", SEASON_26))
q26_pressure <- run_athena_query(sprintf(
  "SELECT * FROM nfl_data.vw_combined_pressure WHERE season = %d", SEASON_26))

q26_qb_stats <- q26_qb_stats %>%
  left_join(q26_blitz %>% dplyr::select(team_name, week, season, blitz_rate, blitz_pressure_rate, no_blitz_pressure_rate, blitz_grade, no_blitz_grade, blitz_qbr, no_blitz_qbr),
            by = c("posteam" = "team_name", "week" = "week", "season" = "season")) %>%
  left_join(q26_depth %>% dplyr::select(team_name, week, season, behind_los_rate, short_rate, medium_rate, deep_rate, behind_los_grade, short_grade, medium_grade, deep_grade, behind_los_qbr, short_qbr, medium_qbr, deep_qbr),
            by = c("posteam" = "team_name", "week" = "week", "season" = "season")) %>%
  left_join(q26_less %>% dplyr::select(team_name, week, season, less_rate, less_pressure_rate, more_pressure_rate, less_grade, more_grade, less_qbr, more_qbr),
            by = c("posteam" = "team_name", "week" = "week", "season" = "season")) %>%
  left_join(q26_pa %>% dplyr::select(team_name, week, season, pa_rate, pa_pressure_rate, npa_pressure_rate, npa_grade, pa_grade, npa_qbr, pa_qbr),
            by = c("posteam" = "team_name", "week" = "week", "season" = "season")) %>%
  left_join(q26_pressure %>% dplyr::select(team_name, week, season, pressure_rate, no_pressure_grade, pressure_grade, no_pressure_qbr, pressure_qbr),
            by = c("posteam" = "team_name", "week" = "week", "season" = "season"))

# twp / int rates (qb_stats_df_build_AWS.R:263-278)
q26_twp <- q26_less %>%
  group_by(team_name, week, season) %>%
  dplyr::summarise(less_rate = sum(less_snaps, na.rm = T) / (sum(less_snaps, na.rm = T) + sum(more_snaps, na.rm = T)),
                   less_twp_rate = sum(less_snaps * less_twp_rate, na.rm = TRUE) / sum(less_snaps, na.rm = TRUE) / 100,
                   more_twp_rate = sum(more_snaps * more_twp_rate, na.rm = TRUE) / sum(more_snaps, na.rm = TRUE) / 100,
                   less_int_rate = sum(less_snaps * less_int_rate, na.rm = TRUE) / sum(less_snaps, na.rm = T),
                   more_int_rate = sum(more_snaps * more_int_rate, na.rm = TRUE) / sum(more_snaps, na.rm = T),
                   .groups = "drop") %>%
  mutate(twp_rate = less_rate * less_twp_rate + (1 - less_rate) * more_twp_rate,
         int_rate = less_rate * less_int_rate + (1 - less_rate) * more_int_rate) %>%
  dplyr::select(team_name, week, season, twp_rate, int_rate)

q26_qb_stats <- left_join(q26_qb_stats, q26_twp,
                          by = c("posteam" = "team_name", "week" = "week", "season" = "season"))


# ---- 8. receipts + save ----------------------------------------------------

cat("\n================ 2026 QB LANE ================\n")
cat("rows:", nrow(q26_qb_stats),
    "| teams:", length(unique(q26_qb_stats$posteam)),
    "| weeks:", paste(sort(unique(q26_qb_stats$week)), collapse = ","), "\n")

q26_pff_missing <- sum(is.na(q26_qb_stats$blitz_rate)) + sum(is.na(q26_qb_stats$pa_rate)) +
  sum(is.na(q26_qb_stats$less_rate)) + sum(is.na(q26_qb_stats$pressure_rate)) +
  sum(is.na(q26_qb_stats$behind_los_rate))
cat("PFF split cells missing:", q26_pff_missing, "\n")

print(q26_qb_stats %>%
        dplyr::select(posteam, week, pass_rate, fastr_xpass_rate, pbp_xpass_rate,
                      scr_rate, plays, no_huddle, blitz_rate, pa_rate) %>%
        arrange(posteam), n = 32)

if (!dir.exists("cache")) dir.create("cache")
saveRDS(q26_qb_stats, file.path("cache", "qb_stats_2026.rds"))
cat("\nwrote cache/qb_stats_2026.rds -- load it in your working session with",
    "\n  uncache_frames(\"qb_stats_2026\")\n")

