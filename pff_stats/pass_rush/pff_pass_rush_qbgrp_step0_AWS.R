# ============================================================
# PFF PASS RUSH -- STEP 0: full_pass_rush_qbgrp   (split out as its
# own file 2026-09-12; STAMPED 2026-09-13 by Andy: NE faced 21 weeks incl. 28,29,30,32)
#
# WHAT THIS MAKES: full_pass_rush_qbgrp -- pass_rush_summary + pass_rush_
#   kpis player-game rows (snap-gated), team-fixed, + qbgrp_ssn / def_ssn
#   + det_position, + the rate columns.
#
# WHY THIS FILE EXISTS: the canon step-0 is pff_pass_rush_AWS.R lines
#   50-191. Everything after line 191 in that file is the SEA-2025 study
#   (it dies at line 385 on `sim_def`, an object from a dead session).
#   Sourcing the whole file works but ends in an error and a page of
#   plots. This file is lines 50-191 and nothing else.
#
# WHAT IS VERBATIM: the pulls, the 12-key full_join, the .x/.y renames,
#   the snap gate (snap_counts_pass_rush >= 11 | true_pass_set_snap_
#   counts_pass_rush >= 7), the team fixes, the qbgrp join (team_name =
#   opp: DEFENSE side), the play_counts join, the rate mutate.
# WHAT IS NOT: pff_pass_rush_AWS.R:122-123 renames columns 29 and 85 BY
#   INDEX to "position" / "det_position". Those are position.x (PFF) and
#   position.y (play_counts) after the join; this file renames them BY
#   NAME and stops if the pair is not there.
#
# RECEIPTS (printed): rows; NE faced 2025 = 21 weeks incl. 28,29,30,32
#   (the Sept 9 workspace copy stopped at 28 -- a Jan-13 pull); band
#   rows DI / ED per season.
#
# SOURCE ORDER: after data_build ids + shared_ne_2026_constants.R,
#   BEFORE new_england_opp_pass_rush_schedule.R. Pulls play_counts
#   itself if the session lacks it. Never rm()s. Re-source WHOLE file.
# ============================================================

library(dplyr); library(tidyr)

# ------------------------------------------------------------
# 0. Athena helper (house pattern). Only defined if absent.
# ------------------------------------------------------------
if (!exists("run_athena_query")) {
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
    cat("S3 path:", s3_path, "\n")
    Sys.sleep(2)
    check_cmd <- sprintf("aws s3 ls %s", s3_path)
    cat("File check:", system(check_cmd, intern = TRUE), "\n")
    read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
  }
}

# ------------------------------------------------------------
# 1. pulls  (pff_pass_rush_AWS.R:50-58)
# ------------------------------------------------------------
pass_rush_kpis <- run_athena_query("
    SELECT  *
    FROM    nfl_data.pass_rush_kpis
")
pass_rush_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.pass_rush_summary
")
cat("--- pass_rush_summary: ", nrow(pass_rush_summary), " rows | pass_rush_kpis: ",
    nrow(pass_rush_kpis), " rows ---\n", sep = "")

# ------------------------------------------------------------
# 2. the 12-key full_join + .x/.y renames  (:61-75, verbatim)
# ------------------------------------------------------------
full_pass_rush <- full_join(pass_rush_summary, pass_rush_kpis,
  by = c("team_name", "player_game_count", "eligible_season", "team", "position",
         "player", "franchise_id", "player_id", "week", "season", "draft_season",
         "jersey_number"))
full_pass_rush <- full_pass_rush %>%
  rename_with(~ sub("\\.y$", "_kpis", .), ends_with(".y")) %>%
  rename_with(~ sub("\\.x$", "", .), ends_with(".x"))

# ------------------------------------------------------------
# 3. snap gate  (:93-94, verbatim -- the chain's frame is gated here;
#    the schedule's X_QUAL applies on top)
# ------------------------------------------------------------
n_pre_gate <- nrow(full_pass_rush)
full_pass_rush <- full_pass_rush %>%
  filter(snap_counts_pass_rush >= 11 | true_pass_set_snap_counts_pass_rush >= 7)
cat("snap gate (>=11 rush | >=7 TPS): ", n_pre_gate, " -> ", nrow(full_pass_rush), " rows\n", sep = "")

# ------------------------------------------------------------
# 4. team fixes  (:96-108)
# ------------------------------------------------------------
canon_fix <- function(v, season) {
  before <- v
  v[v == "ARI"] <- "ARZ"
  v[v == "BAL"] <- "BLT"
  v[v == "CLE"] <- "CLV"
  v[v == "HOU"] <- "HST"
  v[v == "LAC" & season == 2016] <- "SD"
  v[v == "LV"  & season <= 2019] <- "OAK"
  list(v = v, n = sum(before != v, na.rm = TRUE))
}
for (tc in c("team", "team_name")) {
  fx <- canon_fix(full_pass_rush[[tc]], full_pass_rush$season)
  full_pass_rush[[tc]] <- fx$v
  cat("team-code fixes on ", tc, ": ", fx$n, " values recoded\n", sep = "")
}

# ------------------------------------------------------------
# 5. qbgrp join  (:111-116, verbatim; DEFENSE side: team_name = opp)
# ------------------------------------------------------------
if (!exists("combined_grade_epa_summary")) {
  combined_grade_epa_summary <- run_athena_query("
      SELECT  *
      FROM    nfl_data.combined_grade_epa_summary
  ")
}
full_pass_rush_qbgrp <-
  left_join(full_pass_rush %>% select(-c(scraped_at_kpis, scraped_at)),
            combined_grade_epa_summary %>%
              select(-c(passing_grade:good_epa_def_ind, off_epa_pass_perc:off_pass_gr_perc,
                        def_epa_pass_perc, def_pass_gr_perc)),
            by = c("team_name" = "opp", "week" = "week", "season" = "season"))
n_un <- sum(is.na(full_pass_rush_qbgrp$qbgrp_ssn))
cat("rows with no id match: ", n_un, " of ", nrow(full_pass_rush_qbgrp), "\n", sep = "")

# ------------------------------------------------------------
# 6. play_counts join  (:118-123) -- renames BY NAME, not by index
# ------------------------------------------------------------
if (!exists("play_counts")) {
  play_counts <- run_athena_query("
      SELECT  *
      FROM    nfl_data.play_counts
  ")
}
full_pass_rush_qbgrp <- left_join(full_pass_rush_qbgrp,
                                  play_counts %>% select(player_id, week, season, game_id, position),
                                  by = c("player_id", "week", "season"))
if (!all(c("position.x", "position.y") %in% names(full_pass_rush_qbgrp))) {
  cat("STOP -- expected position.x (PFF) and position.y (play_counts) after the join. columns:\n")
  print(names(full_pass_rush_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}
full_pass_rush_qbgrp <- full_pass_rush_qbgrp %>%
  rename(position = position.x, det_position = position.y)

# ------------------------------------------------------------
# 7. rate columns  (:174-184, verbatim)
# ------------------------------------------------------------
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

# percent_rank_avg: canon body (:187-191). shared_ne_2026_constants.R
# already defines it; this only fires if the step-0 is run alone.
if (!exists("percent_rank_avg")) {
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

# ------------------------------------------------------------
# 8. contract wall + receipts
# ------------------------------------------------------------
req_pr <- c("player", "player_id", "season", "week", "team", "team_name", "position",
            "det_position", "qbgrp_ssn", "def_ssn",
            "snap_counts_pass_rush", "true_pass_set_snap_counts_pass_rush",
            "true_pass_set_grades_pass_rush_defense", "true_pass_set_pass_rush_win_rate",
            "true_pass_set_prp", "true_pass_set_total_pressures", "true_pass_set_hurries",
            "grades_pass_rush_defense", "prp", "pass_rush_win_rate")
miss_pr <- setdiff(req_pr, names(full_pass_rush_qbgrp))
if (length(miss_pr)) {
  cat("STOP -- full_pass_rush_qbgrp lacks required columns:\n"); print(miss_pr)
  cat("frame columns:\n"); print(names(full_pass_rush_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}
ne_wk <- sort(unique(full_pass_rush_qbgrp$week[full_pass_rush_qbgrp$season == 2025 &
                                                startsWith(full_pass_rush_qbgrp$qbgrp_ssn, "NE")]))
cat("\nNE offense faced 2025: ", length(ne_wk), " weeks (want 21); playoff weeks: ",
    paste(ne_wk[ne_wk > 18], collapse = ","), " (want 28,29,30,32)\n", sep = "")
cat("--- band rows (DI / ED) by season ---\n")
print(full_pass_rush_qbgrp %>%
        filter(position %in% c("DI", "ED")) %>%
        count(season, position) %>%
        tidyr::pivot_wider(names_from = position, values_from = n), n = Inf)
rm(n_pre_gate, canon_fix, fx, tc, n_un, req_pr, miss_pr, ne_wk)
cat("full_pass_rush_qbgrp is in session -- the pass-rush chain can run.\n")
