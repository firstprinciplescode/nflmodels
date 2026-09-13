# ============================================================
# PFF RUN DEFENSE -- STEP 0 (v3 rebuilt as a file 2026-09-12; STAMPED
# 2026-09-13 by Andy: raw 102,126 / unmatched 0 / 2025 pools DI 165 ED 146 LB 105 S 106 -- all match the 08-14 header)
#
# WHAT THIS MAKES: run_defense_qbgrp -- RAW run_defense_summary
#   player-game rows (NO snap filter) + qbgrp_ssn / def_ssn ids.
#
# WHY THIS FILE EXISTS: new_england_opp_run_defense_schedule.R,
#   league_run_defense_evaluating_currency_three.R,
#   league_opp_run_defense_schedule.R, league_run_defense_availability.R
#   AND the composite-pack section of BOTH rushing schedule files
#   (new_england_opp_rushing_schedule.R:611, league_opp_rushing_
#   schedule.R:629) all wall on run_defense_qbgrp. Its "step 0 v3"
#   was only ever run in a session (2026-08-14) and never saved.
#
# RECEIPTS TO MATCH (from the run-defense header, 2026-08-14):
#   "raw pull 102,126 rows (session table was filtered >= 14 snaps,
#    ~48% of games deleted -- old gate priced: X14-G10 sees 1.03
#    ED/team; dead). qbgrp join via combined_grade_epa_summary:
#    0 unmatched rows."
#   => this file pulls RAW (the >= 14 filter at pff_run_defense_AWS.R:66
#      is deliberately NOT applied) and expects 0 unmatched. Rows may
#      exceed 102,126 if 2026 weeks have landed since 08-14.
#
# RECIPE (canon = pff_run_defense_AWS.R lines 50-92, minus the filter):
#   1. pull nfl_data.run_defense_summary from Athena (SELECT *)
#   2. team-code fixes: ARI->ARZ, BAL->BLT, CLE->CLV, HOU->HST,
#      LAC-2016->SD, LV<=2019->OAK   (team + team_name)
#   3. join qbgrp_ssn / def_ssn from combined_grade_epa_summary on
#      team_name = opp / week / season  (DEFENSE side joins on opp --
#      same key the secondary file uses at line 182)
#   4. contract wall: the columns the run-defense chain reads
#   The FB/WR drop at pff_run_defense_AWS.R:85 is NOT applied here:
#   every consumer filters position %in% c("DI","ED","LB","S") itself.
#
# SOURCE ORDER: after data_build ids + shared_ne_2026_constants.R,
#   BEFORE new_england_opp_run_defense_schedule.R. Never rm()s.
#   Re-source the WHOLE file after any edit (ONE SESSION LAW).
# ============================================================

library(dplyr)

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
# 1. pull run_defense_summary -- RAW, no snap filter
# ------------------------------------------------------------
run_defense_raw <- tibble::as_tibble(run_athena_query("
    SELECT  *
    FROM    nfl_data.run_defense_summary
"))
cat("--- run_defense_summary pulled: ", nrow(run_defense_raw), " rows x ",
    ncol(run_defense_raw), " cols  (header receipt 2026-08-14: 102,126) ---\n",
    sep = "")
cat("distinct position values (bands DI/ED/LB/S are filtered downstream):\n")
print(sort(unique(run_defense_raw$position)))

# ------------------------------------------------------------
# 2. team-code fixes (canon list; applied to team and team_name)
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
for (tc in intersect(c("team", "team_name"), names(run_defense_raw))) {
  fx <- canon_fix(run_defense_raw[[tc]], run_defense_raw$season)
  run_defense_raw[[tc]] <- fx$v
  cat("team-code fixes on ", tc, ": ", fx$n, " values recoded\n", sep = "")
}

# ------------------------------------------------------------
# 3. join qbgrp_ssn / def_ssn from combined_grade_epa_summary
#    DEFENSE side: the defender's team_name is the offense's opp.
#    (frame reused if already in session)
# ------------------------------------------------------------
if (!exists("combined_grade_epa_summary")) {
  combined_grade_epa_summary <- run_athena_query("
      SELECT  *
      FROM    nfl_data.combined_grade_epa_summary
  ")
}
combined_grade_epa_summary <- tibble::as_tibble(combined_grade_epa_summary)

id_cols_rd <- c("opp", "week", "season", "qbgrp_ssn", "def_ssn")
miss_id_rd <- setdiff(id_cols_rd, names(combined_grade_epa_summary))
if (length(miss_id_rd)) {
  cat("STOP -- combined_grade_epa_summary lacks id columns:\n")
  print(miss_id_rd)
  cat("frame columns:\n"); print(names(combined_grade_epa_summary))
  stop("column receipt printed above -- no-placeholder law")
}
ids_rd <- combined_grade_epa_summary %>% select(all_of(id_cols_rd)) %>% distinct()
dup_rd <- ids_rd %>% count(opp, week, season) %>% filter(n > 1)
if (nrow(dup_rd)) {
  cat("STOP -- id table is not unique on opp/week/season:\n")
  print(dup_rd)
  stop("join would fan out -- evidence printed above")
}

if (!"team_name" %in% names(run_defense_raw)) {
  cat("STOP -- run_defense_summary has no team_name column.\n")
  cat("frame columns:\n"); print(names(run_defense_raw))
  stop("column receipt printed above -- no-placeholder law")
}
cat("join key: team_name = opp (+ week, season)\n")
run_defense_qbgrp <- left_join(
  run_defense_raw, ids_rd,
  by = c("team_name" = "opp", "week" = "week", "season" = "season")
)
n_un_rd <- sum(is.na(run_defense_qbgrp$qbgrp_ssn))
cat("rows with no id match: ", n_un_rd, " of ", nrow(run_defense_qbgrp),
    "  (header receipt: 0)\n", sep = "")
if (n_un_rd == nrow(run_defense_qbgrp)) {
  cat("offending join keys (first 20):\n")
  print(run_defense_qbgrp %>% filter(is.na(qbgrp_ssn)) %>%
          distinct(team_name, season) %>% head(20))
  stop("join matched ZERO rows -- key coding mismatch, evidence above")
} else if (n_un_rd > 0) {
  cat("unmatched (team_name, season) cells (first 20) -- left as holes, never filled:\n")
  print(run_defense_qbgrp %>% filter(is.na(qbgrp_ssn)) %>%
          distinct(team_name, season) %>% head(20))
}

# ------------------------------------------------------------
# 4. contract wall -- every column the run-defense chain reads
#    (c3 req_rd + the schedule's currency summarise, lines 77-101)
# ------------------------------------------------------------
req_rd0 <- c("player", "player_id", "position", "season", "week", "team",
             "team_name", "qbgrp_ssn", "def_ssn",
             "snap_counts_run", "grades_run_defense", "grades_tackle",
             "stops", "run_stop_opp", "tackles", "missed_tackles",
             "avg_depth_of_tackle")
miss_rd0 <- setdiff(req_rd0, names(run_defense_qbgrp))
if (length(miss_rd0)) {
  cat("STOP -- run_defense_qbgrp lacks required columns:\n")
  print(miss_rd0)
  cat("frame columns:\n"); print(names(run_defense_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}

# ------------------------------------------------------------
# 5. coverage receipt
# ------------------------------------------------------------
cat("--- run_defense_qbgrp coverage ---\n")
print(run_defense_qbgrp %>%
        group_by(season) %>%
        summarise(rows    = n(),
                  players = dplyr::n_distinct(player_id),
                  weeks   = dplyr::n_distinct(week),
                  band_rows = sum(position %in% c("DI", "ED", "LB", "S")),
                  .groups = "drop"), n = Inf)
rm(run_defense_raw, ids_rd, dup_rd, id_cols_rd, miss_id_rd, n_un_rd,
   req_rd0, miss_rd0, canon_fix, fx, tc)
cat("run_defense_qbgrp is in session -- the run-defense chain can run.\n")
