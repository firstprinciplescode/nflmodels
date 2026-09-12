# ============================================================
# PFF RUSHING -- STEP 0: rushing_qbgrp   (REBUILT 2026-09-13,
# awaiting Andy's stamp)
#
# WHAT THIS MAKES: rushing_qbgrp -- HB game-level rushing rows
#   (raw rushing_summary columns) + qbgrp_ssn / def_ssn ids.
#
# WHY THIS FILE EXISTS: every rushing canon file ASSUMES
#   rushing_qbgrp is already in session. The house header in
#   new_england_opp_rushing_schedule.R says it plainly:
#     "(1) step-0 file in session: rushing_qbgrp (raw +
#      qbgrp/def ids, HB-only, canonical team fixes incl
#      LAC-2016 -> SD, LV <= 2019 -> OAK)"
#   That step-0 was never saved as its own file, so a fresh
#   session can never rebuild it. This is that file. The join
#   is modeled on pff_run_block_AWS.R (canon pattern).
#
# RECIPE (canon):
#   1. pull nfl_data.rushing_summary from Athena (SELECT *)
#   2. filter to HB
#   3. team-code fixes: ARI->ARZ, BAL->BLT, CLE->CLV, HOU->HST,
#      LAC-2016->SD, LV<=2019->OAK   (team + team_name)
#   4. join qbgrp_ssn / def_ssn out of combined_grade_epa_summary
#      on team(+name) / week / season  (same join as run block)
#   5. contract wall: player, player_id, season, week, team,
#      def_ssn, attempts, grades_run, elu_rush_mtf
#      (currency-three consumes the first 8; the availability
#      layer's gate adds elu_rush_mtf)
#
# SOURCE ORDER: this file runs FIRST in the rushing chain,
#   BEFORE new_england_opp_rushing_schedule.R. It never rm()s
#   anything -- canon files do not nuke the session.
#   Re-source the WHOLE file after any edit (ONE SESSION LAW).
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 0. Athena helper (house pattern: AWS CLI -> S3 -> read.csv).
#    Only defined if an earlier AWS file has not already.
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
# 1. pull rushing_summary
# ------------------------------------------------------------
rushing_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.rushing_summary
")
rushing_summary <- tibble::as_tibble(rushing_summary)
cat("--- rushing_summary pulled:", nrow(rushing_summary), "rows x",
    ncol(rushing_summary), "cols ---\n")

# ------------------------------------------------------------
# 2. HB filter (receipt printed: the values seen, the rows kept)
# ------------------------------------------------------------
pos_col <- intersect(c("position", "position_group"), names(rushing_summary))[1]
if (is.na(pos_col)) {
  cat("STOP -- no position/position_group column on rushing_summary.\n")
  cat("frame columns:\n"); print(names(rushing_summary))
  stop("column receipt printed above -- no-placeholder law")
}
cat("distinct ", pos_col, " values before the HB cut:\n", sep = "")
print(sort(unique(rushing_summary[[pos_col]])))
rushing_summary <- rushing_summary %>% filter(.data[[pos_col]] == "HB")
cat("HB rows kept:", nrow(rushing_summary), "\n")
if (!nrow(rushing_summary))
  stop("HB filter kept ZERO rows -- position values printed above")

# ------------------------------------------------------------
# 3. team-code fixes (canon list; applied to team and team_name)
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
for (tc in intersect(c("team", "team_name"), names(rushing_summary))) {
  fx <- canon_fix(rushing_summary[[tc]], rushing_summary$season)
  rushing_summary[[tc]] <- fx$v
  cat("team-code fixes on ", tc, ": ", fx$n, " values recoded\n", sep = "")
}

# ------------------------------------------------------------
# 4. join qbgrp_ssn / def_ssn from combined_grade_epa_summary
#    (same join the run-block chain uses; frame reused if an
#    earlier AWS file already pulled it)
# ------------------------------------------------------------
if (!exists("combined_grade_epa_summary")) {
  combined_grade_epa_summary <- run_athena_query("
      SELECT  *
      FROM    nfl_data.combined_grade_epa_summary
  ")
}
combined_grade_epa_summary <- tibble::as_tibble(combined_grade_epa_summary)

id_cols <- c("posteam", "week", "season", "qbgrp_ssn", "def_ssn")
miss_id <- setdiff(id_cols, names(combined_grade_epa_summary))
if (length(miss_id)) {
  cat("STOP -- combined_grade_epa_summary lacks id columns:\n")
  print(miss_id)
  cat("frame columns:\n"); print(names(combined_grade_epa_summary))
  stop("column receipt printed above -- no-placeholder law")
}
ids <- combined_grade_epa_summary %>% select(all_of(id_cols)) %>% distinct()
dup <- ids %>% count(posteam, week, season) %>% filter(n > 1)
if (nrow(dup)) {
  cat("STOP -- id table is not unique on posteam/week/season:\n")
  print(dup)
  stop("join would fan out -- evidence printed above")
}

join_key <- if ("team_name" %in% names(rushing_summary)) "team_name" else "team"
cat("join key: ", join_key, " = posteam (+ week, season)\n", sep = "")
rushing_qbgrp <- left_join(
  rushing_summary, ids,
  by = stats::setNames(c("posteam", "week", "season"),
                       c(join_key, "week", "season"))
)
n_un <- sum(is.na(rushing_qbgrp$def_ssn))
cat("rows with no id match: ", n_un, " of ", nrow(rushing_qbgrp), "\n", sep = "")
if (n_un == nrow(rushing_qbgrp)) {
  cat("offending join keys (first 20):\n")
  print(rushing_qbgrp %>%
          filter(is.na(def_ssn)) %>%
          distinct(across(all_of(c(join_key, "season")))) %>%
          head(20))
  stop("join matched ZERO rows -- key coding mismatch, evidence above")
} else if (n_un > 0) {
  cat("unmatched (join_key, season) cells (first 20) -- left as holes, never filled:\n")
  print(rushing_qbgrp %>%
          filter(is.na(def_ssn)) %>%
          distinct(across(all_of(c(join_key, "season")))) %>%
          head(20))
}

# ------------------------------------------------------------
# 5. contract wall -- what the canon chain consumes
# ------------------------------------------------------------
req <- c("player", "player_id", "season", "week", "team",
         "def_ssn", "attempts", "grades_run", "elu_rush_mtf")
miss <- setdiff(req, names(rushing_qbgrp))
if (length(miss)) {
  cat("STOP -- rushing_qbgrp lacks required columns:\n")
  print(miss)
  cat("frame columns:\n"); print(names(rushing_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}

# ------------------------------------------------------------
# 6. coverage receipt
# ------------------------------------------------------------
cat("--- rushing_qbgrp coverage ---\n")
print(rushing_qbgrp %>%
        group_by(season) %>%
        summarise(rows    = n(),
                  players = dplyr::n_distinct(player_id),
                  weeks   = dplyr::n_distinct(week),
                  .groups = "drop"))
cat("rushing_qbgrp is in session -- the rushing chain can run.\n")
