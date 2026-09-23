# ============================================================
# PFF SECONDARY -- STEP 0: the Phase 6 cache, rebuilt as a file
# (2026-09-12; STAMPED 2026-09-13 by Andy: 69,250 rows / band4 LB 24,279 S 16,948 CB 14,503 SCB 5,686 -- exact match to the 08-16 ritual)
#
# WHAT THIS MAKES: coverage_raw_build_cache_cov.rds in getwd() and
#   coverage_built_mz in session -- the UN-GATED coverage player-game
#   frame (raw nfl_data.coverage_summary + final_position, team-fixed).
#
# WHY THIS FILE EXISTS: new_england_opp_secondary_schedule.R:171-173
#   and league_opp_secondary_schedule.R:182-184 STOP with
#   "cache absent -- run phase6_step0_secondary_gate_ritual.txt first".
#   That ritual was a .txt of console code, never committed, and the
#   .rds it wrote is on no machine. This is the ritual as a file.
#
# WHAT THE CACHE HAS TO BE (read off the consumers, not remembered):
#   - UN-GATED: "MODAL BAND MAP on the UN-GATED frame (design law)"
#     (schedule line 315). So NO snap filter -- the tower's
#     pff_pass_coverage_AWS.R:124 gate is deliberately NOT applied.
#   - final_position with an SCB class: band4 is built from
#     final_position (schedule line 323-327). The tower derives it from
#     coverage_summary_by_game's position (pff_pass_coverage_AWS.R:
#     102-139, "adv_position"). Same case_when here, verbatim.
#   - team-fixed: stopifnot(!team %in% c(ARI,BAL,CLE,HOU)) at line 187.
#   - grade + snaps resolve BY NAME: grades_coverage_defense,
#     snap_counts_coverage (line 234-237). Split facets come from the
#     SCHEME table at run time (feed 2) -- NOT from this cache.
#
# RECEIPTS TO MATCH (header, ritual v1.1, 2026-08-16):
#   rows 69,250 (the scheme join: "69,250 = 69,250, no fan-out");
#   Band4 vocab on the graded/snapped frame: LB 24,279 / S 16,948 /
#   CB 14,503 / SCB 5,686. Printed below for comparison.
#
# SOURCE ORDER: after data_build ids + shared_ne_2026_constants.R,
#   BEFORE new_england_opp_secondary_schedule.R. Never rm()s.
#   Re-source the WHOLE file after any edit (ONE SESSION LAW).
# ============================================================

library(dplyr); library(stringr)

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

CACHE_COV <- "coverage_raw_build_cache_cov.rds"   # the name the consumers read

# ------------------------------------------------------------
# 1. raw pulls -- coverage_summary (the frame) + coverage_summary_
#    by_game (the detailed position that births SCB). Both RAW.
# ------------------------------------------------------------
cov_raw <- tibble::as_tibble(run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_summary
"))
cat("--- coverage_summary pulled: ", nrow(cov_raw), " rows x ", ncol(cov_raw),
    " cols  (ritual receipt 2026-08-16: 69,250) ---\n", sep = "")

by_game_raw <- tibble::as_tibble(run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_summary_by_game
"))
cat("--- coverage_summary_by_game pulled: ", nrow(by_game_raw), " rows x ",
    ncol(by_game_raw), " cols ---\n", sep = "")

# ------------------------------------------------------------
# 1b. coverage_scheme -- FEED 2 of both schedule files. They take an
#     in-session object over the cache over a pull, so a stale session
#     copy wins silently (the Sept 9 workspace's stops at week 28 -- a
#     Jan-13 pull; NE's 2025 faced diet would come up 18 weeks, not 21).
#     Pull fresh here, replace the session copy, write the cache the
#     schedule files name.
# ------------------------------------------------------------
SCHEME_CACHE <- "coverage_scheme_cache_mz.rds"
coverage_scheme <- tibble::as_tibble(run_athena_query("
    SELECT  *
    FROM    nfl_data.coverage_scheme
"))
w25_sch <- sort(unique(coverage_scheme$week[coverage_scheme$season == 2025]))
cat("--- coverage_scheme pulled: ", nrow(coverage_scheme), " rows; 2025 playoff weeks: ",
    paste(w25_sch[w25_sch > 18], collapse = ","), "  (want 28,29,30,32) ---\n", sep = "")
saveRDS(coverage_scheme, SCHEME_CACHE)
cat("scheme cache written -> ", normalizePath(SCHEME_CACHE), "\n", sep = "")

# ------------------------------------------------------------
# 2. column walls -- BY NAME (tower landmine: it renamed by index)
# ------------------------------------------------------------
req_cov <- c("player", "player_id", "franchise_id", "week", "season",
             "team", "team_name", "position",
             "grades_coverage_defense", "snap_counts_coverage")
miss_cov <- setdiff(req_cov, names(cov_raw))
if (length(miss_cov)) {
  cat("STOP -- coverage_summary lacks required columns:\n"); print(miss_cov)
  cat("frame columns:\n"); print(names(cov_raw))
  stop("column receipt printed above -- no-placeholder law")
}

# the by-game detailed position: the tower took column 6 and called it
# adv_position. Resolve by name; print candidates and stop if none fit.
adv_col <- intersect(c("adv_position", "position", "pos"), names(by_game_raw))[1]
if (is.na(adv_col)) {
  cat("STOP -- no position column on coverage_summary_by_game. columns:\n")
  print(names(by_game_raw))
  stop("stamp the detailed-position column name by hand")
}
key_bg <- intersect(c("player_id", "franchise_id", "week", "season"), names(by_game_raw))
if (!all(c("player_id", "week", "season") %in% key_bg))
  stop("coverage_summary_by_game lacks player_id/week/season join keys")
cat("by-game detailed position column: ", adv_col,
    " | join keys: ", paste(key_bg, collapse = ", "), "\n", sep = "")
cat("distinct ", adv_col, " values:\n", sep = "")
print(sort(unique(by_game_raw[[adv_col]])))

adv <- by_game_raw %>%
  select(all_of(key_bg), adv_position = all_of(adv_col)) %>%
  distinct()
dup_bg <- adv %>% count(across(all_of(key_bg))) %>% filter(n > 1)
if (nrow(dup_bg)) {
  cat("STOP -- by_game position is not unique on its keys:\n"); print(head(dup_bg, 10))
  stop("join would fan out -- evidence printed above")
}

# ------------------------------------------------------------
# 3. build -- left join adv_position, derive final_position
#    (pff_pass_coverage_AWS.R:128-139 verbatim), team fixes,
#    NO snap gate.
# ------------------------------------------------------------
n_pre <- nrow(cov_raw)
cov_built <- cov_raw %>%
  left_join(adv, by = key_bg)
stopifnot(nrow(cov_built) == n_pre)
cat("adv_position attached: ", sum(!is.na(cov_built$adv_position)), " of ",
    n_pre, " rows\n", sep = "")

cov_built <- cov_built %>%
  mutate(
    final_position = case_when(
      adv_position == "MLB"                     ~ "MLB",
      str_detect(adv_position, "LB$")           ~ "LB",
      adv_position %in% c("FS", "SS")           ~ "S",
      adv_position == "SCB"                     ~ "SCB",
      str_detect(adv_position, "CB")            ~ "CB",
      adv_position %in% c("DE","ED","DI","DT","NT","DLE","DRE","DLT","DRT") ~ "DL",
      TRUE                                       ~ NA_character_
    )
  )

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
  fx <- canon_fix(cov_built[[tc]], cov_built$season)
  cov_built[[tc]] <- fx$v
  cat("team-code fixes on ", tc, ": ", fx$n, " values recoded\n", sep = "")
}
bad_tm <- c("ARI", "BAL", "CLE", "HOU")
stopifnot(!any(cov_built$team %in% bad_tm), !any(cov_built$team_name %in% bad_tm))

# ------------------------------------------------------------
# 4. receipts -- compare to the ritual header
# ------------------------------------------------------------
cat("\n--- band4 vocab on the graded/snapped frame ",
    "(ritual: LB 24,279 / S 16,948 / CB 14,503 / SCB 5,686) ---\n", sep = "")
print(cov_built %>%
        mutate(band4 = case_when(final_position == "SCB" ~ "SCB",
                                 final_position %in% c("MLB", "LB") ~ "LB",
                                 TRUE ~ final_position)) %>%
        filter(band4 %in% c("CB", "SCB", "S", "LB"),
               !is.na(grades_coverage_defense), snap_counts_coverage > 0) %>%
        count(band4) %>% arrange(desc(n)))
cat("\n--- coverage by season ---\n")
print(cov_built %>%
        group_by(season) %>%
        summarise(rows = n(), players = n_distinct(player_id),
                  weeks = n_distinct(week),
                  no_adv = sum(is.na(adv_position)), .groups = "drop"), n = Inf)
w25_cov <- sort(unique(cov_built$week[cov_built$season == 2025]))
cat("2025 playoff weeks in the cache: ", paste(w25_cov[w25_cov > 18], collapse = ","),
    "  (want 28,29,30,32 -- NE faced 21 weeks)\n", sep = "")

# ------------------------------------------------------------
# 4b. POSITION WALL (2026-09-22) -- same wall as pff_pass_coverage_AWS.R.
#     When Athena lost coverage_summary_by_game's 2023 partitions, all of 2023 came
#     back with no adv_position and old frames labeled the season "DE". Normal rate
#     here is ~0%. Stop BEFORE writing the cache if more than 2% of a season, or more
#     than half of any one week, has no position.
# ------------------------------------------------------------
pos_wall_s0 <- cov_built %>%
  group_by(season, week) %>%
  summarise(rows = n(), no_position = sum(is.na(adv_position)), .groups = "drop") %>%
  group_by(season) %>%
  mutate(season_share = sum(no_position) / sum(rows)) %>%
  ungroup() %>%
  filter(season_share > 0.02 | no_position / rows > 0.5)
if (nrow(pos_wall_s0) > 0) {
  print(as.data.frame(pos_wall_s0 %>% filter(no_position > 0)))
  stop("POSITION WALL: coverage_summary_by_game gave no position for the season-weeks above; cache NOT written. ",
       "Athena probably can't see those partitions: run  MSCK REPAIR TABLE nfl_data.coverage_summary_by_game;  ",
       "in the Athena console (or run the dbt build), then source this file again.")
} else {
  cat("POSITION WALL OK: every season-week has its position\n")
}

# ------------------------------------------------------------
# 5. write the cache the consumers read + leave it in session
# ------------------------------------------------------------
saveRDS(cov_built, CACHE_COV)
coverage_built_mz <- cov_built
cat("\ncache written -> ", normalizePath(CACHE_COV), "  (", nrow(cov_built),
    " rows, ", format(Sys.time(), "%Y-%m-%d %H:%M"), ")\n", sep = "")
rm(cov_raw, by_game_raw, adv, dup_bg, cov_built, n_pre, req_cov, miss_cov,
   adv_col, key_bg, canon_fix, fx, tc, bad_tm)
cat("coverage_built_mz is in session -- the secondary chain can run.\n")
