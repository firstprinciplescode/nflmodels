# ============================================================
# PIPELINE STATUS -- what is in the session, unit by unit, stage by
# stage, and which file makes each thing. Added 2026-09-12.
#
#   source("util/pipeline_status.R")
#   pipeline_status()                 # everything
#   pipeline_status("secondary")      # one unit
#
# Every (object, file) pair below was read off the code on 2026-09-12
# (the `<-` that creates the object, not a re-assignment). If a file's
# outputs change, change the row. This is the "where does X come from"
# answer, permanently -- and the verdict line per unit says which file
# to run next.
# ============================================================

PIPELINE_MAP <- tibble::tribble(
  ~unit,         ~stage,        ~object,                                 ~file,
  # ---- shared -------------------------------------------------------------
  "shared",      "base",        "combined_pbp",                          "load nfl_the_everything_workspace.RData  (or data_build/pbp_part_combined_join_AWS.R)",
  "shared",      "base",        "combined_grade_epa_summary",            "any step-0 pulls it  (built by data_build/pbp_nfl_base.R)",
  "shared",      "ids",         "combined_ids",                          "data_build/pff_ids_build_AWS.R",
  "shared",      "ids",         "combined_ids_defense",                  "data_build/pff_ids_build_defense_AWS.R",
  "shared",      "ids",         "id_xwalk",                              "data_build/pff_ids_validate_cross.R",
  "shared",      "ids",         "pff_team_lookup",                       "data_build/pff_ids_validate_cross.R",
  "shared",      "constants",   "opp_2026_teams",                        "pff_stats/shared_ne_2026_constants.R",
  "shared",      "constants",   "sched_2026",                            "pff_stats/shared_ne_2026_constants.R",
  "shared",      "constants",   "in_season",                             "pff_stats/shared_ne_2026_constants.R",
  "shared",      "constants",   "blend2",                                "pff_stats/shared_ne_2026_constants.R",
  "shared",      "constants",   "percent_rank_avg",                      "pff_stats/shared_ne_2026_constants.R  (canon def = pff_pass_rush_AWS.R:187; schedule fallbacks use a different scale)",
  # ---- run defense ---------------------------------------------------------
  "run_defense", "step0",       "run_defense_qbgrp",                     "pff_stats/run_defense/pff_run_defense_qbgrp_step0_AWS.R",
  "run_defense", "schedule",    "rundef_season_pctl_sos",                "pff_stats/run_defense/new_england_opp_run_defense_schedule.R",
  "run_defense", "schedule",    "cmp_rundef_slate",                      "pff_stats/run_defense/new_england_opp_run_defense_schedule.R",
  "run_defense", "schedule",    "team_band_2026",                        "pff_stats/run_defense/new_england_opp_run_defense_schedule.R",
  "run_defense", "c3",          "rd_c3_pctl",                            "pff_stats/run_defense/league_run_defense_evaluating_currency_three.R",
  "run_defense", "league",      "cur_rd_lg",                             "pff_stats/run_defense/league_opp_run_defense_schedule.R",
  "run_defense", "league",      "rot_2026_rd_lg",                        "pff_stats/run_defense/league_opp_run_defense_schedule.R",
  "run_defense", "league",      "league_rundef",                         "pff_stats/run_defense/league_opp_run_defense_schedule.R",
  "run_defense", "availability","members_ra",                            "pff_stats/run_defense/league_run_defense_availability.R",
  "run_defense", "availability","stint25_ra",                            "pff_stats/run_defense/league_run_defense_availability.R",
  "run_defense", "availability","sweep_ra",                              "pff_stats/run_defense/league_run_defense_availability.R",
  # ---- rushing (needs run_defense schedule first: composite pack) -----------
  "rushing",     "step0",       "rushing_qbgrp",                         "pff_stats/rushing/pff_rushing_qbgrp_step0_AWS.R",
  "rushing",     "schedule",    "rush_season_pctl_sos",                  "pff_stats/rushing/new_england_opp_rushing_schedule.R",
  "rushing",     "schedule",    "cmp_rush_slate",                        "pff_stats/rushing/new_england_opp_rushing_schedule.R",
  "rushing",     "schedule",    "team_rb_2026",                          "pff_stats/rushing/new_england_opp_rushing_schedule.R",
  "rushing",     "c3",          "ru_c3_pctl",                            "pff_stats/rushing/league_rushing_evaluating_currency_three.R",
  "rushing",     "league",      "cur_rb_lg",                             "pff_stats/rushing/league_opp_rushing_schedule.R",
  "rushing",     "league",      "rot_2026_rb_lg",                        "pff_stats/rushing/league_opp_rushing_schedule.R",
  "rushing",     "availability","memb_lg_ru",                            "pff_stats/rushing/league_rushing_availability.R",
  "rushing",     "availability","starters_all_ru",                       "pff_stats/rushing/league_rushing_availability.R",
  "rushing",     "availability","sweep_ru",                              "pff_stats/rushing/league_rushing_availability.R",
  # ---- pass rush -----------------------------------------------------------
  "pass_rush",   "step0",       "full_pass_rush_qbgrp",                  "pff_stats/pass_rush/pff_pass_rush_qbgrp_step0_AWS.R  (= pff_pass_rush_AWS.R:50-191 without the SEA-study tail)",
  "pass_rush",   "schedule",    "prush_tps_season_pctl_sos",             "pff_stats/pass_rush/new_england_opp_pass_rush_schedule.R",
  "pass_rush",   "schedule",    "cmp_prush_slate",                       "pff_stats/pass_rush/new_england_opp_pass_rush_schedule.R",
  "pass_rush",   "c3",          "prush_c3_pctl",                         "pff_stats/pass_rush/league_pass_rush_evaluating_currency_three.R",
  "pass_rush",   "league",      "cur_lg",                                "pff_stats/pass_rush/league_opp_pass_rush_schedule.R",
  "pass_rush",   "availability","members_pa",                            "pff_stats/pass_rush/league_pass_rush_availability.R",
  "pass_rush",   "availability","stint25_pa",                            "pff_stats/pass_rush/league_pass_rush_availability.R",
  "pass_rush",   "availability","sweep_pa",                              "pff_stats/pass_rush/league_pass_rush_availability.R",
  # ---- pass block (OL schedule is shared with run block) --------------------
  "pass_block",  "step0",       "all_pass_block_summary",                "pff_stats/pass_block/pff_pass_block_AWS.R",
  "pass_block",  "step0",       "all_pass_block_player_season_summary",  "pff_stats/pass_block/pff_pass_block_AWS.R",
  "pass_block",  "step0",       "tps_pass_block_player_season_summary",  "pff_stats/pass_block/pff_pass_block_AWS.R",
  "pass_block",  "schedule",    "ol_season_pctl",                        "pff_stats/pass_block/new_england_opp_ol_schedule.R",
  "pass_block",  "schedule",    "ne_2025_opp_ol_games",                  "pff_stats/pass_block/new_england_opp_ol_schedule.R",
  "pass_block",  "c3",          "pblk_c3_pctl",                          "pff_stats/pass_block/league_pass_block_evaluating_currency_three.R",
  "pass_block",  "league",      "league_sys_pb",                         "pff_stats/pass_block/league_opp_pass_blocking_schedule.R",
  "pass_block",  "league",      "slots_full",                            "pff_stats/pass_block/league_opp_pass_blocking_schedule.R",
  "pass_block",  "league",      "opp_map_ol",                            "pff_stats/pass_block/league_opp_pass_blocking_schedule.R",
  "pass_block",  "availability","starters_all_av",                       "pff_stats/pass_block/league_pass_block_availability.R",
  "pass_block",  "availability","slot_value_26_pb_build",                "pff_stats/pass_block/league_pass_block_availability.R",
  "pass_block",  "availability","sweep_avail_pb",                        "pff_stats/pass_block/league_pass_block_availability.R",
  # ---- run block -----------------------------------------------------------
  "run_block",   "step0",       "run_block_summary_qbgrp",               "pff_stats/run_block/pff_run_block_AWS.R",
  "run_block",   "step0",       "gap_player_season_summary",             "pff_stats/run_block/pff_run_block_AWS.R",
  "run_block",   "step0",       "zone_player_season_summary",            "pff_stats/run_block/pff_run_block_AWS.R",
  "run_block",   "schedule",    "ol_season_pctl",                        "pff_stats/pass_block/new_england_opp_ol_schedule.R  (shared OL schedule)",
  "run_block",   "c3",          "rblk_c3_pctl",                          "pff_stats/run_block/league_run_block_evaluating_currency_three.R",
  "run_block",   "league",      "league_sys_rb",                         "pff_stats/run_block/league_opp_run_blocking_schedule.R",
  "run_block",   "availability","starters_all_rb",                       "pff_stats/run_block/league_run_block_availability.R",
  "run_block",   "availability","slot_value_26_rb_build",                "pff_stats/run_block/league_run_block_availability.R",
  "run_block",   "availability","sweep_avail_rb",                        "pff_stats/run_block/league_run_block_availability.R",
  # ---- receiving -----------------------------------------------------------
  "receiving",   "step0",       "receiving_func_base",                   "pff_stats/receiving/receiving_stats_build_AWS.R  (needs combined_pbp, combined_ids, cluster_join)",
  "receiving",   "step0",       "receiver_scheme_final",                 "pff_stats/receiving/pff_receiving_man_zone_exploration_AWS.R",
  "receiving",   "schedule",    "rec_band_season",                       "pff_stats/receiving/new_england_opp_receiving_schedule.R",
  "receiving",   "schedule",    "rec_season_pctl_sos",                   "pff_stats/receiving/new_england_opp_receiving_schedule.R",
  "receiving",   "c3",          "rec_c3_pctl",                           "pff_stats/receiving/league_receiving_evaluating_currency_three.R",
  "receiving",   "league",      "league_sys_both",                       "pff_stats/receiving/league_opp_receiving_schedule.R",
  "receiving",   "league",      "league_wide",                           "pff_stats/receiving/league_opp_receiving_schedule.R",
  "receiving",   "availability","members_rc",                            "pff_stats/receiving/league_receiving_availability.R",
  "receiving",   "availability","corps25_rc",                            "pff_stats/receiving/league_receiving_availability.R",
  "receiving",   "availability","sweep_rc",                              "pff_stats/receiving/league_receiving_availability.R",
  # ---- secondary -----------------------------------------------------------
  "secondary",   "step0",       "coverage_built_mz",                     "pff_stats/secondary/pff_secondary_cache_step0_AWS.R  (writes coverage_raw_build_cache_cov.rds)",
  "secondary",   "step0",       "coverage_scheme",                       "pff_stats/secondary/pff_pass_coverage_AWS.R  (or the schedule pulls + caches it)",
  "secondary",   "schedule",    "secondary_qbgrp_mz",                    "pff_stats/secondary/new_england_opp_secondary_schedule.R",
  "secondary",   "schedule",    "modal_band_mz",                         "pff_stats/secondary/new_england_opp_secondary_schedule.R",
  "secondary",   "schedule",    "cmp_slate_mz",                          "pff_stats/secondary/new_england_opp_secondary_schedule.R",
  "secondary",   "schedule",    "cmp_unit_mz",                           "pff_stats/secondary/new_england_opp_secondary_schedule.R",
  "secondary",   "c3",          "cov_c3_pctl",                           "pff_stats/secondary/league_secondary_evaluating_currency_three.R",
  "secondary",   "league",      "sec_qbgrp_lg",                          "pff_stats/secondary/league_opp_secondary_schedule.R",
  "secondary",   "league",      "cov_pctl_sec_lg",                       "pff_stats/secondary/league_opp_secondary_schedule.R",
  "secondary",   "availability","members_cv",                            "pff_stats/secondary/league_secondary_availability.R",
  "secondary",   "availability","sweep_unit_cv",                         "pff_stats/secondary/league_secondary_availability.R",
  # ---- viewer --------------------------------------------------------------
  "viewer",      "functions",   "team_yoy",                              "pff_stats/evaluation/any_team_evaluation.R",
  "viewer",      "functions",   "team_report",                           "pff_stats/evaluation/any_team_evaluation.R"
)

.shape_of <- function(nm) {
  if (!exists(nm, envir = .GlobalEnv)) return("")
  x <- get(nm, envir = .GlobalEnv)
  if (is.data.frame(x)) return(paste0(nrow(x), " x ", ncol(x)))
  if (is.function(x))   return("fn")
  paste0(class(x)[1], "[", length(x), "]")
}

# the last 2025 week a game-level frame carries. 32 = through the Super
# Bowl (complete). 28 = a Jan-13 pull that stopped at wild-card weekend --
# the Sept 9 workspace's pass-rush / pass-block / run-block / coverage
# frames all did, and NE's faced diet came up 18 weeks instead of 21.
.wk25_of <- function(nm) {
  if (!exists(nm, envir = .GlobalEnv)) return("")
  x <- get(nm, envir = .GlobalEnv)
  if (!is.data.frame(x) || !all(c("week", "season") %in% names(x))) return("")
  w <- x$week[x$season == 2025]
  if (!length(w) || all(is.na(w))) return("")
  as.character(max(w, na.rm = TRUE))
}

pipeline_status <- function(units = NULL) {
  m <- PIPELINE_MAP
  if (!is.null(units)) m <- m[m$unit %in% units, ]
  m$status <- ifelse(vapply(m$object, exists, logical(1), envir = .GlobalEnv),
                     "LOADED", "MISSING")
  m$shape  <- vapply(m$object, .shape_of, character(1))
  m$wk25   <- vapply(m$object, .wk25_of, character(1))

  cat(sprintf("  %-13s %-8s %-38s %-14s %-5s %s\n", "stage", "status", "object", "shape",
              "wk25", "file   (wk25 = last 2025 week carried; 32 = complete, 28 = stale Jan-13 pull)"))
  for (u in unique(m$unit)) {
    sub <- m[m$unit == u, ]
    cat("\n== ", u, " ==\n", sep = "")
    for (i in seq_len(nrow(sub)))
      cat(sprintf("  %-13s %-8s %-38s %-14s %-5s %s\n", sub$stage[i], sub$status[i],
                  sub$object[i], sub$shape[i], sub$wk25[i], sub$file[i]))
  }
  stale <- m$object[m$status == "LOADED" & m$wk25 != "" & suppressWarnings(as.numeric(m$wk25)) < 32]
  if (length(stale))
    cat("\n  STALE -- 2025 stops before the Super Bowl (week 32): ",
        paste(stale, collapse = ", "), "\n  re-run those step-0 files; do not trust the workspace copies\n", sep = "")

  cat("\n== verdict (the viewer prints a unit only when its availability stage is loaded) ==\n")
  for (u in setdiff(unique(m$unit), c("shared", "viewer"))) {
    sub    <- m[m$unit == u, ]
    stages <- unique(sub$stage)
    done   <- vapply(stages, function(s) all(sub$status[sub$stage == s] == "LOADED"), logical(1))
    if (all(done)) {
      cat(sprintf("  %-12s COMPLETE\n", u))
    } else {
      s <- stages[!done][1]
      cat(sprintf("  %-12s stops at %-12s -> source(\"%s\")\n", u, s,
                  sub("\\s+\\(.*$", "", unique(sub$file[sub$stage == s])[1])))
    }
  }
  sh <- m[m$unit == "shared", ]
  if (any(sh$status == "MISSING"))
    cat("\n  shared objects MISSING: ", paste(sh$object[sh$status == "MISSING"], collapse = ", "),
        "  -- run Stage 1-3 of REBUILD_RUNBOOK.md first\n", sep = "")
  invisible(m)
}
