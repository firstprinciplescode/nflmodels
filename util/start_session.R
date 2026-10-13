# start_session.R
# PROPOSED -- Claude, UNSIGNED, 2026-09-20. New file.
#
# RUN THIS FIRST in every R session:
#   source("C:/Users/vflre/Downloads/nflmodels_UPDATE/util/start_session.R")
#
# In order: packages -> working directory = the repo -> helper functions -> the good frames from cache/ -> one check table.
#   * It never deletes anything and never replaces a key frame that checks GOOD. The rushing objects and the receiver cluster tables are
#     kept as ONE build: a member left over from an older build (other cluster numbers) is swapped for the cache copy, old one kept.
#   * A key frame that checks GOOD is left alone. A frame that checks STALE (an old copy that came in
#     with a workspace) is replaced from cache/ and the old one is kept beside it as <name>_STALE_<mmdd_HHMM>.
#   * A coverage frame that fails the position check (any "DE", or a season with no / one position) is kept as <name>_BAD_<time>
#     and the real name is removed, with a message saying which file to rebuild (added 2026-09-22 after the 2023 "DE" frames came back).
#   * It does NOT join the weather columns. rec_func_AWS.R and rush_func_AWS.R do that when you source them; twice = temp.x / temp.y.
#   * Safe to source again mid-session: whatever is already there and GOOD is left alone.
#
# Switches -- set one BEFORE sourcing to change it, e.g.  SS_LOAD_PBP <- TRUE
#   SS_LOAD_FRAMES  TRUE       qb_stats_df_final, xtd_proportion, receiving_func_base, the rushing objects, receiver_registry, and the
#                              seven-unit objects the waterfall / bridge cards need (members_*, slot_value_26_*, coverage frames, ids, constants)
#   SS_LOAD_PBP     FALSE      combined_pbp (about 1.4 GB in memory) -- only when you need plays
#   SS_LOOKUPS      "april"    the ten comparison functions + scaled frames + importance matrices:
#                              "april" = APRIL ONLY (Andy 2026-09-27: "IT'S ALL APRIL"). Sources util/load_april_lookups.R, which removes
#                                        any lookup already in the session and loads the 2026-04-01 set from S3.
#                              "none"  = leave whatever is in the session.
#   The January (2026-01-27) set is RETIRED: its cache file was moved to archive/2026-09-27_january_lookups_retired/.
#   NOTE: every *_func_AWS.R file downloads the S3 (April) set again when it is sourced, so re-source util/load_april_lookups.R after it.

if (!exists("SS_LOAD_FRAMES")) SS_LOAD_FRAMES <- TRUE
if (!exists("SS_LOAD_PBP"))    SS_LOAD_PBP    <- FALSE
if (!exists("SS_LOOKUPS"))     SS_LOOKUPS     <- "april"
if (SS_LOOKUPS %in% c("session", "force", "s3")) { cat("SS_LOOKUPS \"", SS_LOOKUPS, "\" is retired (January set gone) -- using \"april\"\n", sep = ""); SS_LOOKUPS <- "april" }
SS_REPO <- "C:/Users/vflre/Downloads/nflmodels_UPDATE"

# ---- 1. packages (the ones in util/R_packages.R that are installed on this machine, same order, plus cluster / rpart for the rushing build)
SS_PACKAGES <- c("nflreadr", "nflfastR", "sqldf", "openxlsx", "tidyr", "dplyr", "tidyverse", "readxl", "xgboost", "stringr", "mgcv",
                 "aws.s3", "DBI", "noctua", "curl", "xml2", "httr", "paws", "rpart", "rpart.plot", "cluster", "MASS", "conflicted", "patchwork", "gt")
SS_RUSH_BUNDLE <- c("rush_stats_final", "situation_cluster_df", "gap_cluster_df", "rusher_xpass_diff_df",
                    "rusher_xpass_diff_a_full", "rusher_xpass_diff_b_full", "rusher_xpass_diff_c_full",
                    "combined_rush_summation_final_a", "combined_rush_summation_final_b", "combined_rush_summation_final_c",
                    "base_run_gap_cluster", "run_gap_a_full", "run_gap_b_full", "run_gap_c_full",
                    "rushing_summary_rank", "rush_order_df", "player_zone_gap_zscore", "rusher_xtd_final",
                    "rush_stats_high", "rush_stats_low", "rush_stats_rec", "a_dashboard", "b_dashboard", "c_dashboard")
# the seven-unit objects waterfall_bridge / impact_bridge / the cards need (cached from the 09-18 session on 2026-09-21)
SS_UNIT_OBJECTS <- c("combined_grade_epa_summary", "combined_ids_defense", "id_xwalk", "pff_team_lookup", "in_season", "blend2", "percent_rank_avg",
                     "opp_2026_teams", "sched_2026", "members_cv", "members_pa", "members_ra", "members_rc", "memb_lg_ru",
                     "slot_value_26_pb_build", "slot_value_26_rb_build", "starters_all_av", "starters_all_rb",
                     "coverage_summary", "coverage_man_player_season_summary", "coverage_zone_player_season_summary", "final_coverage_df_qbgrp",
                     "full_pass_rush_qbgrp", "run_block_summary_qbgrp", "run_defense_qbgrp", "rushing_qbgrp", "all_pass_block_summary",
                     "all_pass_block_player_season_summary", "tps_pass_block_player_season_summary", "gap_player_season_summary", "zone_player_season_summary",
                     "coverage_scheme", "coverage_built_mz", "team_yoy", "team_report")
`%ni%` <- Negate(`%in%`)

# everything below runs inside one function, so no loop variable of this file can land on top of a variable of yours
.ss_run <- function() {
  G <- globalenv()
  missing_pk <- SS_PACKAGES[!vapply(SS_PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
  for (pk in setdiff(SS_PACKAGES, missing_pk)) suppressWarnings(suppressPackageStartupMessages(library(pk, character.only = TRUE)))
  conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
  if (length(missing_pk)) cat("not installed, skipped:", paste(missing_pk, collapse = ", "), "\n")

  # ---- 2. working directory + helpers (these three files only define functions)
  setwd(SS_REPO)
  for (f in c("util/cache_frames.R", "util/frames_guard.R", "util/pipeline_status.R")) sys.source(file.path(SS_REPO, f), envir = G)

  # ---- 3. frames
  stamp <- format(Sys.time(), "%m%d_%H%M")
  same_shape <- function(x, ref) if (is.data.frame(ref)) is.data.frame(x) && identical(dim(x), dim(ref)) && identical(names(x), names(ref)) else
    is.list(x) && identical(names(x), names(ref)) && identical(dim(x$percs), dim(ref$percs))
  if (SS_LOAD_FRAMES) {
    chk <- frames_check(quiet = TRUE)
    for (fr in chk$frame[chk$status == "STALE"])                                                     # old copy kept as <name>_STALE_<time>
      tryCatch(frames_restore(fr, weather = FALSE), error = function(e) cat("COULD NOT RESTORE ", fr, ": ", conditionMessage(e), "\n", sep = ""))
    # the rushing objects + the receiver cluster tables come from ONE build each. A member that does not match the cache copy is an older
    # build with OTHER cluster numbers, so it is swapped (old one kept as <name>_STALE_<time>) -- never mix numberings.
    swapped <- character(0)
    for (b in c(setdiff(SS_RUSH_BUNDLE, "rush_stats_final"), "cluster_join", "receiver_scheme_final")) {
      fb <- file.path(SS_REPO, "cache", paste0(b, ".rds")); if (!file.exists(fb) || !exists(b, envir = G, inherits = FALSE)) next
      ref <- readRDS(fb); cur <- get(b, envir = G, inherits = FALSE)
      if (!same_shape(cur, ref)) { assign(paste0(b, "_STALE_", stamp), cur, envir = G); assign(b, ref, envir = G); swapped <- c(swapped, b) }
    }
    if (length(swapped)) cat("OLDER BUILD in the session, replaced from cache (old copies kept as <name>_STALE_", stamp, "): ", paste(swapped, collapse = ", "), "\n", sep = "")
    want <- c("qb_stats_df_final", "xtd_proportion", "receiving_func_base", "cluster_join", "receiver_scheme_final", "combined_ids", SS_RUSH_BUNDLE, SS_UNIT_OBJECTS)
    want <- want[file.exists(file.path(SS_REPO, "cache", paste0(want, ".rds")))]
    want <- want[!vapply(want, exists, logical(1), envir = G, inherits = FALSE)]
    if (length(want)) uncache_frames(want)
    # ---- 3b. COVERAGE POSITION CHECK (2026-09-22). The January coverage frames had ALL of 2023 labeled "DE" (Athena could not
    #      see that season's positions when they were built), and they rode back in through cache/ every session. A coverage frame
    #      is BAD if it has any "DE", if a season is nearly all one position, or if a season has more than 5% rows with no position.
    #      A bad frame is not left under its real name: it is kept as <name>_BAD_<time> and the real name is removed, so nothing
    #      downstream reads it by mistake. Fix = rebuild from pff_stats/secondary/pff_pass_coverage_AWS.R, then cache_frames().
    ss_cov_bad <- function(x) {
      if (!is.data.frame(x) || !all(c("final_position", "season") %in% names(x))) return(NULL)
      if (any(x$final_position == "DE", na.rm = TRUE)) return("has \"DE\" positions (an old build)")
      for (s in sort(unique(x$season))) {
        p <- x$final_position[which(x$season == s)]; if (length(p) < 100) next
        if (mean(is.na(p)) > 0.05) return(sprintf("season %s: %.0f%% of rows have no position", s, 100 * mean(is.na(p))))
        tb <- table(p); if (length(tb) && max(tb) / sum(tb) > 0.9) return(sprintf("season %s is nearly all one position (%s)", s, names(tb)[which.max(tb)]))
      }
      NULL
    }
    bad_cov <- character(0)
    for (cf in c("coverage_man_player_season_summary", "coverage_zone_player_season_summary", "coverage_slot_player_season_summary",
                 "coverage_combined_player_season_summary", "final_coverage_df_qbgrp", "coverage_built_mz")) {
      if (!exists(cf, envir = G, inherits = FALSE)) next
      why <- ss_cov_bad(get(cf, envir = G, inherits = FALSE)); if (is.null(why)) next
      assign(paste0(cf, "_BAD_", stamp), get(cf, envir = G, inherits = FALSE), envir = G); rm(list = cf, envir = G)
      bad_cov <- c(bad_cov, cf); cat("BAD COVERAGE FRAME ", cf, ": ", why, " -- kept as ", cf, "_BAD_", stamp, ", real name removed\n", sep = "")
    }
    if (length(bad_cov)) cat("REBUILD them: pff_stats/secondary/pff_pass_coverage_AWS.R makes the summaries and final_coverage_df_qbgrp;",
                             " pff_stats/secondary/pff_secondary_cache_step0_AWS.R makes coverage_built_mz (both have a position wall). Then run:\n",
                             "   cache_frames(\"", paste(bad_cov, collapse = "\", \""), "\")\nDo not use the _BAD_ copies.\n", sep = "")
    if (!exists("receiver_registry", envir = G, inherits = FALSE) && file.exists(REGISTRY_FILE)) registry_back()
  }
  if (SS_LOAD_PBP && !exists("combined_pbp", envir = G, inherits = FALSE)) {
    pbp <- readRDS(file.path(SS_REPO, "cache/combined_pbp_GOOD_2026-09-19.rds")); assign("combined_pbp", pbp, envir = G)
    cat(sprintf("LOADED combined_pbp  (%d x %d; good-build test: %s)\n", nrow(pbp), ncol(pbp), "part_predicted_after_scramble_xtd" %in% names(pbp)))
  }
  if (SS_LOOKUPS == "april") sys.source(file.path(SS_REPO, "util/load_april_lookups.R"), envir = G)
  frames_check()
}
.ss_run()


# source("C:/Users/vflre/Downloads/nflmodels_UPDATE/pff_stats/evaluation/waterfall_bridge.R")

# safe_load("C:/Users/vflre/Downloads/nflmodels_UPDATE/nfl_the_everything_workspace.RData", big = TRUE)