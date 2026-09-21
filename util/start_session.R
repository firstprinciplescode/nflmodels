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
#   * It does NOT join the weather columns. rec_func_AWS.R and rush_func_AWS.R do that when you source them; twice = temp.x / temp.y.
#   * Safe to source again mid-session: whatever is already there and GOOD is left alone.
#
# Switches -- set one BEFORE sourcing to change it, e.g.  SS_LOAD_PBP <- TRUE
#   SS_LOAD_FRAMES  TRUE       qb_stats_df_final, xtd_proportion, receiving_func_base, the 21 rushing objects, receiver_registry
#   SS_LOAD_PBP     FALSE      combined_pbp (about 1.4 GB in memory) -- only when you need plays
#   SS_LOOKUPS      "session"  the ten comparison functions + scaled frames + importance matrices:
#                              "session" = the 2026-01-27 build, the set every workbook pushed so far and every saved tolerance was made with
#                                          (cache/comparison_lookups_from_workspace_2026-09-19.rds);
#                                          Loaded only where a lookup is MISSING; lookups already in the session are left alone and counted.
#                              "force"   = the same set, put in over whatever is there;
#                              "s3"      = load nothing here; the *_func_AWS.R files pull the 2026-04-01 rebuild from S3 when sourced;
#                              "none"    = leave whatever is in the session.
#   NOTE: every *_func_AWS.R file downloads the S3 (April) set again when it is sourced, which replaces the "session" set.

if (!exists("SS_LOAD_FRAMES")) SS_LOAD_FRAMES <- TRUE
if (!exists("SS_LOAD_PBP"))    SS_LOAD_PBP    <- FALSE
if (!exists("SS_LOOKUPS"))     SS_LOOKUPS     <- "session"
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
    want <- c("qb_stats_df_final", "xtd_proportion", "receiving_func_base", "cluster_join", "receiver_scheme_final", "combined_ids", SS_RUSH_BUNDLE)
    want <- want[!vapply(want, exists, logical(1), envir = G, inherits = FALSE)]
    if (length(want)) uncache_frames(want)
    if (!exists("receiver_registry", envir = G, inherits = FALSE) && file.exists(REGISTRY_FILE)) registry_back()
  }
  if (SS_LOAD_PBP && !exists("combined_pbp", envir = G, inherits = FALSE)) {
    pbp <- readRDS(file.path(SS_REPO, "cache/combined_pbp_GOOD_2026-09-19.rds")); assign("combined_pbp", pbp, envir = G)
    cat(sprintf("LOADED combined_pbp  (%d x %d; good-build test: %s)\n", nrow(pbp), ncol(pbp), "part_predicted_after_scramble_xtd" %in% names(pbp)))
  }
  if (SS_LOOKUPS %in% c("session", "force")) {
    lk <- readRDS(file.path(SS_REPO, "cache/comparison_lookups_from_workspace_2026-09-19.rds"))
    have <- names(lk)[vapply(names(lk), exists, logical(1), envir = G, inherits = FALSE)]
    put  <- if (SS_LOOKUPS == "force") names(lk) else setdiff(names(lk), have)
    for (k in put) assign(k, lk[[k]], envir = G)
    if (length(put)) cat(sprintf("LOADED %d comparison lookups, the 2026-01-27 build (the set the pushed workbooks and saved tolerances were made with)\n", length(put)))
    if (length(have) && SS_LOOKUPS != "force") {
      same <- vapply(have, function(k) isTRUE(all.equal(get(k, envir = G), lk[[k]], check.environment = FALSE)), logical(1))
      cat(sprintf("comparison lookups already in the session were LEFT ALONE: %d match the 2026-01-27 build, %d are a different build%s\n", sum(same), sum(!same),
                  if (any(!same)) "  (SS_LOOKUPS <- \"force\" then source again to swap them)" else ""))
    }
  }
  frames_check()
}
.ss_run()
