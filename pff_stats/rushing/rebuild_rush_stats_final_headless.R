# rebuild_rush_stats_final_headless.R
# PROPOSED -- Claude, UNSIGNED, 2026-09-20. New file; it edits nothing and never runs inside the RStudio session.
#
# WHY: rush_stats_final in the session kept coming up without playoff weeks 30 / 32. No file in the rushing chain drops a week --
#   the frame in the session was an old copy that rode in with a saved workspace image. six_final builds only from session objects,
#   so an old pbp_rush / rushing_summary_rank decides which weeks exist and nothing warns. This script makes a fresh one from disk + Athena.
#
# RUN IT (Terminal panel, from the repo root -- NOT the RStudio console):
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" pff_stats/rushing/rebuild_rush_stats_final_headless.R
# ~3 minutes. Then, in RStudio, load it yourself:
#   rush_stats_final <- readRDS("cache/rush_stats_final.rds")          # then run rush_func_AWS.R lines 144-147 once (the weather join)
#
# WRITES: cache/rush_stats_final.rds + every other object the rushing tools read, all from the SAME build (the RUSH_BUNDLE list at the
#   bottom: the cluster tables, the *_full frames rushing_stats_comparison.R reads, and a_/b_/c_dashboard). It refuses to write unless
#   (1) every season 2016-2025 has 4 conference team-games (week 30) and 2 Super Bowl team-games (week 32), and
#   (2) every player-season keeps the cluster number it has in the build already sitting in cache/.
# CLUSTER NUMBERS: an object left in the session from an older build (c_dashboard, situation_cluster_df, the *_full frames ...) keeps its OLD
#   numbers and its old group count no matter what rush_stats_final you load. Bring the whole set in together:
#   source("util/cache_frames.R"); uncache_frames(<the RUSH_BUNDLE names>)

# 2026 rows are dropped from every Athena pull (2026 is quarantined while 2025 is being worked; combined_pbp has no 2026 anyway).

if (interactive()) stop("Run this from the Terminal with Rscript, NOT inside RStudio -- it replaces View() and loads combined_pbp into whatever session runs it.")
t0 <- Sys.time()
repo <-"C:/Users/vflre/Downloads/nflmodels_UPDATE"; setwd(repo)
options(width = 220, dplyr.summarise.inform = FALSE, warn = 1)
suppressWarnings(suppressPackageStartupMessages({   # util/R_packages.R lists packages this machine does not have (plyr, factoextra, ...), so load only what the chain calls
  library(sqldf); library(tidyr); library(dplyr); library(tidyverse); library(conflicted); library(cluster); library(rpart); library(rpart.plot)
}))
View <- function(...) invisible(NULL)          # no data viewer in a headless process
grDevices::pdf(NULL)                           # plots go nowhere
MAX_SEASON <- 2025

errors <- list()
run_file <- function(f) {
  cat("\n\n######## ", f, " ########\n", sep = "")
  ex <- parse(f, keep.source = TRUE); sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    ln <- sr[[i]][1]
    msg <- tryCatch({ eval(ex[[i]], envir = globalenv()); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(msg)) { cat(sprintf("  [line %d] ERROR, skipped: %s\n", ln, msg)); errors[[length(errors) + 1]] <<- data.frame(file = basename(f), line = ln, error = substr(msg, 1, 200)) }
    e <- ex[[i]]                               # x <- run_athena_query(...)  ->  drop 2026 rows right after the pull
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "run_athena_query") && is.name(e[[2]])) {
      nm <- as.character(e[[2]]); x <- get(nm, envir = globalenv())
      if (is.data.frame(x) && "season" %in% names(x)) { n0 <- nrow(x); x <- x[x$season <= MAX_SEASON, , drop = FALSE]; assign(nm, x, envir = globalenv())
        cat(sprintf("  [line %d] %s: %d rows pulled, %d rows of season > %d dropped, max 2025 week = %s\n", ln, nm, n0, n0 - nrow(x), MAX_SEASON, max(x$week[x$season == 2025]))) }
    }
  }
}

combined_pbp <- readRDS("cache/combined_pbp_GOOD_2026-09-19.rds")
stopifnot("part_predicted_after_scramble_xtd" %in% names(combined_pbp))      # the good-build test
combined_ids <- readRDS("cache/combined_ids.rds")
cat("combined_pbp:", nrow(combined_pbp), "x", ncol(combined_pbp), "  combined_ids:", nrow(combined_ids), "\n")

chain <- file.path("pff_stats/rushing", c("pff_rushing_stats_build_one_AWS.R", "pff_rushing_stats_build_two_situation_AWS.R", "pff_rushing_stats_build_three_gap_AWS.R",
                                          "pff_rushing_stats_build_four_type_AWS.R", "pff_rushing_stats_build_five_xtd_AWS.R", "pff_rushing_stats_build_six_final.R"))
for (f in chain) { run_file(f); cat(sprintf("  -- done %s  (%.1f min elapsed)\n", basename(f), as.numeric(difftime(Sys.time(), t0, units = "mins")))); invisible(gc()) }

cat("\n\n==================== ERRORS LOGGED ====================\n")
if (length(errors)) print(do.call(rbind, errors), row.names = FALSE, right = FALSE) else cat("none\n")

# ---------------------------------------------------------------- walls
stopifnot(exists("rush_stats_final"), is.data.frame(rush_stats_final))
rsf <- as.data.frame(dplyr::ungroup(rush_stats_final))
cat("\n==================== rush_stats_final ====================\n", nrow(rsf), " x ", ncol(rsf), "\n", sep = ""); print(names(rsf))
grid <- rsf %>% dplyr::filter(week >= 28) %>% dplyr::group_by(season, week) %>% dplyr::summarise(tg = dplyr::n_distinct(qbgrp_ssn), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = week, values_from = tg, names_prefix = "wk")
cat("\nplayoff team-games per season (expect 8 or 12 / 8 / 4 / 2):\n"); print(as.data.frame(grid), row.names = FALSE)
chk <- intersect(c("team", "position_group", "rank_grp", "attempts", "situation_cluster", "gap_cluster", "xtd_percentile", "gap_z", "rush_share"), names(rsf))
cat("\n2025: rows and NA share by week of the columns rush_func filters on:\n")
print(as.data.frame(rsf %>% dplyr::filter(season == 2025) %>% dplyr::group_by(week) %>% dplyr::summarise(rows = dplyr::n(), dplyr::across(dplyr::all_of(chk), ~ round(mean(is.na(.)), 2)))), row.names = FALSE)
ok <- all(c("wk30", "wk32") %in% names(grid)) && all(grid$wk30 == 4) && all(grid$wk32 == 2) && max(rsf$season) == MAX_SEASON
cat("\nWALL -- every season 2016-2025 has 4 conference team-games and 2 Super Bowl team-games, no 2026:", ok, "\n")
if (!ok) stop("wall failed: nothing was cached")

# WALL -- cluster numbers must not move. If cache/ already holds a build, this run has to give every player-season the SAME
# situation / gap cluster number, or nothing is written (the label notes in rush_func_AWS.R are keyed to the numbers in cache/).
same_labels <- function(nm, keys) {
  f <- file.path("cache", paste0(nm, ".rds")); if (!file.exists(f)) return(TRUE)
  old <- as.data.frame(dplyr::ungroup(readRDS(f))); new <- as.data.frame(dplyr::ungroup(get(nm)))
  j <- dplyr::full_join(old[, c(keys, "cluster")], new[, c(keys, "cluster")], by = keys, suffix = c("_old", "_new"))
  moved <- sum(!(is.na(j$cluster_old) & is.na(j$cluster_new)) & (is.na(j$cluster_old) | is.na(j$cluster_new) | j$cluster_old != j$cluster_new))
  cat(sprintf("  %-22s %5d player-seasons in cache, %5d now, %d with a different cluster number\n", nm, nrow(old), nrow(new), moved)); moved == 0
}
cat("\nWALL -- cluster numbers vs the build already in cache/:\n")
keys <- c("rusher_player_id", "posteam", "season", "rank_grp")
if (!all(c(same_labels("situation_cluster_df", keys), same_labels("gap_cluster_df", keys)))) stop("cluster numbers moved: nothing was cached. Do not overwrite cache/ until the label notes are redone.")
cat("groups per rank -- situation:", paste(tapply(situation_cluster_df$cluster, situation_cluster_df$rank_grp, function(x) length(unique(x[!is.na(x)]))), collapse = " / "),
    "  gap:", paste(tapply(gap_cluster_df$cluster, gap_cluster_df$rank_grp, function(x) length(unique(x[!is.na(x)]))), collapse = " / "), "  (A / B / C)\n")

# EVERYTHING the rushing tools read comes from this ONE build, so a session can be brought in line with one uncache_frames() call:
#   rush_func_AWS.R / rush engines -> rush_stats_final;  rushing_stats_comparison.R -> the *_full frames, base_run_gap_cluster, rusher_xpass_diff_df;
#   a_/b_/c_dashboard are the tables you look at to read what a cluster number means.
dir.create("cache", showWarnings = FALSE)
RUSH_BUNDLE <- c("rush_stats_final", "situation_cluster_df", "gap_cluster_df", "rusher_xpass_diff_df",
                 "rusher_xpass_diff_a_full", "rusher_xpass_diff_b_full", "rusher_xpass_diff_c_full",
                 "combined_rush_summation_final_a", "combined_rush_summation_final_b", "combined_rush_summation_final_c",
                 "base_run_gap_cluster", "run_gap_a_full", "run_gap_b_full", "run_gap_c_full",
                 "rushing_summary_rank", "rush_order_df", "player_zone_gap_zscore", "rusher_xtd_final", "a_dashboard", "b_dashboard", "c_dashboard")
miss <- RUSH_BUNDLE[!vapply(RUSH_BUNDLE, exists, TRUE)]
if (length(miss)) stop("the chain did not make: ", paste(miss, collapse = ", "), " -- nothing was cached")
for (nm in RUSH_BUNDLE) saveRDS(get(nm), file.path("cache", paste0(nm, ".rds")))
cat("\nCACHED ", length(RUSH_BUNDLE), " objects from this one build: ", paste(RUSH_BUNDLE, collapse = ", "), "\n", sep = "")
cat(sprintf("total %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
