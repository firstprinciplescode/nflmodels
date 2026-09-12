# rec_comparison_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# The receiving analog of stats_comparison_engine.R. Replaces the three
# hardcoded tolerance blocks in rec_func_AWS.R (lines 172-176, 309-313,
# 452-456 -- three different sets, none of them the tuned one) with one
# table pair, rec_tol_qb / rec_tol_def, filled from rec_sweep. rec_func,
# weather_rec_func, precip_rec_func are ported with the same signatures
# and the same output columns; only where the thresholds come from changed.
# rec_push_all runs every receiver in rec_specs through all three.
#
# Source order (each file defines, nothing fires except the table block):
#   source("stats_comparison_engine.R")        # lookups, stats_categories, stats_tol_or_stop
#   source("comparison_engine_thresholds.R")   # tuner; its tail defines xtds_tol_qb / xtds_tol_def
#   source("rec_thresholds_engine.R")          # rec_template_sea / _ne -> rec_specs_sea / _ne, matchup frames, rec_sweep
#   source("rec_comparison_engine.R")          # this file
#   rec_tune(rec_specs_sea, rec_matchups_sea, xtds_tol_qb,  xtds_tol_def)    # SEA offense: 1 matchup
#   rec_tune(rec_specs_ne,  rec_matchups_ne,  stats_tol_qb, stats_tol_def)   # NE offense: 6 matchups, merges in
#   rec_push_all(rec_specs_ne, rec_matchups_ne)                              # 10 x 6 x 3 = 180 workbooks -> S3
#
# Session needs: receiving_func_base WITH temp, wind, rain_ind, snow_ind
# joined on (the rec_func_AWS.R line 96 left_join from qb_stats_df_final);
# the ten comparison_*_func lookups; dplyr, tidyr, openxlsx, aws.s3.
#
# Three departures from rec_func_AWS.R, all PROPOSED:
#   1. man_z_na: the template puts a trailing NA on Horton's and Foster's
#      man_z band. rec_func's chain had no NA branch on z_score_percentile
#      (xpass and xtd have one). The three functions now take man_z_na
#      (default FALSE = canon behavior); the runner passes what the
#      template says. rec_hits in rec_thresholds_engine.R does the same,
#      so sweep counts and workbook counts agree cell for cell.
#   2. S3 object names carry the matchup: "Rec - Cooper Kupp - SEADarnold-2025
#      vs NE2025.xlsx". The canon name "Rec - Cooper Kupp.xlsx" overwrites
#      itself across matchups. Sheet names stay the canon 31-char stems.
#   3. write_s3 = FALSE returns the results invisibly with no workbook, the
#      same as stats_run.

REC_BUCKET <- "nfl-pff-data-lucas"

# ---------------------------------------------------------------------------
# 1. TOLERANCE TABLES. rec_tune() overwrites these in-session; the banked
#    block is the last stamped sweep so the runner works without re-tuning.
# ---------------------------------------------------------------------------
if (!exists("rec_tol_qb") || !exists("rec_tol_def")) {
  rec_tol_qb <- list(
    "SEADarnold-2025" = c(blitz = 0.960, depth = 0.920, less = 0.995, pa = 1.130, pressure = 0.950)
  )
  rec_tol_def <- list(
    "NE2025" = c(blitz = 0.960, depth = 1.015, less = 1.050, pa = 1.035, pressure = 0.965)
  )
  REC_TOL_STAMP <- "BANKED (rec_sweep 2026-09-07, symmetric, SEADarnold-2025 vs NE2025)"
}

# per (qbgrp, defgrp, lens): tuned base + the largest symmetric delta any
# reachable receiver needed. A QB that faces two defenses takes the max
# across both (surplus is free, shortfall costs -- Andy's rule from the
# tuner). A cell that hit the cap without clearing stops here with its
# name: no table with the cap in it leaves this function.
rec_tables_from_sweep <- function(sw) {
  unreach <- unique(sw$rec[sw$n_profile < sw$floor])
  ok  <- sw[!sw$rec %in% unreach, ]
  bad <- ok[!ok$cleared, ]
  if (nrow(bad))
    stop("rec_sweep hit the cap without clearing: ",
         paste(unique(paste(bad$rec, bad$lens)), collapse = ", "),
         " -- not handing over a table with the cap in it. Rule on those cells first.")
  lenses <- names(stats_categories)
  one_side <- function(key, base_col) {
    parts <- split(ok, ok[[key]])
    lapply(parts, function(x)
      stats::setNames(vapply(lenses, function(L) {
        y <- x[x$lens == L, ]
        if (!nrow(y)) stop("no sweep rows for ", key, " ", x[[key]][1], " lens ", L)
        y[[base_col]][1] + max(y$delta)
      }, numeric(1)), lenses))
  }
  list(qb = one_side("qbgrp", "qb_base"), def = one_side("defgrp", "def_base"), unreach = unreach)
}

# rec_tune: symmetric sweep, then MERGE the resulting entities into
# rec_tol_qb / rec_tol_def in session (entity-level replace, other
# entities untouched) so the SEA side and the NE side accumulate in one
# table the way the stats engine's do. Stamp carries both runs.
rec_tune <- function(specs, matchups, tol_qb, tol_def, deltas = seq(0, 0.35, by = 0.005)) {
  cat("base tables cover QB: ", paste(names(tol_qb), collapse = ", "),
      " | DEF: ", paste(names(tol_def), collapse = ", "), "\n", sep = "")
  sw <- rec_sweep(specs, matchups, tol_qb = tol_qb, tol_def = tol_def, deltas = deltas, side = "both")
  tb <- rec_tables_from_sweep(sw)
  cur_qb  <- if (exists("rec_tol_qb",  envir = globalenv())) get("rec_tol_qb",  envir = globalenv()) else list()
  cur_def <- if (exists("rec_tol_def", envir = globalenv())) get("rec_tol_def", envir = globalenv()) else list()
  cur_qb[names(tb$qb)]   <- tb$qb
  cur_def[names(tb$def)] <- tb$def
  assign("rec_tol_qb",  cur_qb,  envir = globalenv())
  assign("rec_tol_def", cur_def, envir = globalenv())
  run <- paste0("rec_sweep ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (symmetric, ",
                length(specs), " receivers x ", nrow(matchups), " matchups: ",
                paste(unique(matchups$qbgrp), collapse = "/"), " vs ", paste(unique(matchups$defgrp), collapse = "/"),
                if (length(tb$unreach)) paste0("; EXCLUDED ", paste(tb$unreach, collapse = ", ")) else "", ")")
  prev <- if (exists("REC_TOL_STAMP", envir = globalenv())) get("REC_TOL_STAMP", envir = globalenv()) else ""
  assign("REC_TOL_STAMP", if (nzchar(prev) && !grepl("^BANKED", prev)) paste(prev, run, sep = " | ") else run,
         envir = globalenv())
  cat("\nrec_tol_qb now covers: ", paste(names(cur_qb), collapse = ", "),
      "\nrec_tol_def now covers: ", paste(names(cur_def), collapse = ", "),
      "\nREC_TOL_STAMP: ", get("REC_TOL_STAMP", envir = globalenv()), "\n", sep = "")
  invisible(sw)
}

# per-call thresholds in the shape process_category expects. Same lens
# order as stats_categories, which is the order the plain sheet's
# start_rows (1, 5, 9, 13, 17) were built for.
rec_categories <- function(qbgrp_one, defgrp_one) {
  tq <- stats_tol_or_stop(rec_tol_qb,  qbgrp_one,  "QB")
  td <- stats_tol_or_stop(rec_tol_def, defgrp_one, "defense")
  lenses <- names(stats_categories)
  miss <- c(setdiff(lenses, names(tq)), setdiff(lenses, names(td)))
  if (length(miss)) stop("rec_tol tables are missing lens: ", paste(unique(miss), collapse = ", "))
  lapply(stats::setNames(lenses, lenses), function(L)
    list(qb_threshold = tq[[L]], def_threshold = td[[L]],
         qb_func = stats_categories[[L]]$qb_func, def_func = stats_categories[[L]]$def_func))
}

# ---------------------------------------------------------------------------
# 2. THE FILTER CHAIN AND GAME-LEVEL SUMMARY. One copy. rec_func_AWS.R
#    carried it three times and they had already drifted (the plain
#    version computes notrel_* means the other two dropped). The superset
#    is computed for every mode; weather / precip only read the six
#    selected metrics off it, exactly as before.
# ---------------------------------------------------------------------------
rec_game_level <- function(qb_teams, def_teams, mode,
                           tgt_cluster_input, rte_cluster_input, align_cluster_input,
                           position_group_input, pos_rank_vec, team_rank_vec,
                           man_zone_grp_input, man_z_vec_input, man_z_na,
                           xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input) {
  d <- receiving_func_base %>%
    dplyr::ungroup() %>%
    dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
    dplyr::mutate(
      ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
      rec_ind = ifelse(
        tgt_cluster_name %in% tgt_cluster_input &
          rte_cluster_name %in% rte_cluster_input &
          align_cluster_name %in% align_cluster_input &
          final_position_group %in% position_group_input &
          pos_rank >= pos_rank_vec[1] & pos_rank <= pos_rank_vec[2] &
          team_rank >= team_rank_vec[1] & team_rank <= team_rank_vec[2] &
          man_zone_grp_cluster %in% man_zone_grp_input &
          ((z_score_percentile >= man_z_vec_input[1] & z_score_percentile <= man_z_vec_input[2]) |
             (man_z_na & is.na(z_score_percentile))) &
          ((xpass_percentile >= xpass_vec_input[1] & xpass_percentile <= xpass_vec_input[2]) |
             (xpass_na & is.na(xpass_percentile))) &
          td_grp_cluster %in% xtd_grp_input &
          ((xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) |
             (xtd_grp_na & is.na(xtd_percentile))),
        "In", "Out"))
  
  if (mode == "weather") {
    if (!all(c("temp", "wind") %in% names(d)))
      stop("receiving_func_base has no temp / wind -- run the rec_func_AWS.R line 96 join first")
    d <- d %>% dplyr::mutate(wind_na = ifelse(is.na(wind), 0, wind),
                             bucket = ifelse(wind_na >= 10 | temp <= 50, "1_Bad", "2_Good"))
  } else if (mode == "precip") {
    if (!all(c("rain_ind", "snow_ind") %in% names(d)))
      stop("receiving_func_base has no rain_ind / snow_ind -- run the rec_func_AWS.R line 96 join first")
    d <- d %>% dplyr::mutate(bucket = ifelse(snow_ind == 1 | rain_ind == 1, "1_Precip", "2_Clear"))
  } else {
    d <- d %>% dplyr::mutate(bucket = "all")
  }
  
  d %>%
    dplyr::mutate(has_rel_recs = any(rec_ind == "In")) %>%
    dplyr::filter(has_rel_recs) %>%
    dplyr::group_by(qbgrp_ssn, def_ssn, week, season, ind, bucket) %>%
    dplyr::filter(sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
    dplyr::summarise(
      rel_players = sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE),
      notrel_players = sum(ifelse(rec_ind != "In", 1, 0), na.rm = TRUE),
      rel_targets = sum(ifelse(rec_ind == "In", targets, 0), na.rm = TRUE),
      notrel_targets = sum(ifelse(rec_ind != "In", targets, 0), na.rm = TRUE),
      rel_routes = sum(ifelse(rec_ind == "In", routes, 0), na.rm = TRUE),
      notrel_routes = sum(ifelse(rec_ind != "In", routes, 0), na.rm = TRUE),
      rel_pbp_xtds = sum(ifelse(rec_ind == "In", pbp_rec_xtds, 0), na.rm = TRUE),
      notrel_pbp_xtds = sum(ifelse(rec_ind != "In", pbp_rec_xtds, 0), na.rm = TRUE),
      rel_part_xtds = sum(ifelse(rec_ind == "In", part_rec_xtds, 0), na.rm = TRUE),
      notrel_part_xtds = sum(ifelse(rec_ind != "In", part_rec_xtds, 0), na.rm = TRUE),
      rel_ypa = mean(ifelse(rec_ind == "In", ypa, NA), na.rm = TRUE),
      notrel_ypa = mean(ifelse(rec_ind != "In", ypa, NA), na.rm = TRUE),
      rel_pbp_xypa = mean(ifelse(rec_ind == "In", pbp_xypa, NA), na.rm = TRUE),
      notrel_pbp_xypa = mean(ifelse(rec_ind != "In", pbp_xypa, NA), na.rm = TRUE),
      rel_part_xypa = mean(ifelse(rec_ind == "In", part_xypa, NA), na.rm = TRUE),
      notrel_part_xypa = mean(ifelse(rec_ind != "In", part_xypa, NA), na.rm = TRUE),
      rel_acc_rate = mean(ifelse(rec_ind == "In", acc_rate, NA), na.rm = TRUE),
      notrel_acc_rate = mean(ifelse(rec_ind != "In", acc_rate, NA), na.rm = TRUE),
      rel_fastr_cp = mean(ifelse(rec_ind == "In", fastr_cp, NA), na.rm = TRUE),
      notrel_fastr_cp = mean(ifelse(rec_ind != "In", fastr_cp, NA), na.rm = TRUE),
      rel_pbp_cp = mean(ifelse(rec_ind == "In", pbp_cp, NA), na.rm = TRUE),
      notrel_pbp_cp = mean(ifelse(rec_ind != "In", pbp_cp, NA), na.rm = TRUE),
      rel_part_cp = mean(ifelse(rec_ind == "In", part_cp, NA), na.rm = TRUE),
      notrel_part_cp = mean(ifelse(rec_ind != "In", part_cp, NA), na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::mutate(
      tgt_shr = (rel_targets / (rel_targets + notrel_targets)) / rel_players,
      rte_shr = (rel_routes / (rel_routes + notrel_routes)) / rel_players,
      pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
      part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players) %>%
    dplyr::ungroup()
}

# plain: rec_func_AWS.R lines 247-262, same columns same order
rec_plain_summary <- function(game_level) {
  game_level %>%
    dplyr::group_by(ind) %>%
    dplyr::summarise(
      players = dplyr::n(),
      tgt_shr = mean(tgt_shr, na.rm = TRUE),
      rte_shr = mean(rte_shr, na.rm = TRUE),
      pbp_xtd_shr = mean(pbp_xtd_shr, na.rm = TRUE),
      part_xtd_shr = mean(part_xtd_shr, na.rm = TRUE),
      ypa = mean(rel_ypa, na.rm = TRUE),
      pbp_xypa = mean(rel_pbp_xypa, na.rm = TRUE),
      part_xypa = mean(rel_part_xypa, na.rm = TRUE),
      acc_rate = mean(rel_acc_rate, na.rm = TRUE),
      fastr_cp = mean(rel_fastr_cp, na.rm = TRUE),
      pbp_cp = mean(rel_pbp_cp, na.rm = TRUE),
      part_cp = mean(rel_part_cp, na.rm = TRUE),
      .groups = "drop")
}

# weather / precip: rec_func_AWS.R lines 380-425 / 522-567. Computed on
# generic bucket columns then renamed to the canon labels, so the output
# header is byte-identical to the old workbooks (n_Bad ... d_Good_vs_All /
# n_Precip ... d_Clear_vs_All). The 18-column wall guards the rename.
rec_bucket_spec <- list(
  weather = list(levels = c("1_Bad", "2_Good"),     labels = c("Bad", "Good")),
  precip  = list(levels = c("1_Precip", "2_Clear"), labels = c("Precip", "Clear")))

rec_bucket_summary <- function(game_level, mode) {
  spec <- rec_bucket_spec[[mode]]; lv <- spec$levels; lab <- spec$labels
  selected_metrics <- c("tgt_shr", "rte_shr", "pbp_xtd_shr", "part_xtd_shr", "rel_ypa", "rel_acc_rate")
  res <- game_level %>%
    tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "metric", values_to = "value") %>%
    dplyr::group_by(ind, metric) %>%
    dplyr::summarise(
      n_1 = sum(bucket == lv[1], na.rm = TRUE),
      n_2 = sum(bucket == lv[2], na.rm = TRUE),
      n_All = dplyr::n(),
      mean_1 = mean(value[bucket == lv[1]], na.rm = TRUE),
      mean_2 = mean(value[bucket == lv[2]], na.rm = TRUE),
      mean_All = mean(value, na.rm = TRUE),
      overall_sd = sd(value, na.rm = TRUE),
      wilcox_pval = tryCatch(wilcox.test(value ~ bucket, exact = FALSE)$p.value, error = function(e) NA),
      .groups = "drop") %>%
    dplyr::mutate(
      diff_1_vs_2 = mean_1 - mean_2,
      diff_1_vs_All = mean_1 - mean_All,
      diff_2_vs_All = mean_2 - mean_All,
      d_1_vs_2 = diff_1_vs_2 / overall_sd,
      d_1_vs_All = diff_1_vs_All / overall_sd,
      d_2_vs_All = diff_2_vs_All / overall_sd,
      sig = dplyr::case_when(wilcox_pval < 0.01 ~ "***", wilcox_pval < 0.05 ~ "**",
                             wilcox_pval < 0.10 ~ "*", TRUE ~ ""),
      effect_size = dplyr::case_when(abs(d_1_vs_2) >= 0.8 ~ "large", abs(d_1_vs_2) >= 0.5 ~ "medium",
                                     abs(d_1_vs_2) >= 0.2 ~ "small", TRUE ~ "negligible")) %>%
    dplyr::arrange(ind, metric)
  canon <- c("ind", "metric",
             paste0("n_", lab[1]), paste0("n_", lab[2]), "n_All",
             paste0("mean_", lab[1]), paste0("mean_", lab[2]), "mean_All",
             "overall_sd", "wilcox_pval",
             paste0("diff_", lab[1], "_vs_", lab[2]), paste0("diff_", lab[1], "_vs_All"), paste0("diff_", lab[2], "_vs_All"),
             paste0("d_", lab[1], "_vs_", lab[2]), paste0("d_", lab[1], "_vs_All"), paste0("d_", lab[2], "_vs_All"),
             "sig", "effect_size")
  if (ncol(res) != length(canon)) stop("rec_bucket_summary: ", ncol(res), " columns, canon has ", length(canon))
  names(res) <- canon
  res
}

# ---------------------------------------------------------------------------
# 3. THE THREE FUNCTIONS. Same names, same arguments, same outputs as
#    rec_func_AWS.R, plus man_z_na (default FALSE = canon) and write_s3.
# ---------------------------------------------------------------------------
# comparison pools depend only on (entity, lens, tolerance). 180 worker
# calls on the NE side share 25 distinct pools; cache them for the session.
# rec_tune() changes the tolerances, so the key carries the tolerance and a
# stale entry can never be read.
rec_pool_cache <- new.env()
rec_pool <- function(which, f, entity, L, tol) {
  key <- paste(which, entity, L, format(tol, nsmall = 3), sep = "|")
  if (is.null(rec_pool_cache[[key]])) rec_pool_cache[[key]] <- c(f(entity, tol)$QB, entity)
  rec_pool_cache[[key]]
}

rec_worker <- function(mode, qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input,
                       align_cluster_input, position_group_input, pos_rank_vec, team_rank_vec,
                       man_zone_grp_input, man_z_vec_input, xpass_na, xpass_vec_input,
                       xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
                       man_z_na = FALSE, write_s3 = TRUE) {
  categories <- rec_categories(qbgrp_one, defgrp_one)
  results <- lapply(names(categories), function(L) {
    category  <- categories[[L]]
    qb_teams  <- rec_pool("qb",  category$qb_func,  qbgrp_one,  L, category$qb_threshold)
    def_teams <- rec_pool("def", category$def_func, defgrp_one, L, category$def_threshold)
    gl <- rec_game_level(qb_teams, def_teams, mode,
                         tgt_cluster_input, rte_cluster_input, align_cluster_input,
                         position_group_input, pos_rank_vec, team_rank_vec,
                         man_zone_grp_input, man_z_vec_input, man_z_na,
                         xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input)
    if (mode == "rec") rec_plain_summary(gl) else rec_bucket_summary(gl, mode)
  })
  names(results) <- names(categories)
  
  stem      <- paste0(c(rec = "Rec - ", weather = "WeatherRec - ", precip = "PrecipRec - ")[[mode]], player_name)
  full_name <- paste0(stem, " - ", qbgrp_one, " vs ", defgrp_one)
  
  if (write_s3) {
    wb <- openxlsx::createWorkbook()
    if (mode == "rec") {
      sheet_name <- substr(stem, 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      start_rows <- c(1, 5, 9, 13, 17)
      for (i in seq_along(results))
        openxlsx::writeData(wb, sheet = sheet_name, x = data.frame(results[[i]]), startRow = start_rows[i])
    } else {
      for (nm in names(results)) {
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, sheet = nm, x = data.frame(results[[nm]]))
      }
    }
    tmp <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
    aws.s3::put_object(file = tmp, object = paste0("outputs/", full_name, ".xlsx"), bucket = REC_BUCKET)
    cat("saved s3://", REC_BUCKET, "/outputs/", full_name, ".xlsx\n", sep = "")
  }
  invisible(results)
}

rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
                     position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
                     xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
                     man_z_na = FALSE, write_s3 = TRUE) {
  rec_worker("rec", qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
             position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
             xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
             man_z_na, write_s3)
}

weather_rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
                             position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
                             xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
                             man_z_na = FALSE, write_s3 = TRUE) {
  rec_worker("weather", qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
             position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
             xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
             man_z_na, write_s3)
}

precip_rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
                            position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
                            xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
                            man_z_na = FALSE, write_s3 = TRUE) {
  rec_worker("precip", qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input,
             position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input,
             xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name,
             man_z_na, write_s3)
}

# ---------------------------------------------------------------------------
# 4. THE RUNNER. A spec is a profile; the matchup comes in beside it.
#    rec_specs entries carry rec_func's argument names (rec_specs_from_df
#    builds them that way), so a spec plus a (qbgrp, defgrp) IS the call.
# ---------------------------------------------------------------------------
rec_run <- function(player_name, qbgrp, defgrp, mode = c("rec", "weather", "precip"),
                    specs, write_s3 = TRUE) {
  mode <- match.arg(mode)
  sp <- specs[[player_name]]
  if (is.null(sp)) stop("no spec named '", player_name, "' -- names are: ", paste(names(specs), collapse = ", "))
  args <- sp[intersect(names(sp), names(formals(rec_worker)))]
  do.call(rec_worker, c(list(mode = mode, qbgrp_one = qbgrp, defgrp_one = defgrp,
                             player_name = player_name, write_s3 = write_s3), args))
}

# every receiver x every matchup x every mode. Order is matchup-major so
# the S3 listing groups by "NEMaye-2025 vs SEA2025" first.
rec_push_all <- function(specs, matchups, modes = c("rec", "weather", "precip"), write_s3 = TRUE) {
  for (k in unique(c(matchups$qbgrp)))  stats_tol_or_stop(rec_tol_qb,  k, "QB")
  for (k in unique(c(matchups$defgrp))) stats_tol_or_stop(rec_tol_def, k, "defense")
  n <- length(specs) * nrow(matchups) * length(modes)
  cat("tolerances: ", REC_TOL_STAMP, "\n", sep = "")
  cat("pushing ", n, " workbooks (", length(specs), " receivers x ", nrow(matchups), " matchups x ",
      length(modes), " modes) to s3://", REC_BUCKET, "/outputs/\n\n", sep = "")
  out <- list(); i <- 0
  for (m in seq_len(nrow(matchups))) {
    qb <- matchups$qbgrp[m]; def <- matchups$defgrp[m]
    for (md in modes) for (nm in names(specs)) {
      i <- i + 1
      cat("[", i, "/", n, "] ", nm, " (", md, ") ", qb, " vs ", def, "\n", sep = "")
      out[[paste(nm, qb, def, md, sep = " | ")]] <- rec_run(nm, qb, def, mode = md, specs = specs, write_s3 = write_s3)
    }
  }
  invisible(out)
}

# ---------------------------------------------------------------------------
# usage -- nothing above fires except the banked table block
# ---------------------------------------------------------------------------
cat("
rec_comparison_engine loaded -- tolerances: ", REC_TOL_STAMP, "
  rec_tune(rec_specs_sea, rec_matchups_sea, xtds_tol_qb,  xtds_tol_def)    # SEA offense (tuner tail tables)
  rec_tune(rec_specs_ne,  rec_matchups_ne,  stats_tol_qb, stats_tol_def)   # NE offense (engine's banked 3 x 2)
                                                #   each run MERGES its entities into rec_tol_qb / rec_tol_def
  r <- rec_run('AJ Brown', 'NEMaye-2025', 'SEA2025', 'rec', rec_specs_ne, write_s3 = FALSE); r$pa
  rec_run('AJ Brown', 'NEMaye-2025', 'SEA2025', 'weather', rec_specs_ne)   # one workbook
  rec_push_all(rec_specs_ne, rec_matchups_ne)   # 10 x 6 x 3 = 180 workbooks -> S3
  rec_push_all(rec_specs_ne, rec_matchups_ne, modes = 'rec')               # plain only, 60
  rec_push_all(rec_specs_ne, rec_matchups_ne[rec_matchups_ne$defgrp == 'SEA2025', ])   # one defense, 90
  rec_tol_qb; rec_tol_def                       # what the workers read right now
Check: r$blitz$players for ind == 'In' should equal rec_sweep's in_final for that receiver x matchup x lens.
", sep = "")



rec_tune(rec_specs_ne, rec_matchups_ne, stats_tol_qb, stats_tol_def)
rec_tol_qb    # now 4 QBs: SEADarnold, NEMaye, TENTannehill, DALPrescott
rec_tol_def   # now 3 DEFs: NE2025, SEA2025, SEA2024


rec_push_all(rec_specs_ne, rec_matchups_ne)
