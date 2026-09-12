# rush_comparison_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# The rushing twin of rec_comparison_engine.R. Replaces the hardcoded
# tolerance block in rush_func_AWS.R (lines 172-178: blitz 1.02/1.00,
# depth 1.01/1.07, less .99/.94, pa 1.07/1.32, pressure 1.02/.98) with a
# table pair, rush_tol_qb / rush_tol_def, filled from rush_sweep. rush_func
# keeps its name, its argument list, and its output columns; only where
# the thresholds come from changed. rush_push_all runs every rusher in a
# spec list against every matchup. rush_func has ONE mode (no weather /
# precip variants exist in the canon), so the runner has one mode.
#
# Source order:
#   source("stats_comparison_engine.R")      # lookups, stats_categories, stats_tol_or_stop
#   source("rush_thresholds_engine.R")       # rush_template_from_paste -> rush_specs_from_df, rush_sweep
#   source("rush_comparison_engine.R")       # this file
#   rush_tune(rush_specs_ne, rush_matchups_ne, stats_tol_qb, stats_tol_def)
#   rush_push_all(rush_specs_ne, rush_matchups_ne)
#
# Session needs: rush_stats_final (build six), the ten comparison_*_func
# lookups, dplyr, openxlsx, aws.s3.
#
# Departures from rush_func_AWS.R, both PROPOSED:
#   1. S3 object names carry the matchup: "Rush - Rhamondre Stevenson -
#      NEMaye-2025 vs SEA2025.xlsx" (the canon name overwrites itself
#      across matchups). Sheet name stays the canon 31-char stem.
#   2. write_s3 = FALSE returns the five lens results, no workbook.

RUSH_BUCKET <- "nfl-pff-data-lucas"

# ---------------------------------------------------------------------------
# 1. TOLERANCE TABLES. rush_tune() fills these; nothing is banked yet.
# ---------------------------------------------------------------------------
if (!exists("rush_tol_qb"))  rush_tol_qb  <- list()
if (!exists("rush_tol_def")) rush_tol_def <- list()
if (!exists("RUSH_TOL_STAMP")) RUSH_TOL_STAMP <- "EMPTY -- run rush_tune()"

rush_tables_from_sweep <- function(sw) {
  unreach <- unique(sw$rec[sw$n_profile < sw$floor])
  ok  <- sw[!sw$rec %in% unreach, ]
  bad <- ok[!ok$cleared, ]
  if (nrow(bad)) stop("rush_sweep hit the cap without clearing: ", paste(unique(paste(bad$rec, bad$lens)), collapse = ", "),
                      " -- not handing over a table with the cap in it. Rule on those cells first.")
  lenses <- names(stats_categories)
  one_side <- function(key, base_col) {
    parts <- split(ok, ok[[key]])
    lapply(parts, function(x) stats::setNames(vapply(lenses, function(L) {
      y <- x[x$lens == L, ]; if (!nrow(y)) stop("no sweep rows for ", key, " ", x[[key]][1], " lens ", L)
      y[[base_col]][1] + max(y$delta) }, numeric(1)), lenses))
  }
  list(qb = one_side("qbgrp", "qb_base"), def = one_side("defgrp", "def_base"), unreach = unreach)
}

rush_tune <- function(specs, matchups, tol_qb, tol_def, deltas = seq(0, 0.35, by = 0.005)) {
  cat("base tables cover QB: ", paste(names(tol_qb), collapse = ", "), " | DEF: ", paste(names(tol_def), collapse = ", "), "\n", sep = "")
  sw <- rush_sweep(specs, matchups, tol_qb = tol_qb, tol_def = tol_def, deltas = deltas, side = "both")
  tb <- rush_tables_from_sweep(sw)
  cur_qb  <- if (exists("rush_tol_qb",  envir = globalenv())) get("rush_tol_qb",  envir = globalenv()) else list()
  cur_def <- if (exists("rush_tol_def", envir = globalenv())) get("rush_tol_def", envir = globalenv()) else list()
  cur_qb[names(tb$qb)] <- tb$qb; cur_def[names(tb$def)] <- tb$def
  assign("rush_tol_qb",  cur_qb,  envir = globalenv())
  assign("rush_tol_def", cur_def, envir = globalenv())
  run <- paste0("rush_sweep ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (symmetric, ", length(specs), " rushers x ", nrow(matchups),
                " matchups: ", paste(unique(matchups$qbgrp), collapse = "/"), " vs ", paste(unique(matchups$defgrp), collapse = "/"),
                if (length(tb$unreach)) paste0("; EXCLUDED ", paste(tb$unreach, collapse = ", ")) else "", ")")
  prev <- if (exists("RUSH_TOL_STAMP", envir = globalenv())) get("RUSH_TOL_STAMP", envir = globalenv()) else ""
  assign("RUSH_TOL_STAMP", if (nzchar(prev) && !grepl("^EMPTY", prev)) paste(prev, run, sep = " | ") else run, envir = globalenv())
  cat("\nrush_tol_qb now covers: ", paste(names(cur_qb), collapse = ", "), "\nrush_tol_def now covers: ", paste(names(cur_def), collapse = ", "),
      "\nRUSH_TOL_STAMP: ", get("RUSH_TOL_STAMP", envir = globalenv()), "\n", sep = "")
  invisible(sw)
}

rush_categories <- function(qbgrp_one, defgrp_one) {
  tq <- stats_tol_or_stop(rush_tol_qb,  qbgrp_one,  "QB")
  td <- stats_tol_or_stop(rush_tol_def, defgrp_one, "defense")
  lenses <- names(stats_categories)
  miss <- c(setdiff(lenses, names(tq)), setdiff(lenses, names(td)))
  if (length(miss)) stop("rush_tol tables are missing lens: ", paste(unique(miss), collapse = ", "))
  lapply(stats::setNames(lenses, lenses), function(L)
    list(qb_threshold = tq[[L]], def_threshold = td[[L]], qb_func = stats_categories[[L]]$qb_func, def_func = stats_categories[[L]]$def_func))
}

# ---------------------------------------------------------------------------
# 2. rush_func -- rush_func_AWS.R lines 165-261, the categories block swapped
#    for rush_categories(), plus write_s3. Chain and summarise verbatim.
# ---------------------------------------------------------------------------
rush_pool_cache <- new.env()
rush_pool <- function(which, f, entity, L, tol) {
  key <- paste(which, entity, L, format(tol, nsmall = 3), sep = "|")
  if (is.null(rush_pool_cache[[key]])) rush_pool_cache[[key]] <- c(f(entity, tol)$QB, entity)
  rush_pool_cache[[key]]
}

rush_func <- function(qbgrp_one, defgrp_one, rank_grp_input, situation_input,
                      gap_input, gap_z_range, gap_z_NA = FALSE,
                      xtd_vec_input, td_na = FALSE,
                      position_group_input, player_name, write_s3 = TRUE) {
  categories <- rush_categories(qbgrp_one, defgrp_one)
  process_category <- function(category, L) {
    qb_teams  <- rush_pool("qb",  category$qb_func,  qbgrp_one,  L, category$qb_threshold)
    def_teams <- rush_pool("def", category$def_func, defgrp_one, L, category$def_threshold)
    rush_stats_final %>%
      dplyr::ungroup() %>%
      dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
      dplyr::mutate(
        ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
        rush_ind = ifelse(
          rank_grp %in% rank_grp_input &
            position_group %in% position_group_input &
            situation_cluster %in% situation_input &
            gap_cluster %in% gap_input &
            ((gap_z >= gap_z_range[1] & gap_z <= gap_z_range[2]) | (gap_z_NA & is.na(gap_z))) &
            ((xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) | (td_na & is.na(xtd_percentile))),
          "In", "Out")) %>%
      dplyr::mutate(has_rel_rushers = any(rush_ind == "In")) %>%
      dplyr::filter(has_rel_rushers) %>%
      dplyr::group_by(qbgrp_ssn, def_ssn, week, season, ind) %>%
      dplyr::filter(sum(ifelse(rush_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
      dplyr::summarise(
        rel_players = sum(ifelse(rush_ind == "In", 1, 0), na.rm = TRUE),
        notrel_players = sum(ifelse(rush_ind != "In", 1, 0), na.rm = TRUE),
        rel_rushes = sum(ifelse(rush_ind == "In", attempts, 0), na.rm = TRUE),
        notrel_rushes = sum(ifelse(rush_ind != "In", attempts, 0), na.rm = TRUE),
        rel_pbp_xtds = sum(ifelse(rush_ind == "In", pbp_xtd, 0), na.rm = TRUE),
        notrel_pbp_xtds = sum(ifelse(rush_ind != "In", pbp_xtd, 0), na.rm = TRUE),
        rel_part_xtds = sum(ifelse(rush_ind == "In", part_xtd, 0), na.rm = TRUE),
        notrel_part_xtds = sum(ifelse(rush_ind != "In", part_xtd, 0), na.rm = TRUE),
        rel_ypc = mean(ifelse(rush_ind == "In", ypc, NA), na.rm = TRUE),
        rel_pbp_ypc = mean(ifelse(rush_ind == "In", pbp_xypc, NA), na.rm = TRUE),
        rel_part_ypc = mean(ifelse(rush_ind == "In", part_xypc, NA), na.rm = TRUE),
        .groups = "drop") %>%
      dplyr::mutate(
        rush_shr = (rel_rushes / (rel_rushes + notrel_rushes)) / rel_players,
        pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
        part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ind) %>%
      dplyr::summarise(
        players = dplyr::n(),
        rush_shr = mean(rush_shr, na.rm = TRUE),
        pbp_xtd_shr = mean(pbp_xtd_shr, na.rm = TRUE),
        part_xtd_shr = mean(part_xtd_shr, na.rm = TRUE),
        ypc = mean(rel_ypc, na.rm = TRUE),
        pbp_ypc = mean(rel_pbp_ypc, na.rm = TRUE),
        part_ypc = mean(rel_part_ypc, na.rm = TRUE),
        .groups = "drop")
  }
  category_results <- lapply(names(categories), function(L) process_category(categories[[L]], L))
  names(category_results) <- names(categories)
  
  stem <- paste0("Rush - ", player_name)
  full_name <- paste0(stem, " - ", qbgrp_one, " vs ", defgrp_one)
  if (write_s3) {
    wb_tds <- openxlsx::createWorkbook()
    sheet_name <- substr(stem, 1, 31)
    openxlsx::addWorksheet(wb_tds, sheet_name)
    start_rows <- c(1, 5, 9, 13, 17)
    for (i in seq_along(category_results))
      openxlsx::writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
    tmp <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb_tds, tmp, overwrite = TRUE)
    aws.s3::put_object(file = tmp, object = paste0("outputs/", full_name, ".xlsx"), bucket = RUSH_BUCKET)
    cat("saved s3://", RUSH_BUCKET, "/outputs/", full_name, ".xlsx\n", sep = "")
  }
  invisible(category_results)
}

# ---------------------------------------------------------------------------
# 3. THE RUNNER. A spec carries rush_func's argument names; add the matchup.
# ---------------------------------------------------------------------------
rush_run <- function(player_name, qbgrp, defgrp, specs, write_s3 = TRUE) {
  sp <- specs[[player_name]]
  if (is.null(sp)) stop("no spec named '", player_name, "' -- names are: ", paste(names(specs), collapse = ", "))
  args <- sp[intersect(names(sp), names(formals(rush_func)))]
  do.call(rush_func, c(list(qbgrp_one = qbgrp, defgrp_one = defgrp, player_name = player_name, write_s3 = write_s3), args))
}

rush_push_all <- function(specs, matchups, write_s3 = TRUE) {
  for (k in unique(matchups$qbgrp))  stats_tol_or_stop(rush_tol_qb,  k, "QB")
  for (k in unique(matchups$defgrp)) stats_tol_or_stop(rush_tol_def, k, "defense")
  n <- length(specs) * nrow(matchups)
  cat("tolerances: ", RUSH_TOL_STAMP, "\npushing ", n, " workbooks (", length(specs), " rushers x ", nrow(matchups),
      " matchups) to s3://", RUSH_BUCKET, "/outputs/\n\n", sep = "")
  out <- list(); i <- 0
  for (m in seq_len(nrow(matchups))) { qb <- matchups$qbgrp[m]; def <- matchups$defgrp[m]
  for (nm in names(specs)) { i <- i + 1
  cat("[", i, "/", n, "] ", nm, " ", qb, " vs ", def, "\n", sep = "")
  out[[paste(nm, qb, def, sep = " | ")]] <- rush_run(nm, qb, def, specs = specs, write_s3 = write_s3) } }
  invisible(out)
}

cat("
rush_comparison_engine loaded -- tolerances: ", RUSH_TOL_STAMP, "
  rush_tune(rush_specs_ne, rush_matchups_ne, stats_tol_qb, stats_tol_def)     # NE offense (engine's banked 3 x 2)
  rush_tune(rush_specs_sea, rush_matchups_sea, xtds_tol_qb, xtds_tol_def)     # SEA offense (tuner tail tables)
  r <- rush_run('Rhamondre Stevenson', 'NEMaye-2025', 'SEA2025', rush_specs_ne, write_s3 = FALSE); r$pa
  rush_push_all(rush_specs_ne, rush_matchups_ne)                             # rushers x 6 matchups -> S3
Check: r$blitz$players on the In row == rush_sweep's in_final for that rusher x matchup x blitz.
", sep = "")