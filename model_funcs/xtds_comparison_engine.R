# xtds_comparison_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# THE SECOND FILE. comparison_engine_thresholds.R tunes; this file runs.
# It reaches into the session the tuner left behind and builds its
# tolerance tables from the tuner's own result objects -- no paste step.
#
# RUN ORDER:
#   source("comparison_engine_thresholds.R")   # tune until you like it
#   source("xtds_comparison_engine.R")         # pulls the tables, fires nothing
#   xtds_push_all()                            # 3 QB x 2 DEF x 3 modes = 18 workbooks
#
# WHERE THE TABLES COME FROM, in order:
#   1. the tuner's session objects (final_qb / final_def / qbs / defs /
#      lenses) if comparison_engine_thresholds.R ran in this session
#   2. otherwise the banked block in section 3 -- the last result you
#      stamped -- so production still runs in a fresh session
# XTDS_TOL_STAMP records which, and prints in the push banner. Re-tune,
# re-source this file, and the new solution is what runs.
#
# Session needs: qb_stats_df_final, dplyr, tidyr, openxlsx, aws.s3.
# No library() calls by law -- every package call is namespaced.
# Weather columns carry NO NAs (interpolated upstream) -- no NA handling
# anywhere by law.

# ---------------------------------------------------------------------------
# 0. SWITCHES
# ---------------------------------------------------------------------------
XTDS_BUCKET     <- "nfl-pff-data-lucas"
XTDS_LOAD_DEPS  <- TRUE    # pull the ten comparison lookups from S3 if absent

XTDS_QBS  <- c("NEMaye-2025", "TENTannehill-2019", "DALPrescott-2025")
XTDS_DEFS <- c("SEA2025", "SEA2024")

if (!exists("%ni%")) `%ni%` <- Negate(`%in%`)   # fallback only; upstream wins

if (!exists("qb_stats_df_final"))
  stop("qb_stats_df_final is not in the session -- load it before sourcing this file")

# ---------------------------------------------------------------------------
# 1. LOADER -- the ten comparison_*_func lookups and their dependencies.
#    Guarded: only fires when the functions are absent, so re-sourcing this
#    file mid-session costs nothing. The .rds files are RData-format saves,
#    which is why they take load() and not readRDS() -- unchanged from the
#    original loader on purpose.
# ---------------------------------------------------------------------------
if (XTDS_LOAD_DEPS && !exists("comparison_blitz_func")) {
  for (k in c("blitz", "depth", "less", "pa", "pressure")) {
    tmp <- tempfile()
    aws.s3::save_object(paste0("qbgrp_def_functions/comparison_", k, "_func.rds"),
                        bucket = XTDS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("qbgrp_def_functions/list_dependencies_", k, ".RData"),
                        bucket = XTDS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("def_functions/comparison_", k, "_def_func.rds"),
                        bucket = XTDS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("def_functions/list_dependencies_", k, "_def.RData"),
                        bucket = XTDS_BUCKET, file = tmp); load(tmp)
  }
  for (k in c("blitz", "depth", "less", "pa", "pressure")) {
    assign(paste0("importance_matrix_", k),
           get(paste0("list_dependencies_", k))[[2]], envir = globalenv())
    assign(paste0("df_", k, "_scaled_z"),
           get(paste0("list_dependencies_", k))[[1]], envir = globalenv())
    assign(paste0("importance_matrix_", k, "_def"),
           get(paste0("list_dependencies_", k, "_def"))[[2]], envir = globalenv())
    assign(paste0("df_", k, "_def_scaled_z"),
           get(paste0("list_dependencies_", k, "_def"))[[1]], envir = globalenv())
  }
  cat("loaded the ten comparison lookups from s3://", XTDS_BUCKET, "\n", sep = "")
}

# ---------------------------------------------------------------------------
# 2. CATEGORY SPEC -- the five lenses, defined ONCE. One column set per
#    lens, used by all three modes: the reduced set (no _rank_def columns,
#    ypc / ypc_rank appended). The lens list, the lookup pairing, and the
#    columns all live here, so tuner and engine can never disagree about
#    what a lens is.
# ---------------------------------------------------------------------------
xtds_core_cols <- c("tds", "fgs", "pbp_xtds", "part_xtds",
                    "pbp_xtds_rank", "part_xtds_rank",
                    "ypa", "pbp_xypa", "part_xypa",
                    "ypa_rank", "pbp_xypa_rank", "part_xypa_rank")

xtds_categories <- list(
  blitz = list(
    qb_func  = function(...) comparison_blitz_func(...),
    def_func = function(...) comparison_blitz_def_func(...),
    columns = c(xtds_core_cols,
                "ypa_rank_def",
                "pressure_rate", "pressure_rate_rank",
                "blitz_rate", "blitz_rate_rank",
                "blitz_grade", "blitz_gr_rank",
                "no_blitz_grade", "no_blitz_gr_rank",
                "blitz_qbr", "blitz_qbr_rank",
                "no_blitz_qbr", "no_blitz_qbr_rank",
                "sack_rate", "sack_rate_rank",
                "ypc", "ypc_rank")),
  depth = list(
    qb_func  = function(...) comparison_depth_func(...),
    def_func = function(...) comparison_depth_def_func(...),
    columns = c(xtds_core_cols,
                "behind_los_rate", "behind_los_rate_rank",
                "behind_los_grade", "behind_los_gr_rank",
                "behind_los_qbr", "behind_los_qbr_rank",
                "short_rate", "short_rate_rank",
                "short_grade", "short_gr_rank",
                "short_qbr", "short_qbr_rank",
                "medium_rate", "medium_rate_rank",
                "medium_grade", "medium_gr_rank",
                "medium_qbr", "medium_qbr_rank",
                "deep_rate", "deep_rate_rank",
                "deep_grade", "deep_gr_rank",
                "deep_qbr", "deep_qbr_rank",
                "ypc", "ypc_rank")),
  less = list(
    qb_func  = function(...) comparison_less_func(...),
    def_func = function(...) comparison_less_def_func(...),
    columns = c(xtds_core_cols,
                "less_rate", "less_rate_rank",
                "less_grade", "less_gr_rank",
                "more_grade", "more_gr_rank",
                "less_qbr", "less_qbr_rank",
                "more_qbr", "more_qbr_rank",
                "ypc", "ypc_rank")),
  pa = list(
    qb_func  = function(...) comparison_pa_func(...),
    def_func = function(...) comparison_pa_def_func(...),
    columns = c(xtds_core_cols,
                "pressure_rate", "pressure_rate_rank",
                "pa_rate", "pa_rate_rank",
                "pa_grade", "pa_gr_rank",
                "npa_grade", "npa_gr_rank",
                "pa_qbr", "pa_qbr_rank",
                "npa_qbr", "npa_qbr_rank",
                "sack_rate", "sack_rate_rank",
                "ypc", "ypc_rank")),
  pressure = list(
    qb_func  = function(...) comparison_pressure_func(...),
    def_func = function(...) comparison_pressure_def_func(...),
    columns = c(xtds_core_cols,
                "acc_rate", "acc_rate_rank",
                "pressure_rate", "pressure_rate_rank",
                "pressure_grade", "pressure_gr_rank",
                "no_pressure_grade", "no_pressure_gr_rank",
                "pressure_qbr", "pressure_qbr_rank",
                "no_pressure_qbr", "no_pressure_qbr_rank",
                "sack_rate", "sack_rate_rank",
                "ypc", "ypc_rank"))
)

# wall: every column a lens asks for must exist NOW, not on workbook 11 of 18
xtds_missing <- setdiff(unique(unlist(lapply(xtds_categories, `[[`, "columns"))),
                        names(qb_stats_df_final))
if (length(xtds_missing))
  stop("qb_stats_df_final is missing columns the lenses ask for: ",
       paste(xtds_missing, collapse = ", "))

# ---------------------------------------------------------------------------
# 3. TOLERANCE TABLES -- pulled from the tuner's session objects when they
#    exist, otherwise the banked block below. The banked block is the last
#    result you stamped; overwrite it when a new tune becomes the standard.
# ---------------------------------------------------------------------------
xtds_tables_from_tuner <- function() {
  # the tuner leaves final_qb[[lens]][qb index] / final_def[[lens]][def index]
  # plus the qbs / defs / lenses vectors it solved over
  if (!identical(sort(lenses), sort(names(xtds_categories))))
    stop("tuner lenses (", paste(lenses, collapse = ", "), ") do not match the ",
         "engine's (", paste(names(xtds_categories), collapse = ", "), ")")
  bad <- lenses[vapply(lenses, function(L)
    anyNA(final_qb[[L]]) || anyNA(final_def[[L]]), logical(1))]
  if (length(bad))
    stop("tuner missed the floor in: ", paste(bad, collapse = ", "),
         " -- not pulling a table with NA thresholds. Rule on those lenses first.")
  tq <- lapply(seq_along(qbs), function(a)
    stats::setNames(vapply(lenses, function(L) final_qb[[L]][a], numeric(1)), lenses))
  td <- lapply(seq_along(defs), function(b)
    stats::setNames(vapply(lenses, function(L) final_def[[L]][b], numeric(1)), lenses))
  names(tq) <- qbs; names(td) <- defs
  list(qb = tq, def = td)
}

if (exists("final_qb") && exists("final_def") && exists("qbs") &&
    exists("defs") && exists("lenses")) {
  xtds_pulled  <- xtds_tables_from_tuner()
  xtds_tol_qb  <- xtds_pulled$qb
  xtds_tol_def <- xtds_pulled$def
  XTDS_TOL_STAMP <- paste0("FROM TUNER SESSION (T_INT ", T_INT, " / T_FLOOR ", T_FLOOR,
                           " / anchor ", T_ANCHOR, " / one-sided ", ONE_SIDED, ")")
} else {
  xtds_tol_qb <- list(
    "NEMaye-2025"       = c(blitz = 0.995, depth = 0.960, less = 0.990, pa = 1.020, pressure = 1.035),
    "TENTannehill-2019" = c(blitz = 1.090, depth = 1.100, less = 0.995, pa = 0.980, pressure = 1.115),
    "DALPrescott-2025"  = c(blitz = 0.970, depth = 0.965, less = 0.940, pa = 0.940, pressure = 0.940)
  )
  xtds_tol_def <- list(
    "SEA2025" = c(blitz = 0.995, depth = 1.110, less = 1.020, pa = 1.035, pressure = 1.005),
    "SEA2024" = c(blitz = 0.965, depth = 1.085, less = 0.955, pa = 1.005, pressure = 1.010)
  )
  XTDS_TOL_STAMP <- "BANKED (no tuner objects in session)"
}

xtds_tol_or_stop <- function(tab, key, side) {
  if (is.null(tab[[key]]))
    stop("no tuned tolerances for ", side, " '", key,
         "' -- add its row to the tolerance table first")
  tab[[key]]
}

# ---------------------------------------------------------------------------
# 4. THE ENGINE
# ---------------------------------------------------------------------------
xtds_comp_pool <- function(qbgrp_one, defgrp_one, category, tol_qb, tol_def) {
  spec <- xtds_categories[[category]]
  list(qb_teams  = c(spec$qb_func(qbgrp_one,   tol_qb[[category]])$QB,  qbgrp_one),
       def_teams = c(spec$def_func(defgrp_one, tol_def[[category]])$QB, defgrp_one))
}

xtds_plain_summary <- function(pool, selected_columns) {
  one <- function(d) d %>%
    dplyr::summarise(n = dplyr::n(),
                     dplyr::across(dplyr::all_of(selected_columns),
                                   ~ mean(.x, na.rm = TRUE)))
  rbind(
    one(qb_stats_df_final %>%
          dplyr::filter(qbgrp_ssn %in% pool$qb_teams & def_ssn %in% pool$def_teams)),
    one(qb_stats_df_final %>%
          dplyr::filter(qbgrp_ssn %in% pool$qb_teams & def_ssn %ni% pool$def_teams)),
    one(qb_stats_df_final %>%
          dplyr::filter(qbgrp_ssn %ni% pool$qb_teams & def_ssn %in% pool$def_teams)))
}

# bucket labels, the 'All' baseline label, the short names used in the diff
# columns, and (precip only) the reference bucket the others also compare to.
# Column names and order below reproduce the old weather/precip workbooks
# exactly -- the Excel side of this is already built against them.
xtds_wx_spec <- list(
  weather = list(labs  = c("1_Both", "2_Wind", "3_Cold", "4_Good"),
                 short = c("Both", "Wind", "Cold", "Good"),
                 all   = "5_All", ref = NA_character_),
  precip  = list(labs  = c("1_Snow", "2_Rain", "3_Clear"),
                 short = c("Snow", "Rain", "Clear"),
                 all   = "4_All", ref = "3_Clear")
)

xtds_bucket_summary <- function(pool, selected_columns, mode) {
  spec <- xtds_wx_spec[[mode]]
  
  df <- qb_stats_df_final %>%
    dplyr::mutate(
      wx = if (mode == "weather")
        dplyr::case_when(wind >= 10 & temp <= 50 ~ "1_Both",
                         wind >= 10 & temp >  50 ~ "2_Wind",
                         wind <  10 & temp <= 50 ~ "3_Cold",
                         TRUE                    ~ "4_Good")
      else
        dplyr::case_when(snow_ind == 1 ~ "1_Snow",
                         rain_ind == 1 ~ "2_Rain",
                         TRUE          ~ "3_Clear"),
      comp_bucket = dplyr::case_when(
        qbgrp_ssn %in% pool$qb_teams & def_ssn %in% pool$def_teams ~ "1_Both",
        qbgrp_ssn %in% pool$qb_teams & def_ssn %ni% pool$def_teams ~ "2_Off_only",
        qbgrp_ssn %ni% pool$qb_teams & def_ssn %in% pool$def_teams ~ "3_Def_only",
        TRUE ~ "Neither")) %>%
    dplyr::filter(comp_bucket != "Neither") %>%
    tidyr::pivot_longer(dplyr::all_of(selected_columns),
                        names_to = "metric", values_to = "value")
  
  out <- df %>%
    dplyr::group_by(comp_bucket, metric) %>%
    dplyr::summarise(
      n_all      = dplyr::n(),
      mean_all   = mean(value, na.rm = TRUE),
      overall_sd = stats::sd(value, na.rm = TRUE),
      anova_pval = tryCatch(summary(stats::aov(value ~ wx))[[1]][["Pr(>F)"]][1],
                            error = function(e) NA_real_),
      eta_sq     = tryCatch({
        ss <- summary(stats::aov(value ~ wx))[[1]][["Sum Sq"]]
        ss[1] / sum(ss)
      }, error = function(e) NA_real_),
      .groups = "drop")
  
  for (lb in spec$labs) {
    agg <- df %>%
      dplyr::filter(wx == lb) %>%
      dplyr::group_by(comp_bucket, metric) %>%
      dplyr::summarise(n_lb = dplyr::n(), mean_lb = mean(value, na.rm = TRUE),
                       .groups = "drop")
    names(agg)[3:4] <- c(paste0("n_", lb), paste0("mean_", lb))
    out <- dplyr::left_join(out, agg, by = c("comp_bucket", "metric"))
    out[[paste0("n_", lb)]][is.na(out[[paste0("n_", lb)]])] <- 0
  }
  
  for (k in seq_along(spec$labs)) {
    m <- out[[paste0("mean_", spec$labs[k])]]
    out[[paste0("diff_", spec$short[k], "_vs_All")]] <- m - out$mean_all
    out[[paste0("d_",    spec$short[k], "_vs_All")]] <- (m - out$mean_all) / out$overall_sd
  }
  
  ord_ref <- character(0)
  if (!is.na(spec$ref)) {
    r  <- match(spec$ref, spec$labs)
    mr <- out[[paste0("mean_", spec$ref)]]
    for (k in setdiff(seq_along(spec$labs), r)) {
      m <- out[[paste0("mean_", spec$labs[k])]]
      out[[paste0("diff_", spec$short[k], "_vs_", spec$short[r])]] <- m - mr
      out[[paste0("d_",    spec$short[k], "_vs_", spec$short[r])]] <- (m - mr) / out$overall_sd
    }
    ord_ref <- c(paste0("diff_", spec$short[-r], "_vs_", spec$short[r]),
                 paste0("d_",    spec$short[-r], "_vs_", spec$short[r]))
  }
  
  out$anova_sig <- dplyr::case_when(out$anova_pval < 0.01 ~ "***",
                                    out$anova_pval < 0.05 ~ "**",
                                    out$anova_pval < 0.10 ~ "*",
                                    TRUE ~ "")
  out$effect_size <- dplyr::case_when(out$eta_sq >= 0.14 ~ "large",
                                      out$eta_sq >= 0.06 ~ "medium",
                                      out$eta_sq >= 0.01 ~ "small",
                                      TRUE ~ "negligible")
  
  names(out)[names(out) == "n_all"]    <- paste0("n_", spec$all)
  names(out)[names(out) == "mean_all"] <- paste0("mean_", spec$all)
  
  out <- out[, c("comp_bucket", "metric",
                 paste0("n_",    c(spec$labs, spec$all)),
                 paste0("mean_", c(spec$labs, spec$all)),
                 "overall_sd", "anova_pval", "eta_sq",
                 paste0("diff_", spec$short, "_vs_All"),
                 paste0("d_",    spec$short, "_vs_All"),
                 ord_ref, "anova_sig", "effect_size")]
  dplyr::arrange(out, comp_bucket, metric)
}

# ---------------------------------------------------------------------------
# 5. PUBLIC API -- one runner, three modes. Results come back invisibly, so
#    a test run needs no S3 round trip: read the list, skip the workbook.
# ---------------------------------------------------------------------------
xtds_run <- function(qbgrp_one, defgrp_one,
                     mode = c("xtds", "weather", "precip"), write_s3 = TRUE) {
  mode    <- match.arg(mode)
  tol_qb  <- xtds_tol_or_stop(xtds_tol_qb,  qbgrp_one,  "QB")
  tol_def <- xtds_tol_or_stop(xtds_tol_def, defgrp_one, "defense")
  
  results <- lapply(names(xtds_categories), function(category) {
    pool <- xtds_comp_pool(qbgrp_one, defgrp_one, category, tol_qb, tol_def)
    cols <- xtds_categories[[category]]$columns
    if (mode == "xtds") xtds_plain_summary(pool, cols)
    else                xtds_bucket_summary(pool, cols, mode)
  })
  names(results) <- names(xtds_categories)
  
  full_name <- paste0(switch(mode, xtds = "TDs - ", weather = "Weather TDs - ",
                             precip = "Precip TDs - "),
                      qbgrp_one, " vs ", defgrp_one)
  
  if (write_s3) {
    wb <- openxlsx::createWorkbook()
    if (mode == "xtds") {
      sheet_name <- substr(full_name, 1, 31)   # Excel's sheet-name limit
      openxlsx::addWorksheet(wb, sheet_name)
      start_rows <- c(1, 6, 11, 16, 21)
      for (i in seq_along(results))
        openxlsx::writeData(wb, sheet = sheet_name,
                            x = data.frame(results[[i]]), startRow = start_rows[i])
    } else {
      for (nm in names(results)) {
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, sheet = nm, x = data.frame(results[[nm]]))
      }
    }
    tmp <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, tmp)
    aws.s3::put_object(tmp, bucket = XTDS_BUCKET,
                       object = paste0("outputs/", full_name, ".xlsx"))
    cat("saved s3://", XTDS_BUCKET, "/outputs/", full_name, ".xlsx\n", sep = "")
  }
  invisible(results)
}

xtds_run_grid <- function(qbs = XTDS_QBS, defs = XTDS_DEFS,
                          mode = "xtds", write_s3 = TRUE) {
  out <- list()
  for (q in qbs) for (d in defs) {
    cat("== ", q, " vs ", d, " (", mode, ") ==\n", sep = "")
    out[[paste(q, d, sep = " vs ")]] <- xtds_run(q, d, mode, write_s3)
  }
  invisible(out)
}

xtds_push_all <- function(qbs = XTDS_QBS, defs = XTDS_DEFS, write_s3 = TRUE) {
  cat("tolerances: ", XTDS_TOL_STAMP, "\n", sep = "")
  cat("pushing ", length(qbs) * length(defs) * 3, " workbooks to s3://",
      XTDS_BUCKET, "/outputs/\n\n", sep = "")
  out <- list()
  for (m in c("xtds", "weather", "precip"))
    out[[m]] <- xtds_run_grid(qbs, defs, mode = m, write_s3 = write_s3)
  invisible(out)
}

# ---------------------------------------------------------------------------
# usage -- nothing above this line fires anything
# ---------------------------------------------------------------------------
cat("
xtds_comparison_engine loaded. Tolerances: ", XTDS_TOL_STAMP, "

  xtds_push_all()                                   all 18 workbooks
  xtds_run_grid(mode = 'weather')                   one mode, six combos
  r <- xtds_run('NEMaye-2025', 'SEA2025', write_s3 = FALSE); r$pressure

Re-tune: source comparison_engine_thresholds.R again, then re-source this
file -- it rebuilds its tables from whatever the tuner just solved.
To make a tune the standard for fresh sessions, paste the tuner's printed
block over the banked lists in section 3.

New entity: add its name to XTDS_QBS / XTDS_DEFS and to the tuner's qbs /
defs, re-tune, re-source. Nothing else changes.
", sep = "")

xtds_run_grid()