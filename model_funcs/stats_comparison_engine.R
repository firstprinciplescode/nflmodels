# stats_comparison_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# THE SECOND FILE. comparison_engine_thresholds.R tunes; this file runs.
# It reaches into the session the tuner left behind and builds its
# tolerance tables from the tuner's own result objects -- no paste step.
#
# RUN ORDER:
#   source("comparison_engine_thresholds.R")   # tune until you like it
#   source("stats_comparison_engine.R")         # pulls the tables, fires nothing
#   stats_run_grid(stat = "plays")             # six workbooks of one family
#   stats_push_all()                            # every family x every mode x six combos
#
# WHERE THE TABLES COME FROM, in order:
#   1. the tuner's session objects (final_qb / final_def / qbs / defs /
#      lenses) if comparison_engine_thresholds.R ran in this session
#   2. otherwise the banked block in section 3 -- the last result you
#      stamped -- so production still runs in a fresh session
# STATS_TOL_STAMP records which, and prints in the push banner. Re-tune,
# re-source this file, and the new solution is what runs.
#
# Session needs: qb_stats_df_final (and xtd_proportion for the xtd_prop
# family), dplyr, tidyr, openxlsx, aws.s3.
# No library() calls by law -- every package call is namespaced.
# Weather columns carry NO NAs (interpolated upstream) -- no NA handling
# anywhere by law.

# ---------------------------------------------------------------------------
# 0. SWITCHES
# ---------------------------------------------------------------------------
STATS_BUCKET     <- "nfl-pff-data-lucas"
STATS_LOAD_DEPS  <- TRUE    # pull the ten comparison lookups from S3 if absent

STATS_QBS  <- c("SEADarnold-2025")
STATS_DEFS <- c("NE2025")

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
if (STATS_LOAD_DEPS && !exists("comparison_blitz_func")) {
  for (k in c("blitz", "depth", "less", "pa", "pressure")) {
    tmp <- tempfile()
    aws.s3::save_object(paste0("qbgrp_def_functions/comparison_", k, "_func.rds"),
                        bucket = STATS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("qbgrp_def_functions/list_dependencies_", k, ".RData"),
                        bucket = STATS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("def_functions/comparison_", k, "_def_func.rds"),
                        bucket = STATS_BUCKET, file = tmp); load(tmp)
    tmp <- tempfile()
    aws.s3::save_object(paste0("def_functions/list_dependencies_", k, "_def.RData"),
                        bucket = STATS_BUCKET, file = tmp); load(tmp)
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
  cat("loaded the ten comparison lookups from s3://", STATS_BUCKET, "\n", sep = "")
}

# ---------------------------------------------------------------------------
# 2. CATEGORY SPEC -- the five lenses, defined ONCE. One column set per
#    lens, used by all three modes: the reduced set (no _rank_def columns,
#    ypc / ypc_rank appended). The lens list, the lookup pairing, and the
#    columns all live here, so tuner and engine can never disagree about
#    what a lens is.
# ---------------------------------------------------------------------------
tds_core_cols <- c("tds", "fgs", "pbp_xtds", "part_xtds",
                   "pbp_xtds_rank", "part_xtds_rank",
                   "ypa", "pbp_xypa", "part_xypa",
                   "ypa_rank", "pbp_xypa_rank", "part_xypa_rank")

stats_categories <- list(
  blitz = list(
    qb_func  = function(...) comparison_blitz_func(...),
    def_func = function(...) comparison_blitz_def_func(...),
    columns = c(tds_core_cols,
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
    columns = c(tds_core_cols,
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
    columns = c(tds_core_cols,
                "less_rate", "less_rate_rank",
                "less_grade", "less_gr_rank",
                "more_grade", "more_gr_rank",
                "less_qbr", "less_qbr_rank",
                "more_qbr", "more_qbr_rank",
                "ypc", "ypc_rank")),
  pa = list(
    qb_func  = function(...) comparison_pa_func(...),
    def_func = function(...) comparison_pa_def_func(...),
    columns = c(tds_core_cols,
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
    columns = c(tds_core_cols,
                "acc_rate", "acc_rate_rank",
                "pressure_rate", "pressure_rate_rank",
                "pressure_grade", "pressure_gr_rank",
                "no_pressure_grade", "no_pressure_gr_rank",
                "pressure_qbr", "pressure_qbr_rank",
                "no_pressure_qbr", "no_pressure_qbr_rank",
                "sack_rate", "sack_rate_rank",
                "ypc", "ypc_rank"))
)

# STAT FAMILIES -- what gets averaged inside each comp pool. tds carries a
# different column set per lens (defined above); every other family is one
# uniform set used by all five lenses. Column sets are the reduced sets
# (no _rank_def), lifted from each *_func_AWS file. label is the workbook
# name stem the old function used: "SackRt - NEMaye-2025 vs SEA2025".
# data is the game-level frame the family reads; every family reads
# qb_stats_df_final except xtd_prop, which reads the xtd_proportion frame
# built from combined_pbp. Comp pools are keyed on qbgrp_ssn / def_ssn in
# both frames, so the same lists apply.
stat_families <- list(
  tds      = list(label = "TDs",      data = "qb_stats_df_final", columns = NULL),
  plays    = list(label = "Plays",    data = "qb_stats_df_final",
                  columns = c("plays", "no_huddle", "plays_rank", "no_huddle_rank")),
  xpass    = list(label = "XPass",    data = "qb_stats_df_final",
                  columns = c("pass_rate", "fastr_xpass_rate", "pbp_xpass_rate", "part_xpass_rate",
                              "pass_rate_rank", "fastr_xpass_rate_rank", "pbp_xpass_rate_rank",
                              "part_xpass_rate_rank")),
  cp       = list(label = "CP",       data = "qb_stats_df_final",
                  columns = c("acc_rate", "fastr_cp", "pbp_cp", "part_cp",
                              "acc_rate_rank", "fastr_cp_rank", "pbp_cp_rank", "part_cp_rank")),
  sack     = list(label = "SackRt",   data = "qb_stats_df_final",
                  columns = c("sack_rate", "pbp_sack_rate", "part_sack_rate",
                              "sack_rate_rank", "pbp_sack_rate_rank", "part_sack_rate_rank",
                              "pressure_rate", "pbp_pressure", "part_pressure_before",
                              "pressure_rate_rank", "pbp_pressure_rank", "part_pressure_before_rank")),
  scr      = list(label = "ScrStats", data = "qb_stats_df_final",
                  columns = c("scr_rate", "scr_ypc", "pbp_scr_xypc", "part_scr_xypc",
                              "scr_rate_rank", "scr_ypc_rank", "pbp_scr_xypc_rank", "part_scr_xypc_rank")),
  twp = list(label = "TWP", data = "qb_stats_df_final",
             columns = c("twp_rate", "int_rate",
                         "twp_rate_rank", "int_rate_rank")),
  ypa = list(label = "YPA", data = "qb_stats_df_final",
             columns = c("ypa", "pbp_xypa", "part_xypa",
                         "ypa_rank", "pbp_xypa_rank", "part_xypa_rank")),
  xtd_prop = list(label = "XTD_Prop", data = "xtd_proportion",
                  columns = c("pbp_pass_prop", "part_pass_prop",
                              "pbp_pass_prop_rank", "part_pass_prop_rank",
                              "pbp_qb_scramble_prop", "part_qb_scramble_prop",
                              "pbp_qb_scramble_prop_rank", "part_qb_scramble_prop_rank",
                              "pbp_run_prop", "part_run_prop",
                              "pbp_run_xtd_rank", "part_run_xtd_rank"))
)
stat_family_cols <- function(stat, category) {
  s <- stat_families[[stat]]
  if (is.null(s)) stop("no stat family '", stat, "' -- add it to stat_families")
  if (is.null(s$columns)) stats_categories[[category]]$columns else s$columns
}
stat_family_data <- function(stat) {
  nm <- stat_families[[stat]]$data
  if (!exists(nm)) stop("stat family '", stat, "' reads ", nm,
                        ", which is not in the session -- build it first")
  dplyr::ungroup(get(nm))   # xtd_proportion arrives grouped from its build
}

# wall: every column every family asks for must exist in that family's
# data NOW, not on workbook 11 of 18. A family whose data frame is not in
# the session is reported, not fatal -- it stops at run time instead.
for (st in names(stat_families)) {
  nm   <- stat_families[[st]]$data
  need <- if (is.null(stat_families[[st]]$columns))
    unique(unlist(lapply(stats_categories, `[[`, "columns"))) else stat_families[[st]]$columns
  if (!exists(nm)) { cat("note: ", nm, " not in session -- '", st, "' unavailable until it is\n", sep = ""); next }
  miss <- setdiff(need, names(get(nm)))
  if (length(miss)) stop(nm, " is missing columns '", st, "' asks for: ", paste(miss, collapse = ", "))
}

# ---------------------------------------------------------------------------
# 3. TOLERANCE TABLES -- pulled from the tuner's session objects when they
#    exist, otherwise the banked block below. The banked block is the last
#    result you stamped; overwrite it when a new tune becomes the standard.
# ---------------------------------------------------------------------------
stats_tables_from_tuner <- function() {
  # the tuner leaves final_qb[[lens]][qb index] / final_def[[lens]][def index]
  # plus the qbs / defs / lenses vectors it solved over
  if (!identical(sort(lenses), sort(names(stats_categories))))
    stop("tuner lenses (", paste(lenses, collapse = ", "), ") do not match the ",
         "engine's (", paste(names(stats_categories), collapse = ", "), ")")
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
  stats_pulled  <- stats_tables_from_tuner()
  stats_tol_qb  <- stats_pulled$qb
  stats_tol_def <- stats_pulled$def
  STATS_TOL_STAMP <- paste0("FROM TUNER SESSION (T_INT ", T_INT, " / T_FLOOR ", T_FLOOR,
                            " / anchor ", T_ANCHOR, " / one-sided ", ONE_SIDED, ")")
} else {
  stats_tol_qb <- list(
    "NEMaye-2025"       = c(blitz = 0.995, depth = 0.960, less = 0.990, pa = 1.020, pressure = 1.035),
    "TENTannehill-2019" = c(blitz = 1.090, depth = 1.100, less = 0.995, pa = 0.980, pressure = 1.115),
    "DALPrescott-2025"  = c(blitz = 0.970, depth = 0.965, less = 0.940, pa = 0.940, pressure = 0.940)
  )
  stats_tol_def <- list(
    "SEA2025" = c(blitz = 0.995, depth = 1.110, less = 1.020, pa = 1.035, pressure = 1.005),
    "SEA2024" = c(blitz = 0.965, depth = 1.085, less = 0.955, pa = 1.005, pressure = 1.010)
  )
  STATS_TOL_STAMP <- "BANKED (no tuner objects in session)"
}

stats_tol_or_stop <- function(tab, key, side) {
  if (is.null(tab[[key]]))
    stop("no tuned tolerances for ", side, " '", key,
         "' -- add its row to the tolerance table first")
  tab[[key]]
}

# ---------------------------------------------------------------------------
# 4. THE ENGINE
# ---------------------------------------------------------------------------
stats_comp_pool <- function(qbgrp_one, defgrp_one, category, tol_qb, tol_def) {
  spec <- stats_categories[[category]]
  list(qb_teams  = c(spec$qb_func(qbgrp_one,   tol_qb[[category]])$QB,  qbgrp_one),
       def_teams = c(spec$def_func(defgrp_one, tol_def[[category]])$QB, defgrp_one))
}

stats_plain_summary <- function(pool, selected_columns, df) {
  one <- function(d) d %>%
    dplyr::summarise(n = dplyr::n(),
                     dplyr::across(dplyr::all_of(selected_columns),
                                   ~ mean(.x, na.rm = TRUE)))
  rbind(
    one(df %>% dplyr::filter(qbgrp_ssn %in% pool$qb_teams & def_ssn %in% pool$def_teams)),
    one(df %>% dplyr::filter(qbgrp_ssn %in% pool$qb_teams & def_ssn %ni% pool$def_teams)),
    one(df %>% dplyr::filter(qbgrp_ssn %ni% pool$qb_teams & def_ssn %in% pool$def_teams)))
}

# bucket labels, the 'All' baseline label, the short names used in the diff
# columns, and (precip only) the reference bucket the others also compare to.
# Column names and order below reproduce the old weather/precip workbooks
# exactly -- the Excel side of this is already built against them.
stats_wx_spec <- list(
  weather = list(labs  = c("1_Both", "2_Wind", "3_Cold", "4_Good"),
                 short = c("Both", "Wind", "Cold", "Good"),
                 all   = "5_All", ref = NA_character_),
  precip  = list(labs  = c("1_Snow", "2_Rain", "3_Clear"),
                 short = c("Snow", "Rain", "Clear"),
                 all   = "4_All", ref = "3_Clear")
)

stats_bucket_summary <- function(pool, selected_columns, mode, df) {
  spec <- stats_wx_spec[[mode]]
  
  df <- df %>%
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
stats_run <- function(qbgrp_one, defgrp_one, stat = "tds",
                      mode = c("main", "weather", "precip"), write_s3 = TRUE) {
  mode    <- match.arg(mode)
  tol_qb  <- stats_tol_or_stop(stats_tol_qb,  qbgrp_one,  "QB")
  tol_def <- stats_tol_or_stop(stats_tol_def, defgrp_one, "defense")
  
  df <- stat_family_data(stat)
  results <- lapply(names(stats_categories), function(category) {
    pool <- stats_comp_pool(qbgrp_one, defgrp_one, category, tol_qb, tol_def)
    cols <- stat_family_cols(stat, category)
    if (mode == "main") stats_plain_summary(pool, cols, df)
    else                stats_bucket_summary(pool, cols, mode, df)
  })
  names(results) <- names(stats_categories)
  
  full_name <- paste0(c(main = "", weather = "Weather ", precip = "Precip ")[[mode]],
                      stat_families[[stat]]$label, " - ", qbgrp_one, " vs ", defgrp_one)
  
  if (write_s3) {
    wb <- openxlsx::createWorkbook()
    if (mode == "main") {
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
    aws.s3::put_object(tmp, bucket = STATS_BUCKET,
                       object = paste0("outputs/", full_name, ".xlsx"))
    cat("saved s3://", STATS_BUCKET, "/outputs/", full_name, ".xlsx\n", sep = "")
  }
  invisible(results)
}

stats_run_grid <- function(qbs = STATS_QBS, defs = STATS_DEFS, stat = "tds",
                           mode = "main", write_s3 = TRUE) {
  out <- list()
  for (q in qbs) for (d in defs) {
    cat("== ", q, " vs ", d, " (", stat, ", ", mode, ") ==\n", sep = "")
    out[[paste(q, d, sep = " vs ")]] <- stats_run(q, d, stat = stat, mode = mode,
                                                  write_s3 = write_s3)
  }
  invisible(out)
}

stats_push_all <- function(qbs = STATS_QBS, defs = STATS_DEFS,
                           stats = names(stat_families),
                           modes = c("main", "weather", "precip"), write_s3 = TRUE) {
  cat("tolerances: ", STATS_TOL_STAMP, "\n", sep = "")
  cat("pushing ", length(qbs) * length(defs) * length(stats) * length(modes),
      " workbooks to s3://", STATS_BUCKET, "/outputs/\n\n", sep = "")
  out <- list()
  for (s in stats) for (m in modes)
    out[[paste(s, m)]] <- stats_run_grid(qbs, defs, stat = s, mode = m, write_s3 = write_s3)
  invisible(out)
}

# ---------------------------------------------------------------------------
# usage -- nothing above this line fires anything
# ---------------------------------------------------------------------------
cat("
stats_comparison_engine loaded. Tolerances: ", STATS_TOL_STAMP, "

  stats_run_grid(stat = 'plays')                     six Plays workbooks
  stats_run_grid()                                   six TDs workbooks
  stats_run_grid(stat = 'xpass', mode = 'weather')   one family, one mode
  stats_push_all()                                   every family x every mode
  stats_push_all(stats = 'plays')                    one family, all three modes
  r <- stats_run('NEMaye-2025', 'SEA2025', stat = 'plays', write_s3 = FALSE); r$blitz

Stat families: ", paste(names(stat_families), collapse = ", "), ". Adding one is one
entry in stat_families: a label and a column set. Lenses, tolerances, and
comp pools are shared by every family.

Re-tune: source comparison_engine_thresholds.R again, then re-source this
file -- it rebuilds its tables from whatever the tuner just solved.
To make a tune the standard for fresh sessions, paste the tuner's printed
block over the banked lists in section 3 (the tuner prints them as
xtds_tol_qb / xtds_tol_def; here they are stats_tol_qb / stats_tol_def).
", sep = "")


# STAT FAMILIES -- key -> workbook stem -> data source
#   tds       "TDs"       qb_stats_df_final   per-lens column sets (blitz/depth/less/pa/pressure differ)
#   plays     "Plays"     qb_stats_df_final   plays, no_huddle + ranks
#   xpass     "XPass"     qb_stats_df_final   pass_rate, fastr/pbp/part xpass_rate + ranks
#   cp        "CP"        qb_stats_df_final   acc_rate, fastr/pbp/part cp + ranks
#   sack      "SackRt"    qb_stats_df_final   sack_rate x3, pressure_rate/pbp_pressure/part_pressure_before + ranks
#   scr       "ScrStats"  qb_stats_df_final   scr_rate, scr_ypc, pbp/part scr_xypc + ranks
#   twp       "TWP"       qb_stats_df_final   twp_rate, int_rate, pressure_rate + ranks
#   ypa       "YPA"       qb_stats_df_final   ypa, pbp/part xypa, adot, four depth-band rates + ranks
#   xtd_prop  "XTD_Prop"  xtd_proportion      pass/scramble/run proportions + ranks
#
# stats_run_grid(stat = "<key>") writes "<stem> - QB vs DEF.xlsx" for the six combos.
# Weather/precip modes prefix the stem: "Weather SackRt - ...", "Precip SackRt - ...".

stats_push_all(stat = 'twp')  
stats_run_grid(stat = 'xtd_prop')
