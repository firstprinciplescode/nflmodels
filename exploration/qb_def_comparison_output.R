# qb_def_comparison_engine.R  -- source this, nothing else.
# (renamed from qbwork_run.R; function names unchanged: qbwork, qbwork_all)
#
# ONE MODULE:   qbwork("pressure", "DEN2025", "def")
# ALL FIVE:     qbwork_all("DEN2025", "def")
# AND SAVE IT:  qbwork("pressure", "DEN2025", "def", save = TRUE)
#               -> writes den2025_pressure_def_qbwork.xlsx and still prints
#               dir = "out/" to set the folder, fmt = "csv" to force csv
#
# Reads sim_def$QB (def) or sim_qb$QB (off) from your session. Set those first, the
# way you always have, at the top of def_comparison_AWS.R / qb_comparison_AWS.R.

U <- 0.4; COLLAPSE_THR <- 0.14; COH_DEAD <- 0.26

# COLUMN LAYOUT. Pinned to the column order of QB Work.xlsx, per module, so a row
# of output pastes straight onto a row of the sheet with no shifting. Columns in
# your z-frame that have no slot in the sheet (e.g. depth's adot, behind/short BTT)
# are dropped from the output -- the console tells you which. A sheet slot with no
# matching column in the z-frame prints blank and keeps its place. If you ever
# rearrange a sheet, edit the matching vector here.
QBWORK_LAYOUT <- list(
  blitz = c(
    "blitz_rate", "blitz_pressure_rate", "no_blitz_pressure_rate", "blitz_grade",
    "no_blitz_grade", "blitz_time_to_throw", "no_blitz_time_to_throw", "blitz_ypa",
    "no_blitz_ypa", "blitz_acc_pct", "no_blitz_acc_pct", "blitz_qbr",
    "no_blitz_qbr", "blitz_btt_rate", "no_blitz_btt_rate", "blitz_twp_rate",
    "no_blitz_twp_rate", "blitz_adot", "no_blitz_adot", "blitz_scr_rate",
    "no_blitz_scr_rate", "blitz_sack_pct", "no_blitz_sack_pct", "grade_difference",
    "ttt_difference", "ypa_difference", "acc_pct_difference", "qbr_difference",
    "btt_difference", "twp_difference", "adot_difference", "scr_rate_difference",
    "pressure_rate_difference", "sack_rate_difference"),
  depth = c(
    "behind_los_rate", "short_rate", "medium_rate", "deep_rate",
    "behind_los_pressure_rate", "short_pressure_rate", "medium_pressure_rate", "deep_pressure_rate",
    "behind_los_grade", "short_grade", "medium_grade", "deep_grade",
    "behind_los_time_to_throw", "short_time_to_throw", "medium_time_to_throw", "deep_time_to_throw",
    "behind_los_ypa", "short_ypa", "medium_ypa", "deep_ypa",
    "behind_los_acc_pct", "short_acc_pct", "medium_acc_pct", "deep_acc_pct",
    "behind_los_qbr", "short_qbr", "medium_qbr", "deep_qbr",
    "medium_btt_rate", "deep_btt_rate", "short_twp_rate", "medium_twp_rate",
    "deep_twp_rate", "ds_grade_difference", "ds_acc_pct_difference", "ds_qbr_difference",
    "ds_twp_difference", "ms_grade_difference", "ttt_difference", "ypa_difference",
    "ms_acc_pct_difference", "ms_qbr_difference", "ms_twp_difference", "pressure_rate_difference"),
  less = c(
    "less_rate", "less_pressure_rate", "more_pressure_rate", "less_grade",
    "more_grade", "less_time_to_throw", "more_time_to_throw", "less_ypa",
    "more_ypa", "less_acc_pct", "more_acc_pct", "less_qbr",
    "more_qbr", "less_btt_rate", "more_btt_rate", "less_twp_rate",
    "more_twp_rate", "less_adot", "more_adot", "more_scr_rate",
    "less_sack_pct", "more_sack_pct", "grade_difference", "ttt_difference",
    "ypa_difference", "acc_pct_difference", "qbr_difference", "btt_difference",
    "twp_difference", "adot_difference", "scr_rate_difference", "pressure_rate_difference",
    "sack_rate_difference"),
  pa = c(
    "pa_rate", "pa_pressure_rate", "npa_pressure_rate", "pa_grade",
    "npa_grade", "pa_time_to_throw", "npa_time_to_throw", "pa_ypa",
    "npa_ypa", "pa_acc_pct", "npa_acc_pct", "pa_qbr",
    "npa_qbr", "pa_btt_rate", "npa_btt_rate", "pa_twp_rate",
    "npa_twp_rate", "pa_adot", "npa_adot", "pa_scr_rate",
    "npa_scr_rate", "pa_sack_pct", "npa_sack_pct", "grade_difference",
    "ttt_difference", "ypa_difference", "acc_pct_difference", "qbr_difference",
    "btt_difference", "twp_difference", "adot_difference", "scr_rate_difference",
    "pressure_rate_difference", "sack_rate_difference"),
  pressure = c(
    "pressure_rate", "pressure_grade", "no_pressure_grade", "pressure_time_to_throw",
    "no_pressure_time_to_throw", "pressure_ypa", "no_pressure_ypa", "pressure_acc_pct",
    "no_pressure_acc_pct", "pressure_qbr", "no_pressure_qbr", "pressure_btt_rate",
    "no_pressure_btt_rate", "pressure_twp_rate", "no_pressure_twp_rate", "pressure_adot",
    "no_pressure_adot", "pressure_scr_rate", "no_pressure_scr_rate", "sack_pct",
    "pressures_qb", "pressures_oth", "grade_difference", "ttt_difference",
    "ypa_difference", "acc_pct_difference", "qbr_difference", "btt_difference",
    "twp_difference", "adot_difference", "scr_rate_difference")
)

# APOSTROPHE. Any cell starting with + - = @ gets a leading ' written INTO the
# file -- xlsx included, not just csv. If your paste target re-parses text
# (Google Sheets, Paste Special as text, double-click paste), "- CC" would
# otherwise be read as a formula and blow up. The ' is eaten on that paste, so
# the cell lands as clean text. You WILL see the ' when you open the .xlsx --
# that is the protection working, do not delete it. APOS <- FALSE kills it
# everywhere (file and console) if you ever paste cell-to-cell inside Excel only.
APOS <- TRUE

.ap <- function(x, apos = APOS) {
  if (!apos) return(x)
  ifelse(nzchar(x) & substr(x, 1, 1) %in% c("+", "-", "=", "@"), paste0("'", x), x)
}

.band <- function(z) if (is.na(z)) 0L else as.integer(floor(abs(z)/U + 0.5) * sign(z))
.coh <- function(cc, ccm) {
  if (is.na(cc) && is.na(ccm)) return(NA_real_)
  if (is.na(cc)) return(ccm); if (is.na(ccm)) return(cc)
  if (cc * ccm > 0) (cc + ccm)/2 else if (abs(cc) <= abs(ccm)) cc else ccm
}
# tight = TRUE drops the space ("2+VS") -- used only when two tags share a cell
# ("2-VS, +CC"); a single-tag cell keeps its space ("2+ VS", "- CC").
.tag <- function(b, l, tight = FALSE) if (b == 0) "" else
  paste0(if (abs(b) == 1) "" else abs(b), if (b > 0) "+" else "-",
         if (tight) "" else " ", l)

.cell <- function(vs, cc, ccm) {
  co <- .coh(cc, ccm)
  if (!is.na(vs) && !is.na(co) && abs(vs - co) < COLLAPSE_THR)
    return(.tag(.band((vs + co)/2), "CC"))
  tot <- .band(vs)
  bc  <- if (is.na(co) || abs(co) < COH_DEAD) 0L else .band(co)
  bv  <- tot - bc
  if (bv == 0 && bc == 0) return("")
  if (bv == 0) return(.tag(bc, "CC"))
  if (bc == 0) return(.tag(bv, "VS"))
  paste0(.tag(bv, "VS", tight = TRUE), ", ", .tag(bc, "CC", tight = TRUE))
}

# Writes df to <dir>/<stem>.xlsx (writexl or openxlsx), else .csv fallback.
# Apostrophes are applied to every character column for BOTH formats.
.write_out <- function(df, stem, dir, fmt) {
  df[] <- lapply(df, function(col) if (is.character(col)) .ap(col) else col)
  if (fmt == "xlsx") {
    if (requireNamespace("writexl", quietly = TRUE)) {
      p <- file.path(dir, paste0(stem, ".xlsx")); writexl::write_xlsx(df, p); return(p)
    }
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      p <- file.path(dir, paste0(stem, ".xlsx")); openxlsx::write.xlsx(df, p); return(p)
    }
    message("writexl/openxlsx not installed -- falling back to .csv")
    fmt <- "csv"
  }
  p <- file.path(dir, paste0(stem, ".csv"))
  utils::write.csv(df, p, row.names = FALSE, na = "")
  p
}

# ---- ONE MODULE ----------------------------------------------------------------------
# module: "blitz" | "depth" | "less" | "pa" | "pressure"
# side:   "def" | "off"
qbwork <- function(module, focal, side = "def", cohort = NULL, save = FALSE, dir = ".",
                   fmt = c("xlsx", "csv")) {
  fmt <- match.arg(fmt)
  id_col <- if (side == "def") "def_ssn" else "qbgrp_ssn"
  if (is.null(cohort)) {
    src <- if (side == "def") "sim_def" else "sim_qb"
    if (!exists(src)) stop("`", src, "` not in session -- run your comparison_*_func calls first")
    cohort <- get(src)$QB
  }
  zname <- paste0("df_", module, if (side == "def") "_def", "_scaled_z")
  if (!exists(zname)) stop("`", zname, "` not in session")
  z <- get(zname)
  cohort <- setdiff(as.character(cohort), focal)
  d <- z[z[[id_col]] %in% c(cohort, focal), , drop = FALSE]
  is_f <- d[[id_col]] == focal
  if (sum(is_f) != 1) stop("focal `", focal, "` matched ", sum(is_f), " rows in ", zname)
  keep <- setdiff(names(d), id_col); keep <- keep[!grepl("snaps|int_rate", keep)]
  vars <- unique(sub("_(Good|Bad|diff)$", "", keep))
  lay <- QBWORK_LAYOUT[[module]]
  if (!is.null(lay)) {
    extra <- setdiff(vars, lay)
    if (length(extra)) cat("-- dropped ", length(extra), " col(s) with no slot in QB Work: ",
                           paste(extra, collapse = ", "), "\n", sep = "")
    vars <- lay
  }
  cat("#### ", toupper(module), if (side == "def") " DEF", " -- ", focal,
      "  (", length(cohort), " comps, ", length(vars), " cols) ####\n", sep = "")
  rowlab <- c(Good = "Good", Bad = "Bad", diff = "Diff")
  out <- NULL
  for (bk in c("Good", "Bad", "diff")) {
    cells <- vapply(vars, function(v) {
      col <- paste0(v, "_", bk); if (!col %in% keep) return("")
      x <- d[[col]]
      .cell(x[is_f], median(x[!is_f], na.rm = TRUE), mean(x[!is_f], na.rm = TRUE))
    }, character(1))
    cat(rowlab[[bk]], "\t", paste(.ap(cells), collapse = "\t"), "\n", sep = "")
    out <- rbind(out, cells)
  }
  rownames(out) <- unname(rowlab); colnames(out) <- vars
  
  if (!isFALSE(save)) {
    stem <- paste0(tolower(gsub("[^A-Za-z0-9]", "", focal)), "_", module, "_", side, "_qbwork")
    df <- data.frame(row = rownames(out), out, check.names = FALSE, stringsAsFactors = FALSE)
    p <- .write_out(df, stem, dir, fmt)
    cat("-> ", normalizePath(p, mustWork = FALSE), "\n", sep = "")
  }
  invisible(out)
}

# ---- ALL FIVE ------------------------------------------------------------------------
qbwork_all <- function(focal, side = "def", cohort = NULL, save = FALSE, dir = ".",
                       fmt = c("xlsx", "csv")) {
  fmt <- match.arg(fmt)
  res <- list()
  for (m in c("blitz", "depth", "less", "pa", "pressure")) {
    r <- try(qbwork(m, focal, side, cohort, save = save, dir = dir, fmt = fmt), silent = TRUE)
    if (inherits(r, "try-error")) {
      cat("#### ", toupper(m), " -- SKIPPED: ",
          conditionMessage(attr(r, "condition")), "\n", sep = "")
    } else res[[m]] <- r
    cat("\n")
  }
  # one tidy long file across all modules, for the record -- xlsx like the rest
  if (!isFALSE(save) && length(res)) {
    long <- do.call(rbind, lapply(names(res), function(m) {
      x <- res[[m]]
      data.frame(focal = focal, side = side, module = m,
                 row = rep(rownames(x), times = ncol(x)),
                 var = rep(colnames(x), each = nrow(x)),
                 cell = as.vector(x), stringsAsFactors = FALSE)
    }))
    stem <- paste0(tolower(gsub("[^A-Za-z0-9]", "", focal)), "_", side, "_qbwork_all")
    p <- .write_out(long, stem, dir, fmt)
    cat("-> ", normalizePath(p, mustWork = FALSE), "  (", nrow(long), " rows, long format)\n", sep = "")
  }
  invisible(res)
}


qbwork_all("DENNix-2025", "off", save = TRUE)